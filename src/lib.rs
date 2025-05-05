use std::{collections::HashMap, ops::Index};

use atrium_api::types::string::Nsid;

use serde::{Deserialize, de};
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

pub mod string_format;
use string_format::{FormatError, StringFormat};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Document {
    #[allow(dead_code)]
    pub lexicon: u32,
    pub id: Nsid,
    #[allow(dead_code)]
    pub description: Option<String>,
    pub defs: HashMap<String, TopLevelDef>,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("expected object")]
    ExpectedObject,
    #[error("missing nsid in context: {0}")]
    MissingNsid(String),
    #[error("invalid reference: {0}")]
    InvalidRef(String),
    #[error("reference not found in context: {0}")]
    MissingRef(String),
    #[error("nsid parsing error: {0}")]
    NsidParse(&'static str),
    #[error("no reference in union matched schema document in context")]
    UnfoundRefInUnion,
    #[error("missing field '{0}'")]
    MissingField(String),
    #[error("invalid type for field '{0}'")]
    InvalidField(String),
    #[error("$type mismatch: expected {expected}, found {actual}")]
    TypeMismatch { expected: String, actual: String },
    #[error("no main definition found in lexicon document")]
    MissingMain,
    #[error("missing required property: {0}")]
    MissingRequired(String),
    #[error("invalid null property: {0}")]
    InvalidNull(String),
    #[error("unexpected field in data object: {0}")]
    UnexpectedField(String),
    #[error("invalid floating-point number")]
    InvalidFloat,
    #[error("integer exceeds capacity")]
    ExceedsCapacity,
    #[error("integer {0} out of bounds min_value={1:?}, max_value={2:?}")]
    IntOutOfBounds(i64, Option<i64>, Option<i64>),
    #[error("blob size {0} out of bounds min_size={1:?}, max_size={2:?}")]
    SizeOutOfBounds(u64, Option<u64>, Option<u64>),
    #[error("string length {0} outside of bounds min_length={1:?}, max_length={2:?}")]
    StrLenOutOfBounds(usize, Option<u32>, Option<u32>),
    #[error("number of graphemes {0} outside of bounds min_graphemes={1:?}, max_graphemes={2:?}")]
    NumGraphemeshOutOfBounds(u32, Option<u32>, Option<u32>),
    #[error("array length {0} outside of bounds min_size={1:?}, max_size={2:?}")]
    ArrLenOutOfBounds(usize, Option<usize>, Option<usize>),
    #[error("value not in enumeration")]
    NotInEnumeration,
    #[error("field type mismatch")]
    FieldTypeMismatch,
    #[error("invalid mime type: {0}")]
    InvalidMimeType(String),
    #[error("string format: {0}")]
    StringFormat(#[from] FormatError),
    #[error("{expected} should be constant {actual}")]
    ConstMismatch { expected: String, actual: String },
    #[error("{0}")]
    Custom(Box<dyn std::error::Error + Send + Sync>),
}

pub struct Context {
    documents: HashMap<Nsid, Document>,
}

impl Context {
    pub fn from_documents(docs: impl IntoIterator<Item = Document>) -> Context {
        Context {
            documents: HashMap::from_iter(docs.into_iter().map(|doc| (doc.id.clone(), doc))),
        }
    }
}

impl Context {
    fn get(&self, index: &Nsid) -> Option<&Document> {
        self.documents.get(index)
    }
}

impl Index<&Nsid> for Context {
    type Output = Document;

    fn index(&self, index: &Nsid) -> &Self::Output {
        &self.documents[index]
    }
}

/// Trait that providees validation against an ATProto lexicon document or subcomponent.
///
/// This trait can be implemented for structs you want to validate against a lexicon document.
/// Typically, to do so you must implement both `Validate<DocumentType<'a>>` and
/// `Validate<ObjectDef>` to provide `$type` validation and struct-level validation, respectively.
pub trait Validate<T> {
    /// Validate against a document or subcomponent definition. Takes and mutats a running list of
    /// errors encountered during validation, to be implemented by users wanting to provide
    /// validation for arbitrary objects.
    ///
    /// See also `[ValidateObject::validate_object]`, which is the normal entry point for calling
    /// for validation of an obect against a given lexicon document.
    fn validate(&self, def: &T, ctxt: &Context, errs: &mut Vec<Error>);
}

/// Trait to provide top-level validation of an ATProto object against a lexicon document. This
/// typically doesn't need to be implemented, as there is a blanket implementation for structs that
/// implement `Validate<DocumentType>` (which should be implemented instead).
pub trait ValidateObject {
    /// Validates the object against the lexicon schema document stored in `context` with key
    /// `nsid`.
    fn validate_object(&self, ctxt: &Context, nsid: &Nsid) -> Result<(), Vec<Error>>;
}

impl<T> ValidateObject for T
where
    T: for<'a> Validate<DocumentType<'a>> + Validate<TopLevelDef>,
{
    fn validate_object(&self, ctxt: &Context, nsid: &Nsid) -> Result<(), Vec<Error>> {
        let doc = ctxt
            .get(nsid)
            .ok_or_else(|| vec![Error::MissingNsid(nsid.as_str().to_owned())])?;
        let mut errs = vec![];
        self.validate(&DocumentType(doc.id.as_str()), ctxt, &mut errs);
        for (def_key, primary_def) in doc.defs.iter() {
            if def_key == "main" {
                self.validate(primary_def, ctxt, &mut errs);
                if errs.is_empty() {
                    return Ok(());
                } else {
                    return Err(errs);
                }
            }
        }
        errs.push(Error::MissingMain);
        Err(errs)
    }
}

pub struct DocumentType<'a>(&'a str);

impl<'a> AsRef<str> for DocumentType<'a> {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl<'a> DocumentType<'a> {
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }
}

#[cfg(feature = "json")]
impl<'a> Validate<DocumentType<'a>> for serde_json::Value {
    fn validate(&self, doc_type: &DocumentType<'a>, _ctxt: &Context, errs: &mut Vec<Error>) {
        let serde_json::Value::Object(map) = self else {
            errs.push(Error::ExpectedObject);
            return;
        };

        match map.get("$type") {
            Some(serde_json::Value::String(object_type)) => {
                if object_type != doc_type.as_str() {
                    errs.push(Error::TypeMismatch {
                        expected: doc_type.as_str().to_owned(),
                        actual: object_type.clone(),
                    });
                }
            }
            Some(_) => errs.push(Error::InvalidField("$type".to_owned())),
            None => errs.push(Error::MissingField("$type".to_owned())),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<Document> for serde_json::Value {
    fn validate(&self, doc: &Document, _ctxt: &Context, errs: &mut Vec<Error>) {
        let serde_json::Value::Object(map) = self else {
            errs.push(Error::ExpectedObject);
            return;
        };

        match map.get("$type") {
            Some(serde_json::Value::String(object_type)) => {
                if object_type != doc.id.as_str() {
                    errs.push(Error::TypeMismatch {
                        expected: doc.id.as_str().to_owned(),
                        actual: object_type.clone(),
                    });
                }
            }
            Some(_) => errs.push(Error::InvalidField("$type".to_owned())),
            None => errs.push(Error::MissingField("$type".to_owned())),
        }
    }
}

//TODO: implement query / procedure / subscription validation
#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum TopLevelDef {
    Record(RecordDef),
    Boolean(BooleanDef),
    Integer(IntegerDef),
    String(StringDef),
    Bytes(BytesDef),
    CidLink(CidLinkDef),
    Array(ArrayDef<Box<Self>>),
    Object(ObjectDef),
    Blob(BlobDef),
}

impl TopLevelDef {
    pub fn type_name(&self) -> &'static str {
        match self {
            TopLevelDef::Record(_) => "record",
            TopLevelDef::Boolean(_) => "boolean",
            TopLevelDef::Integer(_) => "integer",
            TopLevelDef::String(_) => "string",
            TopLevelDef::Bytes(_) => "bytes",
            TopLevelDef::CidLink(_) => "cid_link",
            TopLevelDef::Array(_) => "array",
            TopLevelDef::Object(_) => "object",
            TopLevelDef::Blob(_) => "blob",
        }
    }
}

#[cfg(feature = "json")]
impl Validate<TopLevelDef> for serde_json::Value {
    fn validate(&self, def: &TopLevelDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            TopLevelDef::Record(record) => self.validate(record, ctxt, errs),
            TopLevelDef::Boolean(boolean_def) => self.validate(boolean_def, ctxt, errs),
            TopLevelDef::Integer(integer_def) => self.validate(integer_def, ctxt, errs),
            TopLevelDef::String(string_def) => self.validate(string_def, ctxt, errs),
            TopLevelDef::Bytes(bytes_def) => self.validate(bytes_def, ctxt, errs),
            TopLevelDef::CidLink(cid_link_def) => self.validate(cid_link_def, ctxt, errs),
            TopLevelDef::Array(array_def) => self.validate(array_def, ctxt, errs),
            TopLevelDef::Object(object_def) => self.validate(object_def, ctxt, errs),
            TopLevelDef::Blob(blob_def) => self.validate(blob_def, ctxt, errs),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RecordDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    #[allow(dead_code)]
    pub key: String,
    pub record: ObjectDef,
}

impl<T> Validate<RecordDef> for T
where
    T: Validate<ObjectDef>,
{
    fn validate(&self, def: &RecordDef, ctxt: &Context, errs: &mut Vec<Error>) {
        self.validate(&def.record, ctxt, errs)
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ObjectDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    pub properties: HashMap<String, ObjectFieldDef>,
    pub required: Option<Vec<String>>,
    pub nullable: Option<Vec<String>>,
}

impl ObjectDef {
    pub fn requires(&self, key: impl AsRef<str>) -> bool {
        self.required
            .as_ref()
            .map(|keys| keys.iter().any(|reqd| key.as_ref() == reqd))
            .unwrap_or(false)
    }
}

fn find_invalid_nulls(null_props: Vec<&String>, mut nullable: Vec<&String>, errs: &mut Vec<Error>) {
    // could probably sort both and two a stepwise linear search if this becomes a performance issue
    nullable.sort();
    for null_prop in null_props {
        if nullable.binary_search(&null_prop).is_err() {
            errs.push(Error::InvalidNull(null_prop.clone()));
        }
    }
}

#[cfg(feature = "json")]
impl Validate<ObjectDef> for serde_json::Map<String, serde_json::Value> {
    fn validate(&self, def: &ObjectDef, ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(required) = &def.required {
            for req_prop in required {
                if self.get(req_prop).is_none() {
                    errs.push(Error::MissingRequired(req_prop.clone()));
                };
            }
        }

        let null_props = self
            .iter()
            .filter_map(|(key, value)| match value {
                serde_json::Value::Null => Some(key),
                _ => None,
            })
            .collect::<Vec<_>>();

        match &def.nullable {
            Some(nullable) => {
                find_invalid_nulls(null_props, nullable.iter().collect(), errs);
            }
            None => {
                find_invalid_nulls(null_props, vec![], errs);
            }
        }

        for (field_name, field) in self {
            match def.properties.get(field_name) {
                Some(object_field) => {
                    field.validate(object_field, ctxt, errs);
                }
                None if field_name == "$type" => {
                    // ignore, $type is allowed in some objects
                }
                None => {
                    errs.push(Error::UnexpectedField(field_name.clone()));
                }
            }
        }
    }
}

#[cfg(feature = "json")]
impl Validate<ObjectDef> for serde_json::Value {
    fn validate(&self, def: &ObjectDef, ctxt: &Context, errs: &mut Vec<Error>) {
        let serde_json::Value::Object(obj) = self else {
            errs.push(Error::ExpectedObject);
            return;
        };

        obj.validate(def, ctxt, errs)
    }
}

macro_rules! impl_prop_get_val {
    ($get_fn_name:ident, $val_fn_name:ident, $def_type:ty, $field_def_var:path) => {
        impl ObjectDef {
            /// Helper function to retrieve a specific property by name. Returns the associated
            /// object field definition ($def_type) if successful. In case of failure, records
            /// errors in `errs` Vec and returns `None`.
            pub fn $get_fn_name(
                &self,
                name: impl AsRef<str>,
                errs: &mut Vec<$crate::Error>,
            ) -> Option<&$def_type> {
                let name = name.as_ref();
                match self.properties.get(name) {
                    Some($field_def_var(def)) => {
                        return Some(def);
                    }
                    Some(_) => {
                        errs.push($crate::Error::FieldTypeMismatch);
                    }
                    None => {
                        errs.push($crate::Error::UnexpectedField(name.to_owned()));
                    }
                }
                None
            }
            pub fn $val_fn_name<'a, T>(
                &self,
                name: impl AsRef<str>,
                object: impl Into<Option<&'a T>>,
                ctxt: &$crate::Context,
                errs: &mut Vec<$crate::Error>,
            ) where
                T: 'a + Validate<$def_type>,
            {
                let name = name.as_ref();
                if let Some(field_def) = self.$get_fn_name(name, errs) {
                    match object.into() {
                        Some(object) => {
                            object.validate(field_def, ctxt, errs);
                        }
                        None if self.requires(name) => {
                            errs.push($crate::Error::MissingField(name.to_owned()));
                        }
                        None => {}
                    }
                }
            }
        }
    };
}

impl_prop_get_val!(
    get_string_prop,
    validate_string_prop,
    StringDef,
    ObjectFieldDef::String
);
impl_prop_get_val!(
    get_bool_prop,
    validate_bool_prop,
    BooleanDef,
    ObjectFieldDef::Boolean
);
impl_prop_get_val!(
    get_int_prop,
    validate_int_prop,
    IntegerDef,
    ObjectFieldDef::Integer
);
impl_prop_get_val!(
    get_bytes_prop,
    validate_bytes_prop,
    BytesDef,
    ObjectFieldDef::Bytes
);
impl_prop_get_val!(
    get_cid_link_prop,
    validate_cid_link_prop,
    CidLinkDef,
    ObjectFieldDef::CidLink
);
impl_prop_get_val!(
    get_array_prop,
    validate_array_prop,
    ArrayDef<Box<ObjectFieldDef>>,
    ObjectFieldDef::Array
);
impl_prop_get_val!(
    get_object_prop,
    validate_object_prop,
    ObjectDef,
    ObjectFieldDef::Object
);
impl_prop_get_val!(
    get_blob_prop,
    validate_blob_prop,
    BlobDef,
    ObjectFieldDef::Blob
);
impl_prop_get_val!(
    get_unknown_prop,
    validate_unknown_prop,
    UnknownDef,
    ObjectFieldDef::Unknown
);

#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum ObjectFieldDef {
    Null,
    Boolean(BooleanDef),
    Integer(IntegerDef),
    String(StringDef),
    Bytes(BytesDef),
    CidLink(CidLinkDef),
    Array(ArrayDef<Box<Self>>),
    Object(ObjectDef),
    Blob(BlobDef),
    Ref(RefDef),
    Union(UnionDef),
    Unknown(UnknownDef),
}

#[cfg(feature = "json")]
impl Validate<ObjectFieldDef> for serde_json::Value {
    fn validate(&self, def: &ObjectFieldDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            ObjectFieldDef::Null => {}
            ObjectFieldDef::Boolean(bool_def) => self.validate(bool_def, ctxt, errs),
            ObjectFieldDef::Integer(integer_def) => self.validate(integer_def, ctxt, errs),
            ObjectFieldDef::String(string_def) => self.validate(string_def, ctxt, errs),
            ObjectFieldDef::Bytes(bytes_def) => self.validate(bytes_def, ctxt, errs),
            ObjectFieldDef::CidLink(cid_link_def) => self.validate(cid_link_def, ctxt, errs),
            ObjectFieldDef::Array(array_def) => self.validate(array_def, ctxt, errs),
            ObjectFieldDef::Object(object_def) => self.validate(object_def, ctxt, errs),
            ObjectFieldDef::Blob(blob_def) => self.validate(blob_def, ctxt, errs),
            ObjectFieldDef::Ref(ref_def) => self.validate(ref_def, ctxt, errs),
            ObjectFieldDef::Union(union_def) => self.validate(union_def, ctxt, errs),
            ObjectFieldDef::Unknown(_) => {}
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BooleanDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    #[allow(dead_code)]
    pub default: Option<bool>,
    pub r#const: Option<bool>,
}

impl Validate<BooleanDef> for bool {
    fn validate(&self, def: &BooleanDef, _ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(bool_const) = def.r#const {
            if bool_const != *self {
                errs.push(Error::ConstMismatch {
                    expected: bool_const.to_string(),
                    actual: self.to_string(),
                });
            }
        }
    }
}

#[cfg(feature = "json")]
impl Validate<BooleanDef> for serde_json::Value {
    fn validate(&self, def: &BooleanDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Bool(b) => b.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IntegerDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    pub minimum: Option<i64>,
    pub maximum: Option<i64>,
    pub r#enum: Option<Vec<i64>>,
    #[allow(dead_code)]
    pub default: Option<i64>,
    pub r#const: Option<i64>,
}

impl Validate<IntegerDef> for i64 {
    fn validate(&self, def: &IntegerDef, _ctxt: &Context, errs: &mut Vec<Error>) {
        if def.minimum.filter(|min| self < min).is_some()
            || def.maximum.filter(|max| self < max).is_some()
        {
            errs.push(Error::IntOutOfBounds(*self, def.minimum, def.maximum))
        }
        if let Some(enumerated) = &def.r#enum {
            if !enumerated.contains(self) {
                errs.push(Error::NotInEnumeration);
            }
        }
        if let Some(int_const) = def.r#const {
            if int_const != *self {
                errs.push(Error::ConstMismatch {
                    expected: int_const.to_string(),
                    actual: self.to_string(),
                })
            }
        }
    }
}

#[cfg(feature = "json")]
impl Validate<IntegerDef> for serde_json::Number {
    fn validate(&self, def: &IntegerDef, ctxt: &Context, errs: &mut Vec<Error>) {
        if self.is_f64() {
            errs.push(Error::InvalidFloat);
        }
        match self.as_i64() {
            Some(num) => num.validate(def, ctxt, errs),
            None => {
                // this should only happen for values represented as u64s that are outside of the
                // i64 value range
                errs.push(Error::ExceedsCapacity);
            }
        }
    }
}

#[cfg(feature = "json")]
impl Validate<IntegerDef> for serde_json::Value {
    fn validate(&self, def: &IntegerDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Number(num) => num.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StringDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    pub format: Option<StringFormat>,
    pub min_length: Option<u32>,
    pub max_length: Option<u32>,
    pub min_graphemes: Option<u32>,
    pub max_graphemes: Option<u32>,
    #[allow(dead_code)]
    pub known_values: Option<Vec<String>>,
    pub r#enum: Option<Vec<String>>,
    #[allow(dead_code)]
    pub default: Option<String>,
    pub r#const: Option<String>,
}

impl Validate<StringDef> for &str {
    fn validate(&self, def: &StringDef, ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(min_length) = def.min_length {
            if self.len() < min_length as usize {
                errs.push(Error::StrLenOutOfBounds(
                    self.len(),
                    def.min_length,
                    def.max_length,
                ));
            }
        }
        if let Some(max_length) = def.max_length {
            if self.len() > max_length as usize {
                errs.push(Error::StrLenOutOfBounds(
                    self.len(),
                    def.min_length,
                    def.max_length,
                ));
            }
        }
        let num_graphemes = self.graphemes(true).count() as u32;
        if let Some(min_graphemes) = def.min_graphemes {
            if num_graphemes < min_graphemes {
                errs.push(Error::NumGraphemeshOutOfBounds(
                    num_graphemes,
                    def.min_graphemes,
                    def.max_graphemes,
                ));
            }
        }
        if let Some(max_graphemes) = def.max_graphemes {
            if num_graphemes > max_graphemes {
                errs.push(Error::NumGraphemeshOutOfBounds(
                    num_graphemes,
                    def.min_graphemes,
                    def.max_graphemes,
                ));
            }
        }
        if let Some(r#enum) = &def.r#enum {
            if !r#enum.iter().any(|enum_str| enum_str == self) {
                errs.push(Error::NotInEnumeration);
            }
        }
        if let Some(string_const) = &def.r#const {
            if string_const != self {
                errs.push(Error::ConstMismatch {
                    expected: string_const.to_string(),
                    actual: self.to_string(),
                })
            }
        }

        if let Some(format) = &def.format {
            self.validate(format, ctxt, errs);
        }
    }
}

impl Validate<StringDef> for String {
    fn validate(&self, def: &StringDef, ctxt: &Context, errs: &mut Vec<Error>) {
        self.as_str().validate(def, ctxt, errs);
    }
}

impl Validate<StringDef> for atrium_api::types::string::Datetime {
    fn validate(&self, def: &StringDef, ctxt: &Context, errs: &mut Vec<Error>) {
        self.as_str().validate(def, ctxt, errs)
    }
}

#[cfg(feature = "json")]
impl Validate<StringDef> for serde_json::Value {
    fn validate(&self, def: &StringDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::String(s) => s.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BytesDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    pub min_length: Option<usize>,
    pub max_length: Option<usize>,
}

#[cfg(feature = "json")]
impl Validate<BytesDef> for serde_json::Map<String, serde_json::Value> {
    fn validate(&self, def: &BytesDef, _ctxt: &Context, errs: &mut Vec<Error>) {
        match self.get("$bytes") {
            Some(bytes_value) => match bytes_value {
                serde_json::Value::String(bytes_value) => {
                    if let Some(min_length) = def.min_length {
                        if bytes_value.len() < min_length as usize {
                            errs.push(Error::ArrLenOutOfBounds(
                                bytes_value.len(),
                                def.min_length,
                                def.max_length,
                            ));
                        }
                    }
                    if let Some(max_length) = def.max_length {
                        if bytes_value.len() > max_length as usize {
                            errs.push(Error::ArrLenOutOfBounds(
                                bytes_value.len(),
                                def.min_length,
                                def.max_length,
                            ));
                        }
                    }
                }
                _ => {
                    errs.push(Error::FieldTypeMismatch);
                }
            },
            None => errs.push(Error::MissingField("$bytes".to_owned())),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<BytesDef> for serde_json::Value {
    fn validate(&self, def: &BytesDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Object(obj) => obj.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CidLinkDef {
    #[allow(dead_code)]
    pub description: Option<String>,
}

#[cfg(feature = "json")]
impl Validate<CidLinkDef> for serde_json::Map<String, serde_json::Value> {
    fn validate(&self, _def: &CidLinkDef, _ctxt: &Context, errs: &mut Vec<Error>) {
        match self.get("$link") {
            Some(_bytes_value) => {}
            None => errs.push(Error::MissingField("$bytes".to_owned())),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<CidLinkDef> for serde_json::Value {
    fn validate(&self, def: &CidLinkDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Object(obj) => obj.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ArrayDef<Fields> {
    #[allow(dead_code)]
    pub description: Option<String>,
    pub items: Fields,
    pub min_length: Option<usize>,
    pub max_length: Option<usize>,
}

#[cfg(feature = "json")]
impl<Fields> Validate<ArrayDef<Box<Fields>>> for Vec<serde_json::Value>
where
    serde_json::Value: Validate<Fields>,
{
    fn validate(&self, def: &ArrayDef<Box<Fields>>, ctxt: &Context, errs: &mut Vec<Error>) {
        if def.min_length.filter(|&min| self.len() < min).is_some()
            || def.max_length.filter(|&max| self.len() > max).is_some()
        {
            errs.push(Error::ArrLenOutOfBounds(
                self.len(),
                def.min_length,
                def.max_length,
            ));
        }
        for value in self {
            value.validate(&def.items, ctxt, errs);
        }
    }
}

#[cfg(feature = "json")]
impl<Fields> Validate<ArrayDef<Box<Fields>>> for serde_json::Value
where
    serde_json::Value: Validate<Fields>,
{
    fn validate(&self, def: &ArrayDef<Box<Fields>>, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Array(arr) => arr.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BlobDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    pub accept: Option<Vec<String>>,
    pub max_size: Option<usize>,
}

fn is_accepted_mime_type(mime_type: &String, accepted_mime_types: &Vec<String>) -> bool {
    for accepted_mime_type in accepted_mime_types {
        if accepted_mime_type == "*/*" {
            return true;
        }
        if accepted_mime_type == mime_type {
            return true;
        }
        if accepted_mime_type.ends_with("*") {
            let index_of_asterisk = accepted_mime_type
                .find("*")
                .expect("unexpected missing asterisk");
            if mime_type.len() > index_of_asterisk
                && accepted_mime_type[0..index_of_asterisk] == mime_type[0..index_of_asterisk]
            {
                return true;
            }
        }
    }
    false
}

impl Validate<BlobDef> for atrium_api::types::Blob {
    fn validate(&self, def: &BlobDef, _ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(accept) = &def.accept {
            if !accept.contains(&self.mime_type) {
                errs.push(Error::InvalidMimeType(self.mime_type.clone()));
            }
        }
        if let Some(max_size) = def.max_size {
            if self.size > max_size {
                errs.push(Error::SizeOutOfBounds(
                    self.size as u64,
                    None,
                    Some(max_size as u64),
                ))
            }
        }
    }
}

impl Validate<BlobDef> for atrium_api::types::TypedBlobRef {
    fn validate(&self, def: &BlobDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            atrium_api::types::TypedBlobRef::Blob(blob) => blob.validate(def, ctxt, errs),
        }
    }
}

impl Validate<BlobDef> for atrium_api::types::BlobRef {
    fn validate(&self, def: &BlobDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            atrium_api::types::BlobRef::Typed(typed) => typed.validate(def, ctxt, errs),
            _ => errs.push(Error::MissingField("$type".to_owned())),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<BlobDef> for serde_json::Map<String, serde_json::Value> {
    fn validate(&self, def: &BlobDef, _ctxt: &Context, errs: &mut Vec<Error>) {
        match self.get("$type") {
            Some(type_value) => match type_value {
                serde_json::Value::String(type_value) => {
                    if type_value != "blob" {
                        errs.push(Error::TypeMismatch {
                            expected: "type".to_owned(),
                            actual: type_value.clone(),
                        })
                    }
                }
                _ => {
                    errs.push(Error::FieldTypeMismatch);
                }
            },
            None => errs.push(Error::MissingField("$type".to_owned())),
        }

        match self.get("mimeType") {
            Some(mime_type) => match mime_type {
                serde_json::Value::String(mime_type) => {
                    if let Some(accepted_mime_types) = &def.accept {
                        if !is_accepted_mime_type(mime_type, accepted_mime_types) {
                            errs.push(Error::InvalidMimeType(mime_type.to_owned()))
                        }
                    }
                }
                _ => errs.push(Error::FieldTypeMismatch),
            },
            None => errs.push(Error::MissingField("mimeType".to_owned())),
        }

        match self.get("size") {
            Some(size) => match size {
                serde_json::Value::Number(size) => {
                    if size.is_f64() {
                        errs.push(Error::InvalidFloat);
                    } else {
                        match size.as_u64() {
                            Some(size) => {
                                if let Some(max_size) = def.max_size {
                                    if size > max_size as u64 {
                                        errs.push(Error::SizeOutOfBounds(
                                            size,
                                            Some(0),
                                            Some(max_size as u64),
                                        ))
                                    }
                                }
                            }
                            None => {
                                // this should only happen if size is somehow negative
                                errs.push(Error::ExceedsCapacity);
                            }
                        }
                    }
                }
                _ => {
                    errs.push(Error::FieldTypeMismatch);
                }
            },
            None => errs.push(Error::MissingField("size".to_owned())),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<BlobDef> for serde_json::Value {
    fn validate(&self, def: &BlobDef, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Object(obj) => obj.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RefDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    #[serde(deserialize_with = "deserialize_ref")]
    pub r#ref: Ref,
}

#[derive(Debug)]
pub struct Ref {
    nsid: Nsid,
    fragment: Option<String>,
}

impl Ref {
    pub fn fragment(&self) -> &str {
        match &self.fragment {
            Some(fragment) => fragment.as_str(),
            None => "main",
        }
    }
}

impl std::str::FromStr for Ref {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split("#");
        let nsid_part = split.next().expect("split always has at least one element");
        let fragment_part = split.next();
        if split.next().is_some() {
            // too many '#'
            return Err(Error::InvalidRef(s.to_owned()));
        }
        Ok(Ref {
            nsid: nsid_part.parse().map_err(Error::NsidParse)?,
            fragment: fragment_part.map(|s| s.to_owned()),
        })
    }
}

fn deserialize_ref<'de, D>(deserializer: D) -> Result<Ref, D::Error>
where
    D: de::Deserializer<'de>,
{
    let s: &str = de::Deserialize::deserialize(deserializer)?;
    s.parse().map_err(de::Error::custom)
}

impl std::fmt::Display for Ref {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.fragment {
            Some(fragment) => write!(f, "{}#{}", self.nsid.as_str(), fragment),
            None => write!(f, "{}", self.nsid.as_str()),
        }
    }
}

impl<T> Validate<RefDef> for T
where
    T: Validate<TopLevelDef>,
{
    fn validate(&self, def: &RefDef, ctxt: &Context, errs: &mut Vec<Error>) {
        let Some(doc) = ctxt.get(&def.r#ref.nsid) else {
            errs.push(Error::MissingRef(def.r#ref.to_string()));
            return;
        };
        let Some(tld) = doc.defs.get(def.r#ref.fragment()) else {
            errs.push(Error::MissingRef(def.r#ref.to_string()));
            return;
        };
        self.validate(tld, ctxt, errs);
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UnionDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    #[serde(deserialize_with = "deserialize_refs")]
    pub refs: Vec<Ref>,
    #[serde(default)] // defaults to false
    pub closed: bool,
}

fn deserialize_refs<'de, D>(deserializer: D) -> Result<Vec<Ref>, D::Error>
where
    D: de::Deserializer<'de>,
{
    let mut inputs = Vec::<&str>::deserialize(deserializer)?;
    let refs = inputs
        .drain(..)
        .map(|elem| Ok(elem.parse().map_err(de::Error::custom)?))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(refs)
}

impl<T> Validate<UnionDef> for T
where
    T: for<'a> Validate<DocumentType<'a>> + Validate<TopLevelDef>,
{
    fn validate(&self, def: &UnionDef, ctxt: &Context, errs: &mut Vec<Error>) {
        let mut found = false;
        for r#ref in &def.refs {
            let Some(doc) = ctxt.get(&r#ref.nsid) else {
                continue;
            };
            let Some(tld) = doc.defs.get(r#ref.fragment()) else {
                continue;
            };
            let mut potential_errs = vec![];
            self.validate(&DocumentType(doc.id.as_str()), ctxt, &mut potential_errs);
            if potential_errs.is_empty() {
                // we found the ref in the union that matches this object
                found = true;
                self.validate(tld, ctxt, errs);
                break;
            }
        }
        if !found && def.closed {
            errs.push(Error::UnfoundRefInUnion);
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UnknownDef {
    #[allow(dead_code)]
    pub description: Option<String>,
    #[allow(dead_code)]
    #[serde(flatten)]
    pub inner: ObjectDef,
}

//TODO: these are to be used with eventual query validation
#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum ParamsFieldDef {
    Boolean(BooleanDef),
    Integer(IntegerDef),
    String(StringDef),
    Array(ArrayDef<Box<Self>>),
    Unknown(UnknownDef),
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
#[allow(dead_code)]
pub struct ParamsDef {
    pub description: Option<String>,
    pub properties: HashMap<String, ParamsFieldDef>,
    pub required: Option<Vec<String>>,
}
