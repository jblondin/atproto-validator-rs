use std::collections::HashMap;

use atrium_api::types::string::Nsid;
use atrium_lex::lexicon::{
    LexArray, LexArrayItem, LexBlob, LexBoolean, LexBytes, LexCidLink, LexInteger, LexObject,
    LexObjectProperty, LexPrimitiveArray, LexPrimitiveArrayItem, LexRecord, LexRecordRecord,
    LexRef, LexRefUnion, LexString, LexToken, LexUnknown, LexUserType, LexXrpcBody,
    LexXrpcBodySchema, LexXrpcParameters, LexXrpcParametersProperty, LexXrpcProcedure,
    LexXrpcProcedureParameter, LexXrpcQuery, LexXrpcQueryParameter, LexXrpcSubscription,
    LexXrpcSubscriptionMessage, LexXrpcSubscriptionMessageSchema, LexXrpcSubscriptionParameter,
};
use unicode_segmentation::UnicodeSegmentation;

use crate::{Context, DocumentType, Error, Validate, ValidateObject};

impl<T> ValidateObject for T
where
    T: for<'a> Validate<DocumentType<'a>> + Validate<LexUserType>,
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

//HACK: workaround for a trait bound recursion issue
pub trait ValidateRef<T>: Validate<T> {
    fn validate_ref(&self, def: &T, ctxt: &Context, errs: &mut Vec<Error>) {
        self.validate(def, ctxt, errs);
    }
}

#[cfg(feature = "json")]
impl ValidateRef<LexUserType> for serde_json::Value {}

impl<T> Validate<LexUserType> for T
where
    T: Validate<LexRecord>
        + Validate<LexXrpcQuery>
        + Validate<LexXrpcProcedure>
        + Validate<LexXrpcSubscription>
        + Validate<LexBlob>
        + Validate<LexArray>
        + Validate<LexToken>
        + Validate<LexObject>
        + Validate<LexBoolean>
        + Validate<LexInteger>
        + Validate<LexString>
        + Validate<LexBytes>
        + Validate<LexCidLink>,
{
    fn validate(&self, def: &LexUserType, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexUserType::Record(lex_record) => self.validate(lex_record, ctxt, errs),
            LexUserType::XrpcQuery(lex_xrpc_query) => self.validate(lex_xrpc_query, ctxt, errs),
            LexUserType::XrpcProcedure(lex_xrpc_procedure) => {
                self.validate(lex_xrpc_procedure, ctxt, errs)
            }
            LexUserType::XrpcSubscription(lex_xrpc_subscription) => {
                self.validate(lex_xrpc_subscription, ctxt, errs)
            }
            LexUserType::Blob(lex_blob) => self.validate(lex_blob, ctxt, errs),
            LexUserType::Array(lex_array) => self.validate(lex_array, ctxt, errs),
            LexUserType::Token(lex_token) => self.validate(lex_token, ctxt, errs),
            LexUserType::Object(lex_object) => self.validate(lex_object, ctxt, errs),
            LexUserType::Boolean(lex_boolean) => self.validate(lex_boolean, ctxt, errs),
            LexUserType::Integer(lex_integer) => self.validate(lex_integer, ctxt, errs),
            LexUserType::String(lex_string) => self.validate(lex_string, ctxt, errs),
            LexUserType::Bytes(lex_bytes) => self.validate(lex_bytes, ctxt, errs),
            LexUserType::CidLink(lex_cid_link) => self.validate(lex_cid_link, ctxt, errs),
            LexUserType::Unknown(_lex_unknown) => {}
        }
    }
}

impl<T> Validate<LexRecord> for T
where
    T: Validate<LexRecordRecord>,
{
    fn validate(&self, def: &LexRecord, ctxt: &Context, errs: &mut Vec<Error>) {
        self.validate(&def.record, ctxt, errs)
    }
}

impl<T> Validate<LexRecordRecord> for T
where
    T: Validate<LexObject>,
{
    fn validate(&self, def: &LexRecordRecord, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexRecordRecord::Object(lex_object) => self.validate(lex_object, ctxt, errs),
        }
    }
}

impl<T> Validate<LexXrpcQuery> for T
where
    T: Validate<LexXrpcQueryParameter> + Validate<LexXrpcBody>,
{
    fn validate(&self, def: &LexXrpcQuery, ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(param) = &def.parameters {
            self.validate(param, ctxt, errs);
        }
        if let Some(output) = &def.output {
            self.validate(output, ctxt, errs);
        }
    }
}

impl<T> Validate<LexXrpcQueryParameter> for T
where
    T: Validate<LexXrpcParameters>,
{
    fn validate(&self, def: &LexXrpcQueryParameter, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexXrpcQueryParameter::Params(lex_xrpc_parameters) => {
                self.validate(lex_xrpc_parameters, ctxt, errs)
            }
        }
    }
}

impl<T> Validate<LexXrpcParameters> for T
where
    T: Validate<LexXrpcParametersProperty>,
{
    fn validate(&self, def: &LexXrpcParameters, ctxt: &Context, errs: &mut Vec<Error>) {
        let mut found = match &def.required {
            Some(required) => HashMap::from_iter(required.iter().map(|key| (key, false))),
            None => HashMap::new(),
        };
        for (key, param) in &def.properties {
            if let Some(prop_found) = found.get_mut(key) {
                *prop_found = true;
            }
            self.validate(param, ctxt, errs);
        }
        for (missing_required, _) in found.into_iter().filter(|(_, prop_found)| !prop_found) {
            errs.push(Error::MissingRequired(missing_required.clone()));
        }
    }
}

impl<T> Validate<LexXrpcParametersProperty> for T
where
    T: Validate<LexBoolean>
        + Validate<LexInteger>
        + Validate<LexString>
        + Validate<LexUnknown>
        + Validate<LexPrimitiveArray>,
{
    fn validate(&self, def: &LexXrpcParametersProperty, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexXrpcParametersProperty::Boolean(lex_boolean) => {
                self.validate(lex_boolean, ctxt, errs)
            }
            LexXrpcParametersProperty::Integer(lex_integer) => {
                self.validate(lex_integer, ctxt, errs)
            }
            LexXrpcParametersProperty::String(lex_string) => self.validate(lex_string, ctxt, errs),
            LexXrpcParametersProperty::Unknown(lex_unknown) => {
                self.validate(lex_unknown, ctxt, errs)
            }
            LexXrpcParametersProperty::Array(lex_primitive_array) => {
                self.validate(lex_primitive_array, ctxt, errs)
            }
        }
    }
}

impl<T> Validate<LexXrpcBody> for T
where
    T: Validate<LexXrpcBodySchema>,
{
    fn validate(&self, def: &LexXrpcBody, ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(schema) = &def.schema {
            self.validate(schema, ctxt, errs);
        }
    }
}

impl<T> Validate<LexXrpcBodySchema> for T
where
    T: Validate<LexRef> + Validate<LexRefUnion> + Validate<LexObject>,
{
    fn validate(&self, def: &LexXrpcBodySchema, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexXrpcBodySchema::Ref(lex_ref) => self.validate(lex_ref, ctxt, errs),
            LexXrpcBodySchema::Union(lex_ref_union) => self.validate(lex_ref_union, ctxt, errs),
            LexXrpcBodySchema::Object(lex_object) => self.validate(lex_object, ctxt, errs),
        }
    }
}

impl<T> Validate<LexXrpcProcedure> for T
where
    T: Validate<LexXrpcProcedureParameter> + Validate<LexXrpcBody>,
{
    fn validate(&self, def: &LexXrpcProcedure, ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(param) = &def.parameters {
            self.validate(param, ctxt, errs);
        }
        if let Some(output) = &def.output {
            self.validate(output, ctxt, errs);
        }
    }
}

impl<T> Validate<LexXrpcProcedureParameter> for T
where
    T: Validate<LexXrpcParameters>,
{
    fn validate(&self, def: &LexXrpcProcedureParameter, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexXrpcProcedureParameter::Params(lex_xrpc_parameters) => {
                self.validate(lex_xrpc_parameters, ctxt, errs)
            }
        }
    }
}

impl<T> Validate<LexXrpcSubscription> for T
where
    T: Validate<LexXrpcSubscriptionParameter> + Validate<LexXrpcSubscriptionMessage>,
{
    fn validate(&self, def: &LexXrpcSubscription, ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(param) = &def.parameters {
            self.validate(param, ctxt, errs);
        }
        if let Some(output) = &def.message {
            self.validate(output, ctxt, errs);
        }
    }
}

impl<T> Validate<LexXrpcSubscriptionParameter> for T
where
    T: Validate<LexXrpcParameters>,
{
    fn validate(&self, def: &LexXrpcSubscriptionParameter, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexXrpcSubscriptionParameter::Params(lex_xrpc_parameters) => {
                self.validate(lex_xrpc_parameters, ctxt, errs)
            }
        }
    }
}

impl<T> Validate<LexXrpcSubscriptionMessage> for T
where
    T: Validate<LexXrpcSubscriptionMessageSchema>,
{
    fn validate(&self, def: &LexXrpcSubscriptionMessage, ctxt: &Context, errs: &mut Vec<Error>) {
        if let Some(schema) = &def.schema {
            self.validate(schema, ctxt, errs);
        }
    }
}

impl<T> Validate<LexXrpcSubscriptionMessageSchema> for T
where
    T: Validate<LexRef> + Validate<LexRefUnion> + Validate<LexObject>,
{
    fn validate(
        &self,
        def: &LexXrpcSubscriptionMessageSchema,
        ctxt: &Context,
        errs: &mut Vec<Error>,
    ) {
        match def {
            LexXrpcSubscriptionMessageSchema::Ref(lex_ref) => self.validate(lex_ref, ctxt, errs),
            LexXrpcSubscriptionMessageSchema::Union(lex_ref_union) => {
                self.validate(lex_ref_union, ctxt, errs)
            }
            LexXrpcSubscriptionMessageSchema::Object(lex_object) => {
                self.validate(lex_object, ctxt, errs)
            }
        }
    }
}

impl<T> Validate<LexToken> for T {
    fn validate(&self, _def: &LexToken, _ctxt: &Context, _errs: &mut Vec<Error>) {}
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
impl Validate<LexObject> for serde_json::Map<String, serde_json::Value> {
    fn validate(&self, def: &LexObject, ctxt: &Context, errs: &mut Vec<Error>) {
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
impl Validate<LexObject> for serde_json::Value {
    fn validate(&self, def: &LexObject, ctxt: &Context, errs: &mut Vec<Error>) {
        let serde_json::Value::Object(obj) = self else {
            errs.push(Error::ExpectedObject);
            return;
        };

        obj.validate(def, ctxt, errs)
    }
}

pub trait Requires {
    fn requires(&self, key: impl AsRef<str>) -> bool;
}

impl Requires for LexObject {
    fn requires(&self, key: impl AsRef<str>) -> bool {
        self.required
            .as_ref()
            .map(|keys| keys.iter().any(|reqd| key.as_ref() == reqd))
            .unwrap_or(false)
    }
}

pub trait ValidateProp<PropType> {
    /// Helper function to retrieve a specific property by name. Returns the associated
    /// object field definition ($def_type) if successful. In case of failure, records
    /// errors in `errs` Vec and returns `None`.
    fn get_prop(&self, name: impl AsRef<str>, errs: &mut Vec<Error>) -> Option<&PropType>;
    fn validate_prop<'a, T>(
        &self,
        name: impl AsRef<str>,
        object: impl Into<Option<&'a T>>,
        ctxt: &Context,
        errs: &mut Vec<Error>,
    ) where
        Self: Requires,
        T: 'a + Validate<PropType>,
    {
        let name = name.as_ref();
        if let Some(field_def) = self.get_prop(name, errs) {
            match object.into() {
                Some(object) => {
                    object.validate(field_def, ctxt, errs);
                }
                None if self.requires(name) => {
                    errs.push(Error::MissingField(name.to_owned()));
                }
                None => {}
            }
        }
    }
}

macro_rules! impl_val_prop {
    ($def_type:ty, $field_def_var:path) => {
        impl ValidateProp<$def_type> for LexObject {
            fn get_prop(&self, name: impl AsRef<str>, errs: &mut Vec<Error>) -> Option<&$def_type> {
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
        }
    };
}

impl_val_prop!(LexString, LexObjectProperty::String);
impl_val_prop!(LexBoolean, LexObjectProperty::Boolean);
impl_val_prop!(LexInteger, LexObjectProperty::Integer);
impl_val_prop!(LexBytes, LexObjectProperty::Bytes);
impl_val_prop!(LexCidLink, LexObjectProperty::CidLink);
impl_val_prop!(LexArray, LexObjectProperty::Array);
impl_val_prop!(LexBlob, LexObjectProperty::Blob);
impl_val_prop!(LexUnknown, LexObjectProperty::Unknown);
impl_val_prop!(LexRef, LexObjectProperty::Ref);
impl_val_prop!(LexRefUnion, LexObjectProperty::Union);

impl<T> Validate<LexObjectProperty> for T
where
    T: Validate<LexRef>
        + Validate<LexRefUnion>
        + Validate<LexBytes>
        + Validate<LexCidLink>
        + Validate<LexArray>
        + Validate<LexBlob>
        + Validate<LexBoolean>
        + Validate<LexInteger>
        + Validate<LexString>,
{
    fn validate(&self, def: &LexObjectProperty, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexObjectProperty::Ref(lex_ref) => self.validate(lex_ref, ctxt, errs),
            LexObjectProperty::Union(lex_ref_union) => self.validate(lex_ref_union, ctxt, errs),
            LexObjectProperty::Bytes(lex_bytes) => self.validate(lex_bytes, ctxt, errs),
            LexObjectProperty::CidLink(lex_cid_link) => self.validate(lex_cid_link, ctxt, errs),
            LexObjectProperty::Array(lex_array) => self.validate(lex_array, ctxt, errs),
            LexObjectProperty::Blob(lex_blob) => self.validate(lex_blob, ctxt, errs),
            LexObjectProperty::Boolean(lex_boolean) => self.validate(lex_boolean, ctxt, errs),
            LexObjectProperty::Integer(lex_integer) => self.validate(lex_integer, ctxt, errs),
            LexObjectProperty::String(lex_string) => self.validate(lex_string, ctxt, errs),
            LexObjectProperty::Unknown(_lex_unknown) => self.validate(_lex_unknown, ctxt, errs),
        }
    }
}

impl Validate<LexBoolean> for bool {
    fn validate(&self, def: &LexBoolean, _ctxt: &Context, errs: &mut Vec<Error>) {
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
impl Validate<LexBoolean> for serde_json::Value {
    fn validate(&self, def: &LexBoolean, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Bool(b) => b.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

impl Validate<LexInteger> for i64 {
    fn validate(&self, def: &LexInteger, _ctxt: &Context, errs: &mut Vec<Error>) {
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
impl Validate<LexInteger> for serde_json::Number {
    fn validate(&self, def: &LexInteger, ctxt: &Context, errs: &mut Vec<Error>) {
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
impl Validate<LexInteger> for serde_json::Value {
    fn validate(&self, def: &LexInteger, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Number(num) => num.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

impl<T> Validate<LexUnknown> for T {
    fn validate(&self, _def: &LexUnknown, _ctxt: &Context, _errs: &mut Vec<Error>) {
        // no validation
    }
}

impl Validate<LexString> for &str {
    fn validate(&self, def: &LexString, ctxt: &Context, errs: &mut Vec<Error>) {
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
        let num_graphemes = self.graphemes(true).count();
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

impl Validate<LexString> for String {
    fn validate(&self, def: &LexString, ctxt: &Context, errs: &mut Vec<Error>) {
        self.as_str().validate(def, ctxt, errs);
    }
}

impl Validate<LexString> for atrium_api::types::string::Datetime {
    fn validate(&self, def: &LexString, ctxt: &Context, errs: &mut Vec<Error>) {
        self.as_str().validate(def, ctxt, errs)
    }
}

#[cfg(feature = "json")]
impl Validate<LexString> for serde_json::Value {
    fn validate(&self, def: &LexString, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::String(s) => s.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<LexBytes> for serde_json::Map<String, serde_json::Value> {
    fn validate(&self, def: &LexBytes, _ctxt: &Context, errs: &mut Vec<Error>) {
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
impl Validate<LexBytes> for serde_json::Value {
    fn validate(&self, def: &LexBytes, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Object(obj) => obj.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<LexCidLink> for serde_json::Map<String, serde_json::Value> {
    fn validate(&self, _def: &LexCidLink, _ctxt: &Context, errs: &mut Vec<Error>) {
        match self.get("$link") {
            Some(_bytes_value) => {}
            None => errs.push(Error::MissingField("$bytes".to_owned())),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<LexCidLink> for serde_json::Value {
    fn validate(&self, def: &LexCidLink, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Object(obj) => obj.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

impl<T> Validate<LexPrimitiveArray> for Vec<T>
where
    T: Validate<LexPrimitiveArrayItem>,
{
    fn validate(&self, def: &LexPrimitiveArray, ctxt: &Context, errs: &mut Vec<Error>) {
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

impl<T> Validate<LexPrimitiveArrayItem> for T
where
    T: Validate<LexBoolean> + Validate<LexInteger> + Validate<LexString> + Validate<LexUnknown>,
{
    fn validate(&self, def: &LexPrimitiveArrayItem, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexPrimitiveArrayItem::Boolean(lex_boolean) => self.validate(lex_boolean, ctxt, errs),
            LexPrimitiveArrayItem::Integer(lex_integer) => self.validate(lex_integer, ctxt, errs),
            LexPrimitiveArrayItem::String(lex_string) => self.validate(lex_string, ctxt, errs),
            LexPrimitiveArrayItem::Unknown(lex_unknown) => self.validate(lex_unknown, ctxt, errs),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<LexPrimitiveArray> for serde_json::Value
where
    Vec<serde_json::Value>: Validate<LexPrimitiveArray>,
{
    fn validate(&self, def: &LexPrimitiveArray, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Array(arr) => arr.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

impl<T> Validate<LexArray> for Vec<T>
where
    T: Validate<LexArrayItem>,
{
    fn validate(&self, def: &LexArray, ctxt: &Context, errs: &mut Vec<Error>) {
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

impl<T> Validate<LexArrayItem> for T
where
    T: Validate<LexBoolean>
        + Validate<LexInteger>
        + Validate<LexString>
        + Validate<LexUnknown>
        + Validate<LexBytes>
        + Validate<LexCidLink>
        + Validate<LexBlob>
        + Validate<LexRef>
        + Validate<LexRefUnion>,
{
    fn validate(&self, def: &LexArrayItem, ctxt: &Context, errs: &mut Vec<Error>) {
        match def {
            LexArrayItem::Boolean(lex_boolean) => self.validate(lex_boolean, ctxt, errs),
            LexArrayItem::Integer(lex_integer) => self.validate(lex_integer, ctxt, errs),
            LexArrayItem::String(lex_string) => self.validate(lex_string, ctxt, errs),
            LexArrayItem::Unknown(lex_unknown) => self.validate(lex_unknown, ctxt, errs),
            LexArrayItem::Bytes(lex_bytes) => self.validate(lex_bytes, ctxt, errs),
            LexArrayItem::CidLink(lex_cid_link) => self.validate(lex_cid_link, ctxt, errs),
            LexArrayItem::Blob(lex_blob) => self.validate(lex_blob, ctxt, errs),
            LexArrayItem::Ref(lex_ref) => self.validate(lex_ref, ctxt, errs),
            LexArrayItem::Union(lex_ref_union) => self.validate(lex_ref_union, ctxt, errs),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<LexArray> for serde_json::Value
where
    Vec<serde_json::Value>: Validate<LexArray>,
{
    fn validate(&self, def: &LexArray, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Array(arr) => arr.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
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

impl Validate<LexBlob> for atrium_api::types::Blob {
    fn validate(&self, def: &LexBlob, _ctxt: &Context, errs: &mut Vec<Error>) {
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

impl Validate<LexBlob> for atrium_api::types::TypedBlobRef {
    fn validate(&self, def: &LexBlob, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            atrium_api::types::TypedBlobRef::Blob(blob) => blob.validate(def, ctxt, errs),
        }
    }
}

impl Validate<LexBlob> for atrium_api::types::BlobRef {
    fn validate(&self, def: &LexBlob, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            atrium_api::types::BlobRef::Typed(typed) => typed.validate(def, ctxt, errs),
            _ => errs.push(Error::MissingField("$type".to_owned())),
        }
    }
}

#[cfg(feature = "json")]
impl Validate<LexBlob> for serde_json::Map<String, serde_json::Value> {
    fn validate(&self, def: &LexBlob, _ctxt: &Context, errs: &mut Vec<Error>) {
        match self.get("$type") {
            Some(type_value) => match type_value {
                serde_json::Value::String(type_value) => {
                    if type_value != "blob" {
                        errs.push(Error::TypeMismatch {
                            expected: "blob".to_owned(),
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
impl Validate<LexBlob> for serde_json::Value {
    fn validate(&self, def: &LexBlob, ctxt: &Context, errs: &mut Vec<Error>) {
        match self {
            serde_json::Value::Object(obj) => obj.validate(def, ctxt, errs),
            _ => errs.push(Error::FieldTypeMismatch),
        }
    }
}

fn ref_parts(input: &str) -> (Option<String>, String) {
    let mut split = input.split("#");
    let nsid_part = Some(
        split
            .next()
            .expect("split always has at least one element")
            .to_owned(),
    )
    .filter(|part| !part.is_empty());
    let fragment_part = split
        .next()
        .map(str::to_owned)
        .unwrap_or_else(|| "main".to_owned());
    (nsid_part, fragment_part)
}

impl<T> Validate<LexRef> for T
where
    T: ValidateRef<LexUserType>,
{
    fn validate(&self, def: &LexRef, ctxt: &Context, errs: &mut Vec<Error>) {
        let (nsid_part, fragment_part) = ref_parts(&def.r#ref.as_str());
        let Some(nsid_str) = nsid_part else {
            errs.push(Error::NonCanonicalRef(def.r#ref.to_string()));
            return;
        };
        let nsid = match Nsid::new(nsid_str) {
            Ok(nsid) => nsid,
            Err(e) => {
                errs.push(Error::NsidParse(e));
                return;
            }
        };
        let Some(doc) = ctxt.get(&nsid) else {
            errs.push(Error::MissingRef(def.r#ref.to_string()));
            return;
        };
        let Some(lex_user_type) = doc.defs.get(fragment_part.as_str()) else {
            errs.push(Error::MissingRef(def.r#ref.to_string()));
            return;
        };
        self.validate_ref(lex_user_type, ctxt, errs);
    }
}

impl<T> Validate<LexRefUnion> for T
where
    T: for<'a> Validate<DocumentType<'a>> + ValidateRef<LexUserType>,
{
    fn validate(&self, def: &LexRefUnion, ctxt: &Context, errs: &mut Vec<Error>) {
        let mut found = false;
        for r#ref in &def.refs {
            let (nsid_part, fragment_part) = ref_parts(r#ref.as_str());
            let Some(nsid_str) = nsid_part else {
                // hard error even in a union
                errs.push(Error::NonCanonicalRef(r#ref.to_string()));
                return;
            };
            let nsid = match Nsid::new(nsid_str) {
                Ok(nsid) => nsid,
                Err(e) => {
                    // hard error even in a union
                    errs.push(Error::NsidParse(e));
                    return;
                }
            };
            let Some(doc) = ctxt.get(&nsid) else {
                continue;
            };
            let Some(lex_user_type) = doc.defs.get(fragment_part.as_str()) else {
                continue;
            };
            let mut potential_errs = vec![];
            self.validate(&DocumentType(doc.id.as_str()), ctxt, &mut potential_errs);
            if potential_errs.is_empty() {
                // we found the ref in the union that matches this object
                found = true;
                self.validate_ref(lex_user_type, ctxt, errs);
                break;
            }
        }
        if !found && def.closed.unwrap_or(false) {
            errs.push(Error::UnfoundRefInUnion);
        }
    }
}
