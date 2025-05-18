use std::{collections::HashMap, ops::Index};

use atrium_api::types::string::Nsid;

pub mod atrium;
pub use atrium_lex;
use atrium_lex::LexiconDoc;

mod error;

#[cfg(feature = "json")]
pub use error::DocCreateError;
pub use error::Error;
use serde::Deserialize;

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

#[cfg(feature = "json")]
pub trait FromJsonFile<E>:
    Sized + CanonicalizeDocument<Error = E> + for<'a> Deserialize<'a>
where
    E: std::fmt::Display,
{
    fn from_json_file(path: impl AsRef<std::path::Path>) -> Result<Self, DocCreateError<E>> {
        Ok(serde_json::from_reader(std::fs::File::open(
            path.as_ref(),
        )?)?)
    }
    fn from_json_file_canonicalized(
        path: impl AsRef<std::path::Path>,
    ) -> Result<Self, DocCreateError<E>> {
        let mut doc = Self::from_json_file(path)?;
        doc.canonicalize().map_err(DocCreateError::Canonicalize)?;
        Ok(doc)
    }
}

pub trait CanonicalizeDocument {
    type Error;

    fn canonicalize(&mut self) -> Result<(), Self::Error>;
}

pub trait Canonicalize {
    fn canonicalize(&mut self, nsid: &Nsid);
}

pub struct Context {
    documents: HashMap<Nsid, LexiconDoc>,
}

impl Context {
    pub fn get(&self, index: &Nsid) -> Option<&LexiconDoc> {
        self.documents.get(index)
    }
}

impl Index<&Nsid> for Context {
    type Output = LexiconDoc;

    fn index(&self, index: &Nsid) -> &Self::Output {
        &self.documents[index]
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
