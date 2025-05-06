use thiserror::Error;

use crate::string_format::FormatError;

#[derive(Debug, Error)]
pub enum Error {
    #[error("expected object")]
    ExpectedObject,
    #[error("missing nsid in context: {0}")]
    MissingNsid(String),
    #[error("invalid reference: {0}")]
    InvalidRef(String),
    #[error("non-canonical ref: {0}")]
    NonCanonicalRef(String),
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

#[derive(Debug, Error)]
#[cfg(feature = "json")]
pub enum DocCreateError {
    #[error("failed to open file: {0}")]
    Io(#[from] std::io::Error),
    #[error("failed to deserialize: {0}")]
    Deserialize(#[from] serde_json::Error),
}
