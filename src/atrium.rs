//! Validation routines for `atrium-lex` types.

mod canonicalize;
pub mod string_format;
pub mod validate;

use std::collections::HashMap;

use atrium_api::types::string::Nsid;
use atrium_lex::LexiconDoc;

use crate::{CanonicalizeDocument, Context, FromJsonFile};

impl FromJsonFile<&'static str> for LexiconDoc {}

impl Context {
    pub fn from_documents(
        docs: impl IntoIterator<Item = LexiconDoc>,
    ) -> Result<Context, &'static str> {
        let mut documents = HashMap::new();
        for mut doc in docs.into_iter() {
            doc.canonicalize()?;
            documents.insert(Nsid::new(doc.id.clone())?, doc);
        }
        Ok(Context { documents })
    }
}
