#![cfg(feature = "json")]

use atproto_validator::{
    Context, Error, FromJsonFile, ValidateObject,
    atrium::string_format::{AtUriError, FormatError},
    atrium_lex::LexiconDoc,
};
use atrium_api::types::string::Nsid;
use lexicons::{object_document_reader, record_document_path};

pub mod lexicons;

fn nsid(s: &'static str) -> Nsid {
    s.parse().expect("bad nsid")
}

fn nsids() -> Vec<Nsid> {
    [
        "xyz.statusphere.status",
        "app.bsky.actor.profile",
        "com.atproto.label.defs",
        "com.atproto.repo.strongRef",
    ]
    .into_iter()
    .map(nsid)
    .collect()
}

fn build_context<'a>(nsids: impl IntoIterator<Item = &'a Nsid>) -> Context {
    Context::from_documents(nsids.into_iter().map(|nsid| {
        LexiconDoc::from_json_file(record_document_path(nsid)).expect("unable to load json file")
    }))
    .expect("failed to build context")
}

fn validate_object(ctxt: &Context, nsid: &Nsid, obj_file: &str) -> Result<(), Vec<Error>> {
    let object: serde_json::Value = match serde_json::from_reader(object_document_reader(&obj_file))
    {
        Ok(obj) => obj,
        Err(e) => panic!("object deserialization failed: {e}"),
    };
    object.validate_object(ctxt, nsid)
}

fn validate_object_success(ctxt: &Context, nsid: &Nsid, obj_file: &str) {
    if let Err(errs) = validate_object(ctxt, nsid, obj_file) {
        panic!(
            "validation failed for {obj_file}: [{e}]",
            e = errs
                .iter()
                .map(|e| format!("'{}'", e.to_string()))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
}

macro_rules! validate_object_failure {
    ($ctxt:expr, $nsid:expr, $obj:expr, $p:pat) => {
        match validate_object($ctxt, $nsid, $obj) {
            Ok(()) => panic!("validation didn't fail like expected"),
            Err(errs) => match errs[0] {
                $p => {}
                _ => panic!("unexpected error(s) {errs:?}"),
            },
        }
    };
}

#[test]
fn validate_objects() {
    let nsids = nsids();
    let ctxt = build_context(&nsids);
    // status
    validate_object_success(&ctxt, &nsid("xyz.statusphere.status"), "status");
    validate_object_failure!(
        &ctxt,
        &nsid("xyz.statusphere.status"),
        "status-bad",
        Error::NumGraphemeshOutOfBounds(2, None, Some(1))
    );

    // profile
    validate_object_success(&ctxt, &nsid("app.bsky.actor.profile"), "profile");
    validate_object_success(
        &ctxt,
        &nsid("app.bsky.actor.profile"),
        "profile-with-pinned",
    );
    validate_object_failure!(
        &ctxt,
        &nsid("app.bsky.actor.profile"),
        "profile-with-bad-pinned",
        Error::StringFormat(FormatError::AtUri(AtUriError::MissingSection))
    );
}
