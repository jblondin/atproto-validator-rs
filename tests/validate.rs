use atproto_validator::{Document, ValidateObject};
use atrium_api::types::string::Nsid;

pub mod lexicons;
#[cfg(feature = "json")]
use lexicons::object_document_reader;
use lexicons::record_document_json;

#[cfg(feature = "json")]
fn validate_object(def_file: Nsid, obj_file: &str) {
    use atproto_validator::Context;

    let doc = match serde_json::from_value::<Document>(record_document_json(&def_file)) {
        Ok(doc) => doc,
        Err(e) => panic!("lexicon deserializtion failed: {e}"),
    };
    let ctxt = Context::from_documents([doc]);

    let object: serde_json::Value = match serde_json::from_reader(object_document_reader(&obj_file))
    {
        Ok(obj) => obj,
        Err(e) => panic!("object deserialization failed: {e}"),
    };

    if let Err(e) = object.validate_object(&ctxt, &def_file) {
        panic!("validation failed: {e:?}");
    }
}

#[test]
#[cfg(feature = "json")]
fn validate_objects() {
    // status
    validate_object(
        "xyz.statusphere.status".parse().expect("bad nsid"),
        "status",
    );
    // profile
    validate_object(
        "app.bsky.actor.profile".parse().expect("bad nsid"),
        "profile",
    );
}
