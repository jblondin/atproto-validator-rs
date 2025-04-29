use atproto_validator::Document;

pub mod lexicons;
use lexicons::record_document_json;

#[test]
fn deserialize_lexicon_docs() {
    if let Err(e) =
        serde_json::from_value::<Document>(record_document_json("xyz.statusphere.status"))
    {
        panic!("deserializtion failed: {e}");
    }
    if let Err(e) =
        serde_json::from_value::<Document>(record_document_json("app.bsky.actor.profile"))
    {
        panic!("deserializtion failed: {e}");
    }
    //TODO: include when query handling is implemented
    // if let Err(e) = serde_json::from_value::<Document>(query_document_json("app.bsky.actor.getProfile"))
    // {
    //     panic!("deserializtion failed: {e}");
    // }
}
