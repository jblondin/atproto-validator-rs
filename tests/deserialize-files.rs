use atproto_validator::atrium_lex::LexiconDoc;

pub mod lexicons;
use lexicons::record_document_json;

#[test]
fn deserialize_lexicon_docs() {
    if let Err(e) = serde_json::from_value::<LexiconDoc>(record_document_json(
        &"xyz.statusphere.status".parse().expect("bad nsid"),
    )) {
        panic!("deserializtion failed: {e}");
    }
    if let Err(e) = serde_json::from_value::<LexiconDoc>(record_document_json(
        &"app.bsky.actor.profile".parse().expect("bad nsid"),
    )) {
        panic!("deserializtion failed: {e}");
    }
    //TODO: include when query handling is implemented
    // if let Err(e) = serde_json::from_value::<Document>(query_document_json("app.bsky.actor.getProfile"))
    // {
    //     panic!("deserializtion failed: {e}");
    // }
}
