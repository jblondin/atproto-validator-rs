use std::{io::Read, path::PathBuf};

use atrium_api::types::string::Nsid;

pub fn lexicon_document_root() -> PathBuf {
    PathBuf::from(std::env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("lexicons")
}

pub fn object_document_root() -> PathBuf {
    PathBuf::from(std::env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("objects")
}

pub fn query_document_path(id: &Nsid) -> PathBuf {
    lexicon_document_root()
        .join("query")
        .join(format!("{id}.json", id = id.as_str()))
}

pub fn record_document_path(id: &Nsid) -> PathBuf {
    lexicon_document_root()
        .join("record")
        .join(format!("{id}.json", id = id.as_str()))
}

pub fn object_document_path(id: &str) -> PathBuf {
    object_document_root().join(format!("{id}.json", id = id))
}

pub fn object_document_reader(id: &str) -> impl Read {
    let path = object_document_path(id);
    std::fs::File::open(path.as_path())
        .expect(format!("unable to open JSON file at {}", path.display()).as_str())
}

pub fn record_document_reader(id: &Nsid) -> impl Read {
    let path = record_document_path(id);
    std::fs::File::open(path.as_path())
        .expect(format!("unable to open JSON file at {}", path.display()).as_str())
}

pub fn record_document_json(id: &Nsid) -> serde_json::Value {
    serde_json::from_reader(record_document_reader(id)).expect("JSON file invalid at {path}")
}

pub fn query_document_reader(id: &Nsid) -> impl Read {
    let path = query_document_path(id);
    std::fs::File::open(path.as_path())
        .expect(format!("unable to open JSON file at {}", path.display()).as_str())
}

pub fn query_file_json(id: &Nsid) -> serde_json::Value {
    serde_json::from_reader(query_document_reader(id)).expect("JSON file invalid at {path}")
}
