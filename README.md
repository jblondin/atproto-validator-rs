# ATProto Validator for Rust

This library provides validation routines for validating ATProto objects against their respective
lexicon documents, similar to what is provided by the Bluesky-provided javascript libraries to 
validate objects.

## Progress

This library is still very much a work in progress and only has basic functionally (and severely
lacking tests).

- [x] Deserialization of record-based ATProto lexicon documents 
- [ ] Deserialization of query / procedure / subscription ATProto lexicon documents
- [x] Validation of JSON ATProto record objects
- [ ] Validation of JSON ATproto query / procedure / subscription documents
- [x] Hooks for non-JSON (basic Rust struct) vaidation
- [ ] Full comparison against javascript ATProto libraries to ensure identifical validation results
- [ ] Finish full documentation and provide additional examples
- [ ] Package

### Testing

- [ ] Unit tests for individual type (integer, boolean, etc.) and string format validation
- [x] Basic deserialization integration test of record-based ATProto lexicon documents
- [ ] Full suite of integration tests for record-based ATProto lexicon documents
- [x] Basic validation integration test for JSON record-based ATProto objects 
- [ ] Full suite of validation integration tests for JSON record-based ATProto objects
- [ ] Deserialization integeration tests of JSON non-record-based ATProto lexicon documents
- [ ] Validation integration tests of JSON non-record-based ATProto lexicon documents
- [x] Example integration test of non-JSON (basic Rust struct) validation approach
- [ ] Additional non-JSON validation integration tests

## Features

Includes the following feature flags:

- "`json`": Enables JSON validation routines. Unneeded if your library is only using non-JSON 
validation. Enabled by default.
