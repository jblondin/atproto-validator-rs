use std::{str::FromStr, sync::LazyLock};

use atrium_api::types::string::Cid;
use atrium_lex::lexicon::LexStringFormat;
use chrono::DateTime;
use regex::Regex;
use serde::Deserialize;

use crate::{Context, Error, Validate};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum StringFormat {
    AtIdentifier,
    AtUri,
    Cid,
    Datetime,
    Did,
    Handle,
    Nsid,
    Tid,
    RecordKey,
    Uri,
    Language,
}

impl<S> Validate<LexStringFormat> for S
where
    S: AsRef<str>,
{
    fn validate(&self, format: &LexStringFormat, _ctxt: &Context, errs: &mut Vec<Error>) {
        let s = self.as_ref();
        match format {
            LexStringFormat::AtIdentifier => {
                if s.starts_with("did:") {
                    if let Err(e) = validate_did(s) {
                        errs.push(FormatError::Did(e).into());
                    }
                } else {
                    if let Err(e) = validate_handle(s) {
                        errs.push(FormatError::Handle(e).into());
                    }
                }
            }
            LexStringFormat::AtUri => {
                if let Err(e) = validate_at_uri(s) {
                    errs.push(FormatError::AtUri(e).into());
                }
            }
            LexStringFormat::Cid => {
                if let Err(e) = Cid::from_str(s) {
                    errs.push(FormatError::Cid(e).into());
                }
            }
            LexStringFormat::Datetime => {
                // chrono::DateTime's `parse_from_rfc3339` only parses those datetimes in the
                // overlap between RFC3339 and ISO8601, so this checks both.
                // technically I think we should also check for valid WHATWG HTML dates, but the
                // javascript library only seems to check RFC 3339 and ISO 8601, so we'll do the
                // same
                if let Err(e) = DateTime::parse_from_rfc3339(s) {
                    errs.push(FormatError::DateTime(e).into());
                }
            }
            LexStringFormat::Did => {
                if let Err(e) = validate_did(s) {
                    errs.push(FormatError::Did(e).into());
                }
            }
            LexStringFormat::Handle => {
                if let Err(e) = validate_handle(s) {
                    errs.push(FormatError::Handle(e).into());
                }
            }
            LexStringFormat::Nsid => {
                if let Err(e) = validate_nsid(s) {
                    errs.push(FormatError::Nsid(e).into())
                }
            }
            LexStringFormat::Tid => {
                if let Err(e) = validate_tid(s) {
                    errs.push(FormatError::Tid(e).into());
                }
            }
            LexStringFormat::RecordKey => {
                if let Err(e) = validate_record_key(s) {
                    errs.push(FormatError::RecordKey(e).into());
                }
            }
            LexStringFormat::Uri => {
                if let Err(e) = validate_uri(s) {
                    errs.push(FormatError::Uri(e).into());
                }
            }
            LexStringFormat::Language => {
                if let Err(e) = validate_language(s) {
                    errs.push(FormatError::Language(e).into());
                }
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FormatError {
    #[error("did: {0}")]
    Did(DidError),
    #[error("handle: {0}")]
    Handle(HandleError),
    #[error("at-uri: {0}")]
    AtUri(AtUriError),
    #[error("cid: {0}")]
    Cid(ipld_core::cid::Error),
    #[error("datetime: failed to parse as both ISO8601 and RFC3339: {0}")]
    DateTime(chrono::ParseError),
    #[error("nsid: {0}")]
    Nsid(NsidError),
    #[error("tid: {0}")]
    Tid(TidError),
    #[error("record key: {0}")]
    RecordKey(RecordKeyError),
    #[error("uri: {0}")]
    Uri(UriError),
    #[error("language: {0}")]
    Language(LanguageError),
}

#[derive(Debug, thiserror::Error)]
pub enum DidError {
    #[error("missing 'did:' prefix")]
    MissingDidPrefix,
    #[error(
        "disallowed characters in DID (ASCII letters, numbers, period, underscore, colon, percent sign and hyphen only"
    )]
    InvalidChars,
    #[error("DID requires prefix ('did'), method, and method contents separated by ':'")]
    InvalidFormat,
    #[error("DID method must be lower-case letters")]
    InvalidMethod,
    #[error("DID can not end with ':' or '%'")]
    InvalidEnd,
    #[error("DID too long (2048 chars max)")]
    TooLong,
}

pub fn validate_did(s: impl AsRef<str>) -> Result<(), DidError> {
    let s = s.as_ref();
    if !s.starts_with("did:") {
        return Err(DidError::MissingDidPrefix);
    }
    static RE_DID_CHARS: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^[a-zA-Z0-9._:%-]*$").expect("regex failed to compile"));
    if !RE_DID_CHARS.is_match(s) {
        return Err(DidError::InvalidChars);
    }
    let mut parts = s.split(":");
    if parts.clone().count() < 3 {
        return Err(DidError::InvalidFormat);
    }
    let _ = parts.next().expect("already checks size");
    let method = parts.next().expect("already checked size");
    if !method.chars().all(|c| c.is_ascii_lowercase()) {
        return Err(DidError::InvalidMethod);
    }
    if s.ends_with("%") || s.ends_with(":") {
        return Err(DidError::InvalidEnd);
    }
    if s.len() > 2048 {
        return Err(DidError::TooLong);
    }
    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum HandleError {
    #[error("disallowed characters in handle (ASCII letters, digits, hyphen, and period")]
    InvalidChars,
    #[error("handle is too long (253 chars max")]
    TooLong,
    #[error("handle must contain at least two parts (separated by '.'")]
    InvalidFormat,
    #[error("handle parts (separated by '.') cannot be empty")]
    InvalidEmptyPart,
    #[error("handle part (separated by '.') too long (63 chars max)")]
    PartTooLong,
    #[error("handle parts (separaed by '.') cannot start / end with hyphen")]
    InvalidStartOrEnd,
    #[error("handle final part (top-level domain) must start with letter")]
    TldInvalidChar,
}

pub fn validate_handle(s: impl AsRef<str>) -> Result<(), HandleError> {
    let s = s.as_ref();
    static RE_DID_CHARS: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^[a-zA-Z0-9.-]*$").expect("regex failed to compile"));
    if !RE_DID_CHARS.is_match(s) {
        return Err(HandleError::InvalidChars);
    }
    if s.len() > 253 {
        return Err(HandleError::TooLong);
    }
    let parts = s.split(".");
    let count = parts.clone().count();
    if parts.clone().count() < 2 {
        return Err(HandleError::InvalidFormat);
    }
    for (i, part) in parts.enumerate() {
        if part.is_empty() {
            return Err(HandleError::InvalidEmptyPart);
        }
        if part.len() > 63 {
            return Err(HandleError::PartTooLong);
        }
        if part.starts_with("-") || part.ends_with("-") {
            return Err(HandleError::InvalidStartOrEnd);
        }
        if i == count - 1
            && !part
                .chars()
                .next()
                .expect("checked size")
                .is_ascii_alphabetic()
        {
            return Err(HandleError::TldInvalidChar);
        }
    }
    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum AtUriError {
    #[error("too long (max 8192 chars")]
    TooLong,
    #[error("can have at most one '#' to separate fragment")]
    TooManyFragmentParts,
    #[error("invalid characters")]
    InvalidChars,
    #[error("must start with 'at://'")]
    MissingProtocol,
    #[error("method and authority section must be present")]
    MissingSection,
    #[error("invalid did: {0}")]
    Did(DidError),
    #[error("invalid handle: {0}")]
    Handler(HandleError),
    #[error("invalid slash after authority without a path segment")]
    InvalidSlash,
    #[error("first path segment must be NSID: {0}")]
    InvalidPathSegment(NsidError),
    #[error("too many path segments")]
    TooManyPathSegments,
    #[error("invalid trailing slash")]
    InvalidTrailingSlash,
    #[error("fragment must be non-empty and start with a slash")]
    InvalidFragmentStart,
    #[error("invalid fragment characters")]
    InvalidFragmentChars,
}

pub fn validate_at_uri(s: impl AsRef<str>) -> Result<(), AtUriError> {
    let s = s.as_ref();
    if s.len() > 8192 {
        return Err(AtUriError::TooLong);
    }
    let mut fragment_split = s.split("#");
    if fragment_split.clone().count() > 2 {
        return Err(AtUriError::TooManyFragmentParts);
    }
    let uri = fragment_split
        .next()
        .expect("split should always have one element");
    let fragment = fragment_split.next();
    static RE_VALID_CHARS: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new("^[a-zA-Z0-9._~:@!$&')(*+,;=%/-]*$").expect("regex failed to compile")
    });
    if !RE_VALID_CHARS.is_match(s) {
        return Err(AtUriError::InvalidChars);
    }
    let mut uri_parts = uri.split("/");
    let parts_count = uri_parts.clone().count();
    if parts_count < 3 {
        return Err(AtUriError::MissingSection);
    }
    if let Some(last) = uri_parts.clone().last() {
        if last.is_empty() {
            return Err(AtUriError::InvalidTrailingSlash);
        }
    }
    let protocol = uri_parts.next().expect("already checked length");
    let empty = uri_parts.next().expect("already checked length");
    if protocol != "at:" || !empty.is_empty() {
        return Err(AtUriError::MissingProtocol);
    }
    let authority = uri_parts.next().expect("already checked length");
    if authority.starts_with("did:") {
        if let Err(e) = validate_did(authority) {
            return Err(AtUriError::Did(e));
        }
    } else {
        if let Err(e) = validate_handle(authority) {
            return Err(AtUriError::Handler(e));
        }
    }
    if let Some(path_segment) = uri_parts.next() {
        if let Err(e) = validate_nsid(path_segment) {
            return Err(AtUriError::InvalidPathSegment(e));
        }
    }
    if let Some(_path_segment) = uri_parts.next() {
        // nothing to really validate with rkey
    }
    if let Some(_path_segment) = uri_parts.next() {
        return Err(AtUriError::TooManyPathSegments);
    }
    if let Some(fragment) = fragment {
        if fragment.is_empty() || !fragment.starts_with("/") {
            return Err(AtUriError::InvalidFragmentStart);
        }
        static RE_FRAGMENT_VALID_CHARS: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(r#"^\/[a-zA-Z0-9._~:@!$&')(*+,;=%[\]/-]*$"#)
                .expect("regex failed to compile")
        });
        if !RE_FRAGMENT_VALID_CHARS.is_match(fragment) {
            return Err(AtUriError::InvalidChars);
        }
    }
    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum NsidError {
    #[error("disallowed characters (ASCII letters, digits, hyphens, and periods only)")]
    InvalidChars,
    #[error("too long (max 317 chars")]
    TooLong,
    #[error("should have at least three parts (separated by '.')")]
    NotEnoughParts,
    #[error("parts (separated by '.') cannot be empty")]
    EmptyPart,
    #[error("parts (separated by '.') too long (max 63 chars)")]
    PartTooLong,
    #[error("hyphen disallowed at start or end of part (separated by '.')")]
    InvalidHyphen,
    #[error("first part may not start with digit")]
    InvalidInitialDigit,
    #[error("name part must only be letters")]
    InvalidName,
}

pub fn validate_generic_nsid_segment(segment: &str) -> Result<(), NsidError> {
    if segment.is_empty() {
        return Err(NsidError::EmptyPart);
    }
    if segment.len() > 63 {
        return Err(NsidError::PartTooLong);
    }
    if segment.starts_with("-") || segment.ends_with("-") {
        return Err(NsidError::InvalidHyphen);
    }
    Ok(())
}

pub fn validate_nsid_auth_segment(segment: &str, first: bool) -> Result<(), NsidError> {
    validate_generic_nsid_segment(segment)?;
    if first
        && segment
            .chars()
            .next()
            .expect("non-empty checked")
            .is_ascii_digit()
    {
        return Err(NsidError::InvalidInitialDigit);
    }
    Ok(())
}

pub fn validate_nsid_name_segment(segment: &str) -> Result<(), NsidError> {
    validate_generic_nsid_segment(segment)?;
    if !segment.chars().all(|c| c.is_ascii_alphabetic()) {
        return Err(NsidError::InvalidName);
    }
    Ok(())
}

pub fn validate_nsid(s: impl AsRef<str>) -> Result<(), NsidError> {
    let s = s.as_ref();
    if s.len() > 317 {
        return Err(NsidError::TooLong);
    }
    static RE_VALID_CHARS: LazyLock<Regex> =
        LazyLock::new(|| Regex::new("^[a-zA-Z0-9.-]*$").expect("regex failed to compile"));
    if !RE_VALID_CHARS.is_match(s) {
        return Err(NsidError::InvalidChars);
    }
    let parts = s.split(".");
    let parts_count = parts.clone().count();
    if parts_count < 3 {
        return Err(NsidError::NotEnoughParts);
    }
    for (index, part) in parts.enumerate() {
        if index == parts_count - 1 {
            // last segment (name)
            validate_nsid_name_segment(part)?;
        } else {
            // authority part
            validate_nsid_auth_segment(part, index == 0)?;
        }
    }

    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum TidError {
    #[error("TID must be 13 characters")]
    InvalidLength,
    #[error("TID syntax invalid")]
    InvalidSyntax,
}

pub fn validate_tid(s: impl AsRef<str>) -> Result<(), TidError> {
    let s = s.as_ref();
    if s.len() != 13 {
        return Err(TidError::InvalidLength);
    }
    static RE_SYNTAX: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new("^[234567abcdefghij][234567abcdefghijklmnopqrstuvwxyz]{12}$")
            .expect("regex failed to compile")
    });
    if !RE_SYNTAX.is_match(s) {
        return Err(TidError::InvalidSyntax);
    }
    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum RecordKeyError {
    #[error("record key must be between 1 and 512 characters (inclusive)")]
    InvalidLength,
    #[error("invalid syntax")]
    InvalidSyntax,
    #[error("record key cannot be '.' or '..'")]
    InvalidRecordKey,
}

pub fn validate_record_key(s: impl AsRef<str>) -> Result<(), RecordKeyError> {
    let s = s.as_ref();
    if s.is_empty() || s.len() > 512 {
        return Err(RecordKeyError::InvalidLength);
    }
    static RE_SYNTAX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new("^[a-zA-Z0-9_~.:-]{1,512}$").expect("regex failed to compile"));
    if !RE_SYNTAX.is_match(s) {
        return Err(RecordKeyError::InvalidSyntax);
    }
    if s == "." || s == ".." {
        return Err(RecordKeyError::InvalidRecordKey);
    }
    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum UriError {
    #[error("invalid syntax")]
    InvalidSyntax,
}

pub fn validate_uri(s: impl AsRef<str>) -> Result<(), UriError> {
    let s = s.as_ref();
    static RE_SYNTAX: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r#"^\w+:(?:\/\/)?[^\s/][^\s]*$"#).expect("regex failed to compile")
    });
    if !RE_SYNTAX.is_match(s) {
        return Err(UriError::InvalidSyntax);
    }

    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum LanguageError {
    #[error("invalid syntax")]
    InvalidSyntax,
}

pub fn validate_language(s: impl AsRef<str>) -> Result<(), LanguageError> {
    let s = s.as_ref();
    // Validates well-formed BCP 47 syntax: https://www.rfc-editor.org/rfc/rfc5646.html#section-2.1
    static RE_SYNTAX: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r#"^((?<grandfathered>(en-GB-oed|i-ami|i-bnn|i-default|i-enochian|i-hak|i-klingon|i-lux|i-mingo|i-navajo|i-pwn|i-tao|i-tay|i-tsu|sgn-BE-FR|sgn-BE-NL|sgn-CH-DE)|(art-lojban|cel-gaulish|no-bok|no-nyn|zh-guoyu|zh-hakka|zh-min|zh-min-nan|zh-xiang))|((?<language>([A-Za-z]{2,3}(-(?<extlang>[A-Za-z]{3}(-[A-Za-z]{3}){0,2}))?)|[A-Za-z]{4}|[A-Za-z]{5,8})(-(?<script>[A-Za-z]{4}))?(-(?<region>[A-Za-z]{2}|[0-9]{3}))?(-(?<variant>[A-Za-z0-9]{5,8}|[0-9][A-Za-z0-9]{3}))*(-(?<extension>[0-9A-WY-Za-wy-z](-[A-Za-z0-9]{2,8})+))*(-(?<privateUseA>x(-[A-Za-z0-9]{1,8})+))?)|(?<privateUseB>x(-[A-Za-z0-9]{1,8})+))$"#).expect("regex failed to compile")
    });
    if !RE_SYNTAX.is_match(s) {
        return Err(LanguageError::InvalidSyntax);
    }

    Ok(())
}
