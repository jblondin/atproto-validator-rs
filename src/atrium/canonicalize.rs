use atrium_api::types::string::Nsid;
use atrium_lex::{
    LexiconDoc,
    lexicon::{
        LexArray, LexArrayItem, LexObject, LexObjectProperty, LexRecord, LexRecordRecord, LexRef,
        LexRefUnion, LexUserType, LexXrpcBody, LexXrpcBodySchema, LexXrpcProcedure, LexXrpcQuery,
        LexXrpcSubscription, LexXrpcSubscriptionMessage, LexXrpcSubscriptionMessageSchema,
    },
};

use crate::{Canonicalize, CanonicalizeDocument};

impl CanonicalizeDocument for LexiconDoc {
    type Error = &'static str;

    fn canonicalize(&mut self) -> Result<(), Self::Error> {
        let nsid = Nsid::new(self.id.clone())?;
        for user_type in self.defs.values_mut() {
            user_type.canonicalize(&nsid);
        }
        Ok(())
    }
}

impl Canonicalize for LexUserType {
    fn canonicalize(&mut self, nsid: &Nsid) {
        match self {
            LexUserType::Record(lex_record) => lex_record.canonicalize(nsid),
            LexUserType::XrpcQuery(lex_xrpc_query) => lex_xrpc_query.canonicalize(nsid),
            LexUserType::XrpcProcedure(lex_xrpc_procedure) => lex_xrpc_procedure.canonicalize(nsid),
            LexUserType::XrpcSubscription(lex_xrpc_subscription) => {
                lex_xrpc_subscription.canonicalize(nsid)
            }
            LexUserType::Array(lex_array) => lex_array.canonicalize(nsid),
            LexUserType::Object(lex_object) => lex_object.canonicalize(nsid),
            _ => {}
        }
    }
}

impl Canonicalize for LexRecord {
    fn canonicalize(&mut self, nsid: &Nsid) {
        self.record.canonicalize(nsid)
    }
}

impl Canonicalize for LexRecordRecord {
    fn canonicalize(&mut self, nsid: &Nsid) {
        match self {
            LexRecordRecord::Object(lex_object) => lex_object.canonicalize(nsid),
        }
    }
}

impl Canonicalize for LexXrpcQuery {
    fn canonicalize(&mut self, nsid: &Nsid) {
        if let Some(lex_xrpc_body) = &mut self.output {
            lex_xrpc_body.canonicalize(nsid);
        }
    }
}

impl Canonicalize for LexXrpcBody {
    fn canonicalize(&mut self, nsid: &Nsid) {
        if let Some(lex_xrpc_body_schema) = &mut self.schema {
            lex_xrpc_body_schema.canonicalize(nsid);
        }
    }
}

impl Canonicalize for LexXrpcBodySchema {
    fn canonicalize(&mut self, nsid: &Nsid) {
        match self {
            LexXrpcBodySchema::Ref(lex_ref) => lex_ref.canonicalize(nsid),
            LexXrpcBodySchema::Union(lex_ref_union) => lex_ref_union.canonicalize(nsid),
            LexXrpcBodySchema::Object(lex_object) => lex_object.canonicalize(nsid),
        }
    }
}

impl Canonicalize for LexXrpcProcedure {
    fn canonicalize(&mut self, nsid: &Nsid) {
        if let Some(input_body) = &mut self.input {
            input_body.canonicalize(nsid);
        }
        if let Some(output_body) = &mut self.output {
            output_body.canonicalize(nsid);
        }
    }
}

impl Canonicalize for LexXrpcSubscription {
    fn canonicalize(&mut self, nsid: &Nsid) {
        if let Some(message) = &mut self.message {
            message.canonicalize(nsid);
        }
    }
}

impl Canonicalize for LexXrpcSubscriptionMessage {
    fn canonicalize(&mut self, nsid: &Nsid) {
        if let Some(schema) = &mut self.schema {
            schema.canonicalize(nsid);
        }
    }
}

impl Canonicalize for LexXrpcSubscriptionMessageSchema {
    fn canonicalize(&mut self, nsid: &Nsid) {
        match self {
            LexXrpcSubscriptionMessageSchema::Ref(lex_ref) => lex_ref.canonicalize(nsid),
            LexXrpcSubscriptionMessageSchema::Union(lex_ref_union) => {
                lex_ref_union.canonicalize(nsid)
            }
            LexXrpcSubscriptionMessageSchema::Object(lex_object) => lex_object.canonicalize(nsid),
        }
    }
}

impl Canonicalize for LexObject {
    fn canonicalize(&mut self, nsid: &Nsid) {
        for obj_prop in self.properties.values_mut() {
            obj_prop.canonicalize(nsid);
        }
    }
}

impl Canonicalize for LexObjectProperty {
    fn canonicalize(&mut self, nsid: &Nsid) {
        match self {
            LexObjectProperty::Ref(lex_ref) => lex_ref.canonicalize(nsid),
            LexObjectProperty::Union(lex_ref_union) => lex_ref_union.canonicalize(nsid),
            LexObjectProperty::Array(lex_array) => lex_array.canonicalize(nsid),
            _ => {}
        }
    }
}

impl Canonicalize for LexArray {
    fn canonicalize(&mut self, nsid: &Nsid) {
        self.items.canonicalize(nsid)
    }
}

impl Canonicalize for LexArrayItem {
    fn canonicalize(&mut self, nsid: &Nsid) {
        match self {
            LexArrayItem::Ref(lex_ref) => lex_ref.canonicalize(nsid),
            LexArrayItem::Union(lex_ref_union) => lex_ref_union.canonicalize(nsid),
            _ => {}
        }
    }
}

impl Canonicalize for LexRef {
    fn canonicalize(&mut self, nsid: &Nsid) {
        if self.r#ref.starts_with("#") {
            self.r#ref = format!("{}{}", nsid.as_str(), self.r#ref);
        }
    }
}

impl Canonicalize for LexRefUnion {
    fn canonicalize(&mut self, nsid: &Nsid) {
        for r#ref in &mut self.refs {
            if r#ref.starts_with("#") {
                *r#ref = format!("{}{}", nsid.as_str(), r#ref);
            }
        }
    }
}
