use crate::mods::types::token::Token;

pub struct MappingIdentifier {
    pub identifier: String,
    pub map: Mapping,
    pub visibility: Token,
}

pub enum MappingValue {
    Mapping(Box<Mapping>),
    Raw(String),
}

pub struct Mapping {
    pub key: Option<String>,
    pub value: Option<MappingValue>,
}
