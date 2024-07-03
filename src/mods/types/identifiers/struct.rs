use super::mapping::MappingIdentifier;

pub struct Header {
    pub identifier: String,
    pub is_storage: bool,
}

pub struct StructIdentifier {
    pub header: Header,
    pub types: Vec<StructType>,
}

pub enum StructType {
    Mapping(MappingIdentifier),
    Variant(Variant),
}

#[derive(Debug)]
pub struct Variant {
    pub r#type: String,
    pub name: String,
    pub size: Option<String>,
    pub is_array: bool,
}
