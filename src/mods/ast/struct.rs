use super::mapping::MappingAST;

pub trait TStructIdentifier {
    fn is_storage(&self) -> bool;
}

#[derive(Debug)]
pub struct StructHeader {
    pub identifier: String,
}

#[derive(Debug)]
pub struct StructAST {
    pub header: StructHeader,
    pub line: String,
    pub types: Vec<StructType>,
}

#[derive(Debug)]
pub enum StructType {
    Mapping(MappingAST),
    Variant(StructVariant),
}

#[derive(Debug)]
pub struct StructVariant {
    pub r#type: String,
    pub name: String,
    pub array_size: Option<String>,
    pub is_array: bool,
    pub payable: bool,
}

impl StructVariant {
    pub fn new() -> Self {
        Self {
            r#type: String::new(),
            name: String::new(),
            array_size: None,
            is_array: false,
            payable: false,
        }
    }
}

impl TStructIdentifier for StructAST {
    fn is_storage(&self) -> bool {
        for data in &self.types {
            match data {
                StructType::Mapping(_) => return true,
                _ => {}
            }
        }

        false
    }
}
