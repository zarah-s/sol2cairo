use crate::mods::utils::types::variant::Variant;

use super::{
    function::{FunctionHeader, FunctionPTRDetails},
    mapping::MappingAST,
};

// pub trait TStructIdentifier {
//     fn is_storage(&self) -> bool;
// }

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
    Variant(Variant),
    Function(FunctionPTRDetails, FunctionHeader),
}

// impl TStructIdentifier for StructAST {
//     fn is_storage(&self) -> bool {
//         for data in &self.types {
//             match data {
//                 StructType::Mapping(_) => return true,
//                 _ => {}
//             }
//         }

//         false
//     }
// }
