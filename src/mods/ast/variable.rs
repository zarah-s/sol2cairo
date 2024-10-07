use crate::mods::{
    lexer::tokens::Token,
    utils::types::{
        mutability::Mutability, value::Value, variant::Variant, visibility::Visibility,
    },
};

use super::mapping::MappingAST;

// #[derive(Debug)]

// pub struct StraightVariable {
//     pub data_type: String,
//     pub visibility: Visibility,
//     pub mutability: Mutability,
//     pub name: String,
//     pub is_array: bool,
//     pub is_payable: bool,
//     pub array_size: Option<String>,
//     pub data_location: Option<Token>,
// }

#[derive(Debug)]

pub enum VariableType {
    Tupple,
    Mapping(MappingAST),
    Straight(Variant),
}

#[derive(Debug)]
pub struct VariableAST {
    pub variable_type: VariableType,
    pub value: Option<Value>,
}

pub enum VariableState {
    None,
    DataType,
    Mutability,
    Visibility,
    Identifier,
    DataLocation,
    Assign,
    Value,
}
