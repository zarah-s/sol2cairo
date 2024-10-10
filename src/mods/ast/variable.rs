use crate::mods::{
    lexer::tokens::Token,
    utils::types::{value::Value, variant::Variant},
};

use super::{function::FunctionHeader, mapping::MappingAST};

#[derive(Debug)]

pub struct FunctionTypeDetails {
    pub name: String,
    pub visibility: Option<Token>,
}

#[derive(Debug)]

pub enum VariableType {
    // Tupple,
    Mapping(MappingAST),
    Straight(Variant),
    FunctionPTR(FunctionTypeDetails, FunctionHeader),
}

#[derive(Debug)]
pub struct VariableAST {
    pub variable_type: VariableType,
    pub value: Option<Value>,
}
