use crate::mods::utils::types::{value::Value, variant::Variant};

use super::{function::FunctionHeader, mapping::MappingAST};

#[derive(Debug)]

pub enum VariableType {
    // Tupple,
    Mapping(MappingAST),
    Straight(Variant),
    FunctionPTR(FunctionHeader),
}

#[derive(Debug)]
pub struct VariableAST {
    pub variable_type: VariableType,
    pub value: Option<Value>,
}
