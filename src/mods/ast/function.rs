use crate::mods::{lexer::tokens::Token, utils::types::variant::Variant};

#[derive(Debug)]
pub enum FunctionHeaderState {
    Keyword,
    Args,
    Visibility,
    Mutability,
    Inheritance,
    Modifiers,
    Identifier,
    Returns,
    Gasless,
    MemLocation,
    VarVisibility,
    VarName,
    Array,
    None,
}

#[derive(Debug)]
pub struct ModifierCall {
    pub identifier: String,
    pub arguments: Option<Vec<String>>,
}

#[derive(Debug)]
pub enum FunctionType {
    Variable,
    Interface,
}

#[derive(Debug)]
pub struct FunctionHeader {
    pub name: Option<String>,
    pub gasless: bool,
    pub mutability: Option<Token>,
    pub visibility: Option<Token>,
    pub returns: Option<Vec<Variant>>,
    pub inheritance: Option<Token>,
    pub r#type: FunctionType,
    pub arguments: Option<Vec<ArgType>>,
    pub modifiers: Option<Vec<ModifierCall>>,
    /// for variable type function headers
    pub is_array: bool,
    pub size: Option<String>,
    pub location: Option<Token>,
    pub var_visibility: Option<Token>,
    pub var_name: Option<String>,
}

#[derive(Debug)]
pub enum ArgType {
    Function(FunctionHeader),
    Variant(Variant),
}

impl FunctionHeader {
    pub fn new() -> Self {
        Self {
            arguments: None,
            gasless: false,
            modifiers: None,
            mutability: None,
            name: None,
            inheritance: None,
            returns: None,
            visibility: None,
            r#type: FunctionType::Interface,
            is_array: false,
            location: None,
            size: None,
            var_name: None,
            var_visibility: None,
        }
    }
}
