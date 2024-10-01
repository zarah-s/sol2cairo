use crate::mods::{lexer::tokens::Token, utils::types::variant::Variant};

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
    Invocable,
}

#[derive(Debug)]
pub struct FunctionHeader {
    pub name: Option<String>,
    pub gasless: bool,
    pub mutability: Option<Token>,
    pub visibility: Option<Token>,
    pub returns: Option<Vec<Variant>>,
    pub inheritance: Option<Token>,

    pub arguments: Option<Vec<Variant>>,
    pub modifiers: Option<Vec<ModifierCall>>,
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
        }
    }
}
