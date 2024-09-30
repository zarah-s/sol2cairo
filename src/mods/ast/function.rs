use crate::mods::lexer::tokens::Token;

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

pub enum FunctionArgState {
    None,
    Type,
    Location,
    Array,
    Name,
}

#[derive(Debug)]
pub struct ModifierCall {
    pub identifier: String,
    pub arguments: Option<Vec<String>>,
}

#[derive(Debug)]
pub struct Argument {
    pub r#type: String,
    pub name: Option<String>,
    pub location: Option<Token>,
    pub size: Option<String>,
    pub is_array: bool,
    pub payable_address: bool,
}

#[derive(Debug)]
pub enum FunctionType {
    Variable,
    Interface,
    Invocable,
}

#[derive(Debug)]
pub struct FunctionHeader {
    pub name: String,
    pub gasless: bool,
    pub mutability: Option<Token>,
    pub visibility: Option<Token>,
    pub returns: Option<Vec<Argument>>,
    pub inheritance: Option<Token>,

    pub arguments: Option<Vec<Argument>>,
    pub modifiers: Option<Vec<ModifierCall>>,
}

impl FunctionHeader {
    pub fn new() -> Self {
        Self {
            arguments: None,
            gasless: false,
            modifiers: None,
            mutability: None,
            name: String::new(),
            inheritance: None,
            returns: None,
            visibility: None,
        }
    }
}
