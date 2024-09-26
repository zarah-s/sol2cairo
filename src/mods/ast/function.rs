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

#[derive(Debug)]
pub struct ReturnType {
    pub type_: String,
    pub location: Option<Token>,
    pub size: Option<String>,
    pub is_array: bool,
}

#[derive(Debug)]
pub struct ModifierCall {
    pub identifier: String,
    pub arguments: Option<Vec<String>>,
}

#[derive(Debug)]
pub struct Argument {
    pub type_: String,
    pub name_: String,
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
    pub mutability: Token,
    pub visibility: Token,
    pub returns: Option<Vec<ReturnType>>,
    pub r#override: bool,
    pub r#virtual: bool,
    pub arguments: Option<Vec<Argument>>,
    pub modifiers: Option<Vec<ModifierCall>>,
}

impl FunctionHeader {
    pub fn new() -> Self {
        Self {
            arguments: None,
            gasless: false,
            modifiers: None,
            mutability: Token::Space,
            name: String::new(),
            r#override: false,
            returns: None,
            r#virtual: false,
            visibility: Token::Space,
        }
    }
}
