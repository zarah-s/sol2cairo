use crate::mods::lexer::{lexer::TTokenTrait, tokens::Token};

#[derive(Debug)]
pub enum Mutability {
    Constant,
    Immutable,
    Mutable,
    None,
}

pub enum DataLocation {
    Storage,
    Memory,
    Calldata,
}

impl Mutability {
    pub fn get_mutability_from_token(token: &Token) -> Self {
        match token {
            Token::Immutable => Mutability::Immutable,
            Token::Constant => Mutability::Constant,
            Token::Mutable => Mutability::Mutable,
            _ => Mutability::None,
        }
    }

    pub fn to_string(&self) -> String {
        match &self {
            Mutability::Immutable => Token::Immutable.to_string(),
            Mutability::Constant => Token::Constant.to_string(),
            Mutability::Mutable => Token::Mutable.to_string(),
            _ => unreachable!(),
        }
    }
}
