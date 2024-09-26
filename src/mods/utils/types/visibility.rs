use crate::mods::lexer::{lexer::TTokenTrait, tokens::Token};

#[derive(Debug)]
pub enum Visibility {
    Public,
    Internal,
    External,
    Private,
    None,
}

impl Visibility {
    pub fn get_visibility_from_token(token: &Token) -> Self {
        match token {
            Token::Public => Visibility::Public,
            Token::Internal => Visibility::Internal,
            Token::External => Visibility::External,
            Token::Private => Visibility::Private,
            _ => Visibility::None,
        }
    }

    pub fn to_string(&self) -> String {
        match &self {
            Visibility::External => Token::External.to_string(),
            Visibility::Public => Token::Public.to_string(),
            Visibility::Internal => Token::Internal.to_string(),
            Visibility::Private => Token::Private.to_string(),
            _ => unreachable!(),
        }
    }
}
