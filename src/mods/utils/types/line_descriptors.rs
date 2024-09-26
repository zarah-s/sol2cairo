use crate::mods::lexer::{
    lexer::{TStringExtension, TVecExtension},
    tokens::Token,
};

#[derive(Debug, Clone)]
pub struct LineDescriptions<T> {
    pub line: i32,
    pub data: T,
}
pub trait TStringDescriptor {
    fn lex(&self) -> LineDescriptions<Vec<Token>>;
}

pub trait TTokenDescriptor {
    fn to_string(&self) -> LineDescriptions<String>;
}

impl TStringDescriptor for LineDescriptions<String> {
    fn lex(&self) -> LineDescriptions<Vec<Token>> {
        LineDescriptions {
            line: self.line,
            data: self.data.lex(),
        }
    }
}

impl TTokenDescriptor for LineDescriptions<Vec<Token>> {
    fn to_string(&self) -> LineDescriptions<String> {
        LineDescriptions {
            line: self.line,
            data: self.data.to_string(),
        }
    }
}
