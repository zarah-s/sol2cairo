use super::token::{StringExtension, Token, VecExtension};

#[derive(Debug, Clone)]
pub struct LineDescriptions<T> {
    pub line: i32,
    pub data: T,
}
pub trait StringDescriptor {
    fn lex(&self) -> LineDescriptions<Vec<Token>>;
}

pub trait TokenDescriptor {
    fn to_string(&self) -> LineDescriptions<String>;
}

impl StringDescriptor for LineDescriptions<String> {
    fn lex(&self) -> LineDescriptions<Vec<Token>> {
        LineDescriptions {
            line: self.line,
            data: self.data.lex(),
        }
    }
}

impl TokenDescriptor for LineDescriptions<Vec<Token>> {
    fn to_string(&self) -> LineDescriptions<String> {
        LineDescriptions {
            line: self.line,
            data: self.data.to_string(),
        }
    }
}
