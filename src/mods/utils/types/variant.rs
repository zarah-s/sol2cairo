use crate::mods::{lexer::tokens::Token, utils::functions::variant::process_args};

/// ARGUMENT DEFINITION
#[derive(Debug)]
pub struct Variant {
    pub r#type: Option<String>,
    pub name: Option<String>,
    pub location: Option<Token>,
    pub size: Option<String>,
    pub is_array: bool,
    pub indexed: Option<bool>,
    pub payable_address: bool,
}

pub enum ArgState {
    None,
    Type,
    Location,
    Array,
    Name,
}

pub trait TVariant {
    fn new() -> Self;
    fn process_args(raw_args: &[Token]) -> Result<Variant, &'static str>;
}

impl TVariant for Variant {
    fn new() -> Self {
        Self {
            r#type: None,
            name: None,
            location: None,
            size: None,
            indexed: None,
            is_array: false,
            payable_address: false,
        }
    }

    fn process_args(raw_args: &[Token]) -> Result<Variant, &'static str> {
        process_args(raw_args)
    }
}
