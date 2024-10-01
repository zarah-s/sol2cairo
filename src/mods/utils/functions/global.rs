use std::env::VarError;

use regex::Regex;

use crate::mods::lexer::tokens::Token;

pub fn extract_data_type_from_token(token: &Token) -> Option<&Token> {
    match token {
        Token::Uint(_)
        | Token::Int(_)
        | Token::Bool
        | Token::Bytes(_)
        | Token::Address
        | Token::String => Some(token),

        _ => None,
    }
}

/// Validate identifier regex
pub fn validate_identifier(haystack: &str) -> Result<(), String> {
    let identifier_pattern = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();

    if identifier_pattern.is_match(haystack) {
        Ok(())
    } else {
        Err(format!("Invalid identifier pattern {}", haystack))
    }
}

pub fn get_env_vars(key: &str) -> Result<String, VarError> {
    std::env::var(key)
}
