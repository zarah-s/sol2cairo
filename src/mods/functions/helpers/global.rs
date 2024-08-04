use std::env::VarError;

use regex::Regex;

use crate::mods::types::{
    compiler_errors::ErrType,
    token::{TTokenTrait, TVecExtension, Token},
};

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

pub fn process_size(
    combined: &Vec<Token>,
    open_bracket_index: usize,
    close_bracket_index: usize,
) -> Result<Option<String>, (String, ErrType)> {
    let size_definition = &combined[open_bracket_index + 1..][..close_bracket_index];
    if !size_definition.to_vec().strip_spaces().is_empty() {
        let mut opened_paren_count = 0;
        for size_ in size_definition.to_vec().strip_spaces() {
            match size_ {
                Token::Identifier(_)
                | Token::Multiply
                | Token::Modulu
                | Token::Plus
                | Token::Minus
                | Token::Divide => {}
                Token::OpenParenthesis => opened_paren_count += 1,
                Token::CloseParenthesis => opened_paren_count -= 1,
                _token => {
                    return Err((format!("{} ", _token.to_string()), ErrType::Unexpected));
                }
            }
        }

        if opened_paren_count != 0 {
            if opened_paren_count < 0 {
                return Err((format!(")",), ErrType::Unexpected));
            } else {
                return Err((format!(")",), ErrType::Missing));
            }
        }

        return Ok(Some(size_definition.to_vec().to_string()));
    }

    return Ok(None);
}

pub fn process_type(
    slice: &[Token],
    r#type: &mut String,
    combined: &Vec<Token>,
) -> Result<(), (String, ErrType)> {
    if slice.len() == 3 {
        if slice.contains(&Token::Dot) {
            for slc in slice {
                match slc {
                    Token::Identifier(identifier) => {
                        let variable_name_pattern =
                            Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
                        if !variable_name_pattern.is_match(&identifier) {
                            return Err((
                                format!("Invalid type identifier \"{}\"", identifier),
                                ErrType::Syntax,
                            ));
                        }
                        r#type.push_str(&identifier);
                    }
                    Token::Dot => r#type.push_str(&Token::Dot.to_string()),
                    _ => {
                        return Err((
                            format!("Invalid variant declaration \"{}\"", combined.to_string()),
                            ErrType::Syntax,
                        ));
                    }
                }
            }
        } else {
            return Err((
                format!("Invalid variant declaration \"{}\"", combined.to_string()),
                ErrType::Syntax,
            ));
        }
    } else if slice.len() == 1 {
        let variable_name_pattern = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
        if !variable_name_pattern.is_match(&slice.first().unwrap().to_string()) {
            return Err((
                format!("Invalid variant declaration \"{}\"", combined.to_string()),
                ErrType::Syntax,
            ));
        }
        r#type.push_str(&slice.first().unwrap().to_string())
    } else {
        return Err((
            format!("Invalid variant declaration \"{}\"", combined.to_string()),
            ErrType::Syntax,
        ));
    }

    Ok(())
}

pub fn process_name(
    name_definition: &[Token],
    name: &mut String,
    combined: &Vec<Token>,
) -> Result<(), (String, ErrType)> {
    if name_definition.len() == 2 {
        if let Token::Identifier(name_) = name_definition.first().unwrap() {
            let variable_name_pattern = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
            if !variable_name_pattern.is_match(&name_) {
                return Err((
                    format!("Invalid name identifier \"{}\"", name_),
                    ErrType::Syntax,
                ));
            }
            name.push_str(&name_);
        }

        if *name_definition.last().unwrap() != Token::SemiColon {
            return Err((format!("; \"{}\"", combined.to_string()), ErrType::Missing));
        }
    } else {
        return Err((
            format!("Invalid variant declaration \"{}\"", combined.to_string()),
            ErrType::Syntax,
        ));
    }

    Ok(())
}

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
