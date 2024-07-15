use crate::mods::{
    functions::helpers::global::validate_identifier,
    types::{
        compiler_errors::{CompilerError, ErrType, SyntaxError},
        line_descriptors::LineDescriptions,
        token::{Mutability, Token, TokenTrait, VecExtension, Visibility},
    },
};

pub struct VariableIdentifier {
    pub data_type: String,
    pub visibility: Visibility,
    pub mutability: Mutability,
    pub name: String,
    pub value: Option<String>,
    pub is_array: bool,
    pub array_size: Option<String>,
    pub data_location: Option<Token>,
    pub index: Option<u8>,
}

enum VariableState {
    None,
    DataType,
    Mutability,
    Visibility,
    Identifier,
    Assign,
    Value,
}

pub fn parse_variables(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<VariableIdentifier> {
    let mut variables = Vec::new();

    // println!("{:?}", lexems);
    for lexem in lexems {
        let mut combined: Vec<Token> = Vec::new();
        for lex in lexem {
            for token in lex.data {
                match token {
                    Token::SemiColon => {
                        combined.push(token);
                        process_var_construct(&combined).unwrap_or_else(
                            |(err, err_type): (String, ErrType)| {
                                match err_type {
                                    ErrType::Missing => CompilerError::SyntaxError(
                                        SyntaxError::MissingToken(&format!(
                                            "{} for identifier {}",
                                            err,
                                            combined.to_string()
                                        )),
                                    )
                                    .throw_with_file_info(&std::env::var("file_path").unwrap(), lex.line),

                                    ErrType::Syntax => CompilerError::SyntaxError(
                                        SyntaxError::SyntaxError(&format!(
                                            "{} for identifier {}",
                                            err,
                                            combined.to_string()
                                        )),
                                    )
                                    .throw_with_file_info(&std::env::var("file_path").unwrap(), lex.line),
                                    ErrType::Unexpected => CompilerError::SyntaxError(
                                        SyntaxError::UnexpectedToken(&format!(
                                            "{} for identifier {}",
                                            err,
                                            combined.to_string()
                                        )),
                                    )
                                    .throw_with_file_info(&std::env::var("file_path").unwrap(), lex.line),
                                }
                                unreachable!()
                            },
                        );
                    }
                    // Token::Identifier(_identifier) => {}
                    _ => combined.push(token),
                }
            }
        }
    }

    variables
}

fn process_var_construct(combined: &Vec<Token>) -> Result<(), (String, ErrType)> {
    let mut state = VariableState::None;
    let mut data_type = String::new();
    let mut is_array = false;
    let mut variable_identifier = String::new();
    let mut array_size: Option<String> = None;
    let mut visibility = Visibility::Internal;
    let mut mutability = Mutability::Mutable;
    let mut updated_mutability = false;
    let mut updated_visibility = false;
    let mut pad = 0;
    let mut raw_value: Vec<Token> = Vec::new();
    for (index, token) in combined.iter().enumerate() {
        if pad > index {
            continue;
        }

        if let VariableState::Assign = state {
            match token {
                Token::SemiColon => {
                    if index != combined.len() - 1 {
                        return Err((";".to_string(), ErrType::Unexpected));
                    }
                }
                _ => raw_value.push(token.clone()),
            }
            continue;
        }
        match token {
            Token::Uint(_)
            | Token::Int(_)
            | Token::Bool
            | Token::Bytes(_)
            | Token::Address
            | Token::String => {
                if let VariableState::None = state {
                    data_type = token.to_string();
                    if let Token::OpenSquareBracket = combined[1] {
                        is_array = true;
                        let close_index = combined
                            .iter()
                            .position(|pred| *pred == Token::CloseSquareBracket);
                        if let Some(_close_index) = close_index {
                            let slice = &combined[2.._close_index];
                            if !slice.is_empty() {
                                let mut stringified_array_size = String::new();
                                pad = index + 1 + _close_index + 1;
                                for slc in slice {
                                    stringified_array_size.push_str(&slc.to_string());
                                }
                                array_size = Some(stringified_array_size);
                            } else {
                                pad = index + 3;
                            }
                        } else {
                            return Err(("]".to_string(), ErrType::Missing));
                        }
                    }
                } else {
                    return Err((token.to_string(), ErrType::Unexpected));
                }

                state = VariableState::DataType;
            }

            Token::Public
            | Token::Private
            | Token::Internal
            | Token::Constant
            | Token::Immutable => {
                if let VariableState::DataType
                | VariableState::Mutability
                | VariableState::Visibility = state
                {
                    if let Visibility::None = token.extract_visibility() {
                        if updated_mutability {
                            return Err((
                                format!("Mutability already set to \"{}\"", mutability.to_string()),
                                ErrType::Syntax,
                            ));
                        } else {
                            mutability = token.extract_mutability();
                            updated_mutability = true;
                        }
                        state = VariableState::Mutability;
                    } else {
                        if updated_visibility {
                            return Err((
                                format!("Visibility already set to \"{}\"", visibility.to_string()),
                                ErrType::Syntax,
                            ));
                        } else {
                            visibility = token.extract_visibility();
                            updated_visibility = true;
                        }
                        state = VariableState::Visibility;
                    }
                } else {
                    return Err((token.to_string(), ErrType::Unexpected));
                }
            }

            Token::Identifier(_identifier) => {
                if let VariableState::None = state {
                    let next = combined.get(index + 1);
                    if let Some(_next) = next {
                        if let Token::Dot = _next {
                            if let Some(_after_dot) = combined.get(index + 2) {
                                if let Token::Identifier(__identifier) = _after_dot {
                                    data_type = format!(
                                        "{}{}{}",
                                        combined[0].to_string(),
                                        combined[1].to_string(),
                                        combined[2].to_string()
                                    );
                                    if let Some(_after_identifier) = combined.get(index + 3) {
                                        if let Token::OpenSquareBracket = _after_identifier {
                                            is_array = true;
                                            let close_index = combined.iter().position(|pred| {
                                                *pred == Token::CloseSquareBracket
                                            });
                                            if let Some(_close_index) = close_index {
                                                let slice = &combined[2 + 2.._close_index];
                                                if !slice.is_empty() {
                                                    let mut stringified_array_size = String::new();
                                                    pad = index + 1 + _close_index + 1;
                                                    for slc in slice {
                                                        stringified_array_size
                                                            .push_str(&slc.to_string());
                                                    }
                                                    array_size = Some(stringified_array_size);
                                                } else {
                                                    pad = index + 3 + 3;
                                                }
                                            } else {
                                                return Err(("]".to_string(), ErrType::Missing));
                                            }
                                        }
                                    } else {
                                        return Err((
                                            "Unexpected end of statement".to_string(),
                                            ErrType::Syntax,
                                        ));
                                    }
                                } else {
                                    return Err((_after_dot.to_string(), ErrType::Unexpected));
                                }
                            } else {
                                return Err((
                                    "Unexpected end of statement".to_string(),
                                    ErrType::Syntax,
                                ));
                            }
                        } else {
                            data_type = token.to_string();
                            if let Token::OpenSquareBracket = _next {
                                is_array = true;
                                let close_index = combined
                                    .iter()
                                    .position(|pred| *pred == Token::CloseSquareBracket);
                                if let Some(_close_index) = close_index {
                                    let slice = &combined[2.._close_index];
                                    if !slice.is_empty() {
                                        let mut stringified_array_size = String::new();
                                        pad = index + 1 + _close_index + 1;
                                        for slc in slice {
                                            stringified_array_size.push_str(&slc.to_string());
                                        }
                                        array_size = Some(stringified_array_size);
                                    } else {
                                        pad = index + 3 + 1;
                                    }
                                } else {
                                    return Err(("]".to_string(), ErrType::Missing));
                                }
                            }
                        }
                    } else {
                        return Err(("Unexpected end of statement".to_string(), ErrType::Syntax));
                    }

                    state = VariableState::DataType;
                } else if let VariableState::DataType
                | VariableState::Mutability
                | VariableState::Visibility = state
                {
                    validate_identifier(&_identifier).unwrap_or_else(|err| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "{} for {}",
                            err,
                            combined.to_string()
                        )))
                        .throw_with_file_info(&std::env::var("file_path").unwrap(), 0)
                    });
                    variable_identifier.push_str(&_identifier);
                    state = VariableState::Identifier;
                } else {
                    return Err((token.to_string(), ErrType::Unexpected));
                }
            }

            Token::Equals => {
                if let VariableState::Identifier = state {
                    state = VariableState::Assign;
                } else {
                    return Err((token.to_string(), ErrType::Unexpected));
                }
            }

            Token::SemiColon | Token::Space => {}
            _ => {
                return Err((token.to_string(), ErrType::Unexpected));
            }
        }
    }

    if !raw_value.is_empty() {}
    println!("{:?}", raw_value.to_string());

    Ok(())
}

fn process_variable_value(raw_value: Vec<Token>) {}
