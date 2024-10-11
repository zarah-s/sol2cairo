use crate::mods::ast::function::{FunctionHeader, FunctionType};
use crate::mods::ast::mapping::MappingReturn;
use crate::mods::constants::constants::FILE_PATH;
use crate::mods::parser::function::parse_function_header;
use crate::mods::utils::types::variant::{TVariant, Variant};
use crate::mods::{
    ast::mapping::{Mapping, MappingHeader, MappingState, MappingValue},
    errors::error::{CompilerError, ErrType, SyntaxError},
    lexer::{
        lexer::{TTokenTrait, TVecExtension},
        tokens::Token,
    },
    utils::{
        functions::global::{extract_data_type_from_token, get_env_vars, validate_identifier},
        types::visibility::Visibility,
    },
};

pub fn process_mapping(
    _combined: &Vec<Token>,
    mapping: &mut Mapping,
    mapping_header: &mut MappingHeader,
) -> Result<(), (String, ErrType)> {
    let mut state = MappingState::None;
    let mut pad = 0;
    let mut nested_count = 0;
    let combined = &_combined.strip_spaces();

    /* CONSTRUCT DATA */

    for (index, n) in combined.iter().enumerate() {
        if pad > index {
            continue;
        }
        match n {
            Token::Mapping => {
                nested_count += 1;
                state = MappingState::MappingIdentity;
            }
            Token::OpenParenthesis => {
                if let MappingState::MappingIdentity = state {
                    state = MappingState::OpenParenthesisIdentifier;
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::CloseParenthesis => {
                nested_count -= 1;
                if let MappingState::Value | MappingState::CloseParenthesisIdentifier = state {
                    state = MappingState::CloseParenthesisIdentifier;
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::Public | Token::Private | Token::Internal | Token::External => {
                if let MappingState::CloseParenthesisIdentifier = state {
                    if nested_count != 0 {
                        return Err((
                            format!("Invalid declaration \"{}\"", n.to_string()),
                            ErrType::Syntax,
                        ));
                    } else {
                        if let Visibility::None = n.extract_visibility() {
                            return Err((
                                format!("Invalid declaration \"{}\"", n.to_string()),
                                ErrType::Syntax,
                            ));
                        } else {
                            mapping_header.visibility = n.extract_visibility();
                        }
                    }
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::Equals => {
                if let MappingState::Key = state {
                    state = MappingState::Assign;
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::Gt => {
                if let MappingState::Assign = state {
                    state = MappingState::Gt;
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }

            Token::Function => {
                if let MappingState::Gt = state {
                    let mut iteration = 0;
                    let mut open_context = 0;
                    for tkn in &combined[index..] {
                        match tkn {
                            Token::OpenParenthesis => {
                                open_context += 1;
                            }
                            Token::CloseParenthesis => {
                                if open_context == 0 {
                                    break;
                                }
                                open_context -= 1;
                            }
                            _ => {}
                        }

                        iteration += 1;
                    }

                    if open_context != 0 {
                        return Err((
                            format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                            ErrType::Syntax,
                        ));
                    }

                    pad = iteration + index;

                    let function_header =
                        parse_function_header(combined[index..iteration + index].to_vec(), 0);

                    mapping.insert(
                        None,
                        Some(MappingValue::Raw(MappingReturn::Function(FunctionHeader {
                            r#type: FunctionType::Variable,
                            ..function_header
                        }))),
                    )?;
                    state = MappingState::Value;
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::Uint(_)
            | Token::Int(_)
            | Token::Identifier(_)
            | Token::Bool
            | Token::Bytes(_)
            | Token::Address
            | Token::String => {
                if let MappingState::OpenParenthesisIdentifier = state {
                    let mut key = String::new();

                    if let None = extract_data_type_from_token(n) {
                        let slc = &combined[index..]
                            .iter()
                            .position(|pred| *pred == Token::Equals);
                        if slc.is_none() {
                            return Err((
                                format!("=> \"{}\"", _combined.to_string()),
                                ErrType::Unexpected,
                            ));
                        }

                        key.push_str(&&combined[index..slc.unwrap() + index].to_vec().to_string());
                        pad = index + slc.unwrap();
                    } else {
                        key.push_str(&n.to_string());
                    }

                    mapping.insert(Some(key), None)?;
                    state = MappingState::Key;
                } else if let MappingState::Gt = state {
                    let mut open_context = 0;
                    let mut iiteration = 0;
                    {
                        for tkn in &combined[index..] {
                            match tkn {
                                Token::OpenParenthesis => open_context += 1,
                                Token::CloseParenthesis => {
                                    if open_context == 0 {
                                        break;
                                    }
                                    open_context -= 1;
                                }
                                _ => {}
                            }

                            iiteration += 1;
                        }

                        if open_context != 0 {
                            return Err((
                                format!(
                                    "Invalid variant declaration \"{}\"",
                                    _combined.to_string()
                                ),
                                ErrType::Syntax,
                            ));
                        }
                    }

                    let variant = Variant::process_args(&combined[index..iiteration + index]);
                    if variant.is_err() {
                        return Err((
                            format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                            ErrType::Syntax,
                        ));
                    }

                    pad = iiteration + index;

                    mapping.insert(
                        None,
                        Some(MappingValue::Raw(MappingReturn::Variant(variant.unwrap()))),
                    )?;
                    state = MappingState::Value;
                } else if let MappingState::CloseParenthesisIdentifier = state {
                    if nested_count == 0 {
                        if let Token::Identifier(identifier) = n {
                            validate_identifier(&identifier).unwrap_or_else(|err| {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                    "{} for mapping",
                                    err
                                )))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), 0);
                            });
                            mapping_header.identifier = identifier.to_string();
                        } else {
                            return Err((
                                format!("Expecting identifier but found \"{}\"", n.to_string()),
                                ErrType::Syntax,
                            ));
                        }
                    } else {
                        return Err((
                            format!("Expecting identifier but found \"{}\"", n.to_string()),
                            ErrType::Syntax,
                        ));
                    }
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }

            Token::SemiColon => {}
            _ => {
                return Err((
                    format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                    ErrType::Syntax,
                ));
            }
        }
    }

    if nested_count != 0 {
        return Err((
            format!("Invalid variant declaration \"{}\"", _combined.to_string()),
            ErrType::Syntax,
        ));
    }

    Ok(())
}
