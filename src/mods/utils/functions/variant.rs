use crate::mods::{
    lexer::{
        lexer::{TStringExtension, TTokenTrait, TVecExtension},
        tokens::Token,
    },
    utils::types::variant::{ArgState, TVariant, Variant},
};

/// Process function args
pub fn process_args(raw_args: &[Token]) -> Result<Variant, &'static str> {
    let mut state = ArgState::None;

    let mut arg = Variant::new();
    let mut pad = 0;
    for (index, token) in raw_args.iter().enumerate() {
        if pad > index {
            continue;
        }
        match token {
            Token::Uint(_)
            | Token::Int(_)
            | Token::Bool
            | Token::Bytes(_)
            | Token::Address
            | Token::String => {
                if let ArgState::None = state {
                    if arg.r#type.is_none() {
                        arg.r#type = Some(String::new())
                    }
                    arg.r#type.as_mut().unwrap().push_str(&token.to_string());
                    state = ArgState::Type;
                } else {
                    return Err("Unprocessible entity.");
                }
            }

            Token::Payable => {
                if let ArgState::Type | ArgState::Location = state {
                    if arg.r#type.is_some()
                        && arg.r#type.clone().unwrap().tokenize() != Token::Address
                    {
                        return Err("payable can only be specified for address types");
                    }
                    arg.payable_address = true;
                } else {
                    return Err("Unprocessible entity for");
                }
            }

            Token::Indexed => {
                if let ArgState::Type | ArgState::Array = state {
                    if arg.indexed.is_some() {
                        return Err("Unprocessible entity.");
                    }
                    arg.indexed = Some(true);
                } else {
                    return Err("Unprocessible entity.");
                }
            }

            Token::OpenSquareBracket => {
                if let ArgState::Type | ArgState::Location = state {
                    let mut iteration = 0;
                    let mut open_context = 0;

                    for tkn in &raw_args[index..] {
                        match tkn {
                            Token::OpenSquareBracket => open_context += 1,
                            Token::CloseSquareBracket => {
                                open_context -= 1;
                                if open_context == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }

                        iteration += 1;
                    }

                    if open_context != 0 {
                        return Err("Unprocessible entity.");
                    }

                    let _size = &raw_args[index + 1..iteration + index];
                    if !_size.is_empty() {
                        arg.size = Some(_size.to_vec().to_string());
                    }
                    arg.is_array = true;
                    pad = index + iteration + 1;
                    state = ArgState::Array;
                } else {
                    return Err("Unprocessible entity.");
                }
            }

            Token::Memory | Token::Storage | Token::Calldata => {
                if let ArgState::Type | ArgState::Array = state {
                    arg.location = Some(token.clone());
                    state = ArgState::Location;
                } else {
                    return Err("Unprocessible entity.");
                }
            }

            Token::Identifier(_identifier) => {
                if let ArgState::None | ArgState::Location | ArgState::Array | ArgState::Type =
                    state
                {
                    match state {
                        ArgState::None => {
                            if arg.r#type.is_none() {
                                arg.r#type = Some(String::new())
                            }
                            arg.r#type.as_mut().unwrap().push_str(&token.to_string());
                            state = ArgState::Type;
                        }

                        _ => {
                            arg.name = Some(token.to_string());
                            state = ArgState::Name;
                        }
                    }
                } else {
                    return Err("Unprocessible entity.");
                }
            }

            Token::Dot => {
                if let ArgState::Type = state {
                    if arg.r#type.is_none() {
                        return Err("Unprocessible entity.");
                    }
                    arg.r#type.as_mut().unwrap().push_str(".");
                    if let Some(variant) = raw_args.get(index + 1) {
                        if let Token::Identifier(variant_value) = variant {
                            arg.r#type.as_mut().unwrap().push_str(&variant_value);
                            pad = index + 2;
                        } else {
                            return Err("Unprocessible entity.");
                        }
                    } else {
                        return Err("Unprocessible entity.");
                    }
                } else {
                    return Err("Unprocessible entity.");
                }
            }
            Token::Space => {}
            _ => {
                return Err("Unprocessible entity.");
            }
        }
    }

    Ok(arg)
}
