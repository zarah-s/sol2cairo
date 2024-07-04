use crate::mods::{
    functions::helpers::global::{extract_data_type_from_token, process_size, process_type},
    types::{
        compiler_errors::ErrType,
        token::{StringExtension, Token, TokenTrait, VecExtension},
    },
};

#[derive(Debug)]
enum MappingState {
    MappingIdentity,
    OpenParenthesisIdentifier,
    CloseParenthesisIdentifier,
    Key,
    Assign,
    Gt,
    Value,
    None,
}

#[derive(Debug)]
pub struct MappingIdentifier {
    pub identifier: String,
    pub visibility: Option<Token>,
    pub map: Mapping,
}
#[derive(Debug)]
pub struct ReturnValue {
    pub r#type: String,
    pub size: Option<String>,
    pub is_array: bool,
}

#[derive(Debug)]
pub enum MappingValue {
    Mapping(Box<Mapping>),
    Raw(ReturnValue),
}

#[derive(Debug)]

pub struct Mapping {
    pub key: Option<String>,
    pub value: Option<MappingValue>,
}

impl Mapping {
    pub fn new() -> Self {
        Self {
            key: None,
            value: None,
        }
    }

    pub fn get_return_type(&self) -> Option<&ReturnValue> {
        if let Some(ref _val) = self.value {
            match _val {
                MappingValue::Mapping(_map) => _map.get_return_type(),
                MappingValue::Raw(_return) => return Some(_return),
            };
        }
        None
    }

    pub fn insert(
        &mut self,
        key: Option<String>,
        value: Option<MappingValue>,
    ) -> Result<(), (String, ErrType)> {
        if self.key.is_none() {
            if let Some(_key) = &key {
                // if let Some(_) = extract_data_type_from_token(&_key.tokenize()) {
                self.key = key;
                // } else {
                //     return Err((format!("Invalid data type \"{}\"", _key), ErrType::Syntax));
                // }
            } else {
                return Err(("Expecting key".to_string(), ErrType::Missing));
            }
        } else if self.value.is_none() {
            if let Some(_val) = value {
                self.value = Some(_val);
            } else {
                let _key = key.clone().unwrap();
                // if let Some(_) = extract_data_type_from_token(&_key.tokenize()) {
                self.value = Some(MappingValue::Mapping(Box::new(Mapping {
                    key,
                    value: None,
                })));
                // } else {
                //     return Err((format!("Invalid data type \"{}\"", _key), ErrType::Syntax));
                // }
            }
        } else {
            if let Some(ref mut node) = self.value {
                match node {
                    MappingValue::Mapping(_map) => {
                        _map.insert(key, value)?;
                    }
                    _ => (),
                }
            }
        }

        Ok(())
    }
}

pub fn process_mapping(
    combined: &Vec<Token>,
    mapping: &mut Mapping,
    name: &mut String,
) -> Result<(), (String, ErrType)> {
    let mut state = MappingState::None;
    let mut pad = 0;
    let mut nested_count = 0;

    /* CONSTRUCT DATA */
    let mut is_array = false;
    let mut r#type = String::new();
    let mut size: Option<String> = None;
    //
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
                        format!("Invalid variant declaration \"{}\"", combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::CloseParenthesis => {
                nested_count -= 1;
                if let MappingState::Value = state {
                    state = MappingState::CloseParenthesisIdentifier;
                } else if let MappingState::CloseParenthesisIdentifier = state {
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::Equals => {
                if let MappingState::Key = state {
                    state = MappingState::Assign;
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::Gt => {
                if let MappingState::Assign = state {
                    state = MappingState::Gt;
                } else {
                    return Err((
                        format!("Invalid variant declaration \"{}\"", combined.to_string()),
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
                        if let Some(_slc_index) = slc {
                            pad = index + _slc_index;
                            process_type(
                                &combined[index..index + _slc_index].to_vec().strip_spaces(),
                                &mut key,
                                combined,
                            )?;
                        } else {
                            return Err((
                                format!("=> \"{}\"", combined.to_string()),
                                ErrType::Unexpected,
                            ));
                        }
                    } else {
                        key.push_str(&n.to_string());
                    }

                    mapping.insert(Some(key), None)?;
                    state = MappingState::Key;
                } else if let MappingState::Gt = state {
                    let peek_next = combined.iter().collect::<Vec<_>>();

                    if let Some(_next) = peek_next.get(index + 4) {
                        if combined[index..index + 4].contains(&Token::OpenSquareBracket) {
                            is_array = true;
                            if combined[index..index + 4].contains(&Token::Dot) {
                                if let Token::OpenSquareBracket = combined[index + 3] {
                                    let backward_slice = &combined[index..index + 3];
                                    process_type(backward_slice, &mut r#type, combined)?;
                                    let sliced = &combined[index + 4..];

                                    let size_definition = sliced
                                        .iter()
                                        .position(|pred| *pred == Token::CloseSquareBracket);

                                    if let Some(_sz) = size_definition {
                                        pad = _sz + index + 5;
                                        size = process_size(combined, index + 3, _sz)?;
                                    } else {
                                        return Err((
                                            format!("] \"{}\"", combined.to_string()),
                                            ErrType::Missing,
                                        ));
                                    }
                                } else {
                                    return Err((
                                        format!(
                                            "Invalid variant declaration \"{}\"",
                                            combined.to_string()
                                        ),
                                        ErrType::Syntax,
                                    ));
                                }
                            } else {
                                if let Token::OpenSquareBracket = combined[index + 1] {
                                    let backward_slice = &combined[index..index + 1];
                                    process_type(backward_slice, &mut r#type, combined)?;
                                    let sliced = &combined[index + 2..];
                                    let size_definition = sliced
                                        .iter()
                                        .position(|pred| *pred == Token::CloseSquareBracket);

                                    if let Some(_sz) = size_definition {
                                        size = process_size(combined, index + 1, _sz)?;
                                        pad = _sz + index + 3;
                                    } else {
                                        return Err((
                                            format!("] \"{}\"", combined.to_string()),
                                            ErrType::Missing,
                                        ));
                                    }
                                } else {
                                    return Err((
                                        format!(
                                            "Invalid variant declaration \"{}\"",
                                            combined.to_string()
                                        ),
                                        ErrType::Syntax,
                                    ));
                                }
                            }
                        } else {
                            if combined[index..index + 3].contains(&Token::Dot) {
                                if let Token::Dot = combined[index + 1] {
                                    let backward_slice = &combined[index..index + 3];
                                    process_type(backward_slice, &mut r#type, combined)?;
                                    pad = index + 3;
                                } else {
                                    return Err((
                                        format!(
                                            "Invalid variant declaration \"{}\"",
                                            combined.to_string()
                                        ),
                                        ErrType::Syntax,
                                    ));
                                }
                            } else {
                                let backward_slice = &combined[index..index + 1];
                                process_type(backward_slice, &mut r#type, combined)?;
                            }
                        }
                    } else {
                        return Err((
                            format!("Invalid variant declaration \"{}\"", combined.to_string()),
                            ErrType::Syntax,
                        ));
                    }

                    state = MappingState::Value;

                    mapping.insert(
                        None,
                        Some(MappingValue::Raw(ReturnValue {
                            r#type: r#type.clone(),
                            size: size.clone(),
                            is_array,
                        })),
                    )?
                } else if let MappingState::CloseParenthesisIdentifier = state {
                    if nested_count == 0 {
                        if let Token::Identifier(identifier) = n {
                            name.push_str(identifier)
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
                        format!("Invalid ddvariant declaration \"{}\"", combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::Space | Token::SemiColon => {}
            _ => {
                return Err((
                    format!("Invalid variant declaration \"{}\"", combined.to_string()),
                    ErrType::Syntax,
                ))
            }
        }
    }

    Ok(())
}
