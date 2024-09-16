use crate::mods::{
    constants::constants::FILE_PATH,
    functions::helpers::global::{
        extract_data_type_from_token, get_env_vars, process_size, process_type, validate_identifier,
    },
    types::{
        compiler_errors::{CompilerError, ErrType, SyntaxError},
        token::{TStringExtension, TTokenTrait, TVecExtension, Token, Visibility},
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
pub struct MappingHeader {
    pub identifier: String,
    pub visibility: Visibility,
}

impl MappingHeader {
    pub fn new() -> Self {
        Self {
            identifier: String::new(),
            visibility: Visibility::None,
        }
    }
}

#[derive(Debug)]
pub struct MappingIdentifier {
    pub header: MappingHeader,
    pub map: Mapping,
}
#[derive(Debug)]
pub struct ReturnValue {
    pub r#type: String,
    pub array_size: Option<String>,
    pub is_array: bool,
    pub payable: bool,
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
                self.key = key;
            } else {
                return Err(("Expecting key".to_string(), ErrType::Missing));
            }
        } else if self.value.is_none() {
            if let Some(_val) = value {
                self.value = Some(_val);
            } else {
                let _key = key.clone().unwrap();
                self.value = Some(MappingValue::Mapping(Box::new(Mapping {
                    key,
                    value: None,
                })));
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

    pub fn update_payable_state(&mut self, payable: bool) {
        match &mut self.value {
            Some(_val) => match _val {
                MappingValue::Raw(_raw) => {
                    _raw.payable = payable;
                }
                MappingValue::Mapping(_map) => {
                    _map.update_payable_state(payable);
                }
            },

            _ => {
                // TODO: NOTHING
            }
        }
    }
}

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
    let mut is_array = false;
    let mut r#type = String::new();
    let mut size: Option<String> = None;
    let mut payable = false;
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
                                format!("=> \"{}\"", _combined.to_string()),
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

                                    let mut open_contex = 1;
                                    let mut iteration = 0;
                                    for _strip in sliced {
                                        match _strip {
                                            Token::OpenSquareBracket => {
                                                open_contex += 1;
                                            }
                                            Token::CloseSquareBracket => {
                                                open_contex -= 1;
                                                if open_contex == 0 {
                                                    break;
                                                }
                                            }
                                            _ => {}
                                        }
                                        iteration += 1;
                                    }
                                    if open_contex != 0 {
                                        return Err((
                                            "Uprocessible entity".to_string(),
                                            ErrType::Syntax,
                                        ));
                                    }
                                    if !combined[index + 3 + 1..][..iteration].is_empty() {
                                        size = Some(
                                            combined[index + 3 + 1..][..iteration]
                                                .to_vec()
                                                .to_string(),
                                        );
                                    }
                                    pad = iteration + index + 5;
                                } else {
                                    return Err((
                                        format!(
                                            "Invalid variant declaration \"{}\"",
                                            _combined.to_string()
                                        ),
                                        ErrType::Syntax,
                                    ));
                                }
                            } else {
                                let mut index = index;
                                let backward_slice = &combined[index..index + 1];
                                if let Token::Payable = combined[index + 1] {
                                    payable = true;
                                    index += 1;
                                }
                                if let Token::OpenSquareBracket = combined[index + 1] {
                                    process_type(backward_slice, &mut r#type, combined)?;
                                    let sliced = &combined[index + 2..];

                                    let mut open_contex = 1;
                                    let mut iteration = 0;
                                    for _strip in sliced {
                                        match _strip {
                                            Token::OpenSquareBracket => {
                                                open_contex += 1;
                                            }
                                            Token::CloseSquareBracket => {
                                                open_contex -= 1;
                                                if open_contex == 0 {
                                                    break;
                                                }
                                            }
                                            _ => {}
                                        }
                                        iteration += 1;
                                    }
                                    if open_contex != 0 {
                                        return Err((
                                            "Uprocessible entity".to_string(),
                                            ErrType::Syntax,
                                        ));
                                    }

                                    // if payable {
                                    // panic!(
                                    //     "{:?}",
                                    //     combined[index + 1 + 1..][..iteration].to_vec().to_string()
                                    // );
                                    // }

                                    if !combined[index + 1 + 1..][..iteration].is_empty() {
                                        size = Some(
                                            combined[index + 1 + 1..][..iteration]
                                                .to_vec()
                                                .to_string(),
                                        );
                                    }

                                    pad = iteration + index + 3;
                                } else {
                                    // println!("{:?}", combined[index + 1]);
                                    return Err((
                                        format!(
                                            "Invalid variant declaration \"{}\"",
                                            _combined.to_string()
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
                                            _combined.to_string()
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
                            format!("Invalid variant declaration \"{}\"", _combined.to_string()),
                            ErrType::Syntax,
                        ));
                    }

                    state = MappingState::Value;

                    if payable && r#type.tokenize() != Token::Address {
                        return Err((
                            format!(
                                "Invalid payable for non-address type \"{}\"",
                                _combined.to_string()
                            ),
                            ErrType::Syntax,
                        ));
                    }
                    mapping.insert(
                        None,
                        Some(MappingValue::Raw(ReturnValue {
                            r#type: r#type.clone(),
                            array_size: size.clone(),
                            is_array,
                            payable,
                        })),
                    )?;

                    // println!("{:?}", mapping);
                    // if payable {
                    //     panic!("{:?}", mapping)
                    // }
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

            Token::Payable => {
                if let MappingState::Value = state {
                    if let Token::Address = combined[index - 1] {
                        // payable = true;
                        mapping.update_payable_state(true);
                        // panic!("sdfasd")
                    } else {
                        return Err((
                            format!("Invalidd variant declaration \"{}\"", _combined.to_string()),
                            ErrType::Syntax,
                        ));
                    }
                } else {
                    return Err((
                        format!("Invalidd variant declaration \"{}\"", _combined.to_string()),
                        ErrType::Syntax,
                    ));
                }
            }
            Token::Space | Token::SemiColon => {}
            _ => {
                return Err((
                    format!("Invalidd variant declaration \"{}\"", _combined.to_string()),
                    ErrType::Syntax,
                ))
            }
        }
    }

    Ok(())
}
