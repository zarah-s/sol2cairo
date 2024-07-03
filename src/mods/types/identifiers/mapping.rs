use crate::mods::{
    functions::{controllers::parser::ErrType, helpers::global::extract_data_type_from_token},
    types::token::{StringExtension, Token},
};

#[derive(Debug)]
pub struct MappingIdentifier {
    pub identifier: String,
    pub map: Mapping,
    pub visibility: Token,
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
                if let Some(_) = extract_data_type_from_token(&_key.tokenize()) {
                    self.key = key;
                } else {
                    return Err((format!("Invalid data type \"{}\"", _key), ErrType::Syntax));
                }
            } else {
                return Err(("Expecting key".to_string(), ErrType::Missing));
            }
        } else if self.value.is_none() {
            if let Some(_val) = value {
                self.value = Some(_val);
            } else {
                let _key = key.clone().unwrap();
                if let Some(_) = extract_data_type_from_token(&_key.tokenize()) {
                    self.value = Some(MappingValue::Mapping(Box::new(Mapping {
                        key,
                        value: None,
                    })));
                } else {
                    return Err((format!("Invalid data type \"{}\"", _key), ErrType::Syntax));
                }
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
