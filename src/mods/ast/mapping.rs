use crate::mods::{errors::error::ErrType, utils::types::visibility::Visibility};

#[derive(Debug)]
pub enum MappingState {
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
pub struct MappingAST {
    pub header: MappingHeader,
    pub map: Mapping,
}
#[derive(Debug)]
pub struct MappingReturnValue {
    pub r#type: String,
    pub array_size: Option<String>,
    pub is_array: bool,
    pub payable: bool,
}

#[derive(Debug)]
pub enum MappingValue {
    Mapping(Box<Mapping>),
    Raw(MappingReturnValue),
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

    pub fn get_return_type(&self) -> Option<&MappingReturnValue> {
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
