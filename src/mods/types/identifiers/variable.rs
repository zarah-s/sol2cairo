use crate::mods::{
    constants::constants::FILE_PATH,
    functions::helpers::global::{get_env_vars, validate_identifier},
    types::{
        compiler_errors::{CompilerError, ErrType, SyntaxError},
        line_descriptors::LineDescriptions,
        token::{Mutability, TTokenTrait, TVecExtension, Token, Visibility},
    },
};
#[derive(Debug)]

enum TypeCast {
    Value(Box<VariableValue>),
    Cast(Box<TypeCast>),
}

#[derive(Debug)]

enum StringVariable {
    Literal(String),
    TypeCast(Box<VariableValue>),
}

#[derive(Debug)]

struct StringValue {
    pub value: StringVariable,
    pub then: Option<Box<VariableValue>>,
}
#[derive(Debug)]

enum IntegerVariable {
    Literal(String),
    TypeCast {
        size: Option<u16>,
        value: Box<VariableValue>,
    },
}
#[derive(Debug)]

struct IntegerValue {
    pub value: IntegerVariable,
    pub then: Option<Box<VariableValue>>,
}
#[derive(Debug)]

enum BytesVariable {
    Literal(String),
    TypeCast {
        size: Option<u16>,
        value: Box<VariableValue>,
    },
}

#[derive(Debug)]
struct BytesValue {
    pub value: BytesVariable,
    pub then: Option<Box<VariableValue>>,
}

#[derive(Debug)]

enum AddressVariable {
    Literal(String),
    TypeCast(Box<VariableValue>),
}

#[derive(Debug)]

struct AddressValue {
    pub value: AddressVariable,
    pub then: Option<Box<VariableValue>>,
}

#[derive(Debug)]

enum BooleanValue {
    Literal(String),
    TypeCast(TypeCast),
}
#[derive(Debug)]

enum ExpressionValue {
    Ternary(String),
    Math(String),
}
#[derive(Debug)]

enum FunctionValueType {
    Global,
    Defined,
}
#[derive(Debug)]

struct ContractInstanceValue {
    pub identifier: String,
    pub arguments: Vec<VariableValue>,
}
#[derive(Debug)]

struct ContractFunctionValue {
    pub identifier: String,
    pub arguments: Vec<VariableValue>,
    pub function_identifier: String,
    pub function_args: Vec<VariableValue>,
}
#[derive(Debug)]

enum Contractvalue {
    ContractFunctionValue(ContractFunctionValue),
    ContractInstanceValue(ContractInstanceValue),
}

#[derive(Debug)]

struct FunctionVariable {
    pub identifier: String,
    pub arguments: Option<Vec<VariableValue>>,
    pub r#type: FunctionValueType,
}
#[derive(Debug)]

struct FunctionValue {
    pub value: FunctionVariable,
    pub then: Option<Box<VariableValue>>,
}
#[derive(Debug)]

struct VariantValue {
    pub identifier: String,
    pub variants: Vec<String>,
}
#[derive(Debug)]

struct StructInstanceValue {
    pub identifier: String,
    pub variants: Vec<[String; 2]>,
}

#[derive(Debug)]

enum StructValue {
    Instance(StructInstanceValue),
    Variant(VariantValue),
}

#[derive(Debug)]

struct InstanceValue {
    pub r#type: String,
    pub length: String,
}

#[derive(Debug)]

enum NestType {
    Variant,
    Method,
}

#[derive(Debug)]

struct NestedValue {
    pub r#type: NestType,
    pub value: Box<VariableValue>,
}

#[derive(Debug)]
enum VariableValue {
    StringValue(StringValue),
    ArrayValue(Vec<VariableValue>),
    IntegerValue(IntegerValue),
    BytesValue(BytesValue),
    AddressValue(AddressValue),
    BooleanValue(BooleanValue),
    FunctionValue(FunctionValue),
    StructValue(StructValue),
    ExpressionValue(ExpressionValue),
    StructOrFunctionValue(FunctionValue),
    LibOrStructOrEnumValue(VariantValue),
    MappingValue(VariantValue),
    GlobalVarValue(VariantValue),
    IdentifierValue(String),
    Context(Box<VariableValue>),
    Contractvalue(Contractvalue),
    InstanceValue(InstanceValue),
    NestedValue(NestedValue),
    None,
}

impl VariableValue {
    fn add_method(&mut self, new: VariableValue) {
        match self {
            Self::None => {
                *self = new;
            }
            _ => {
                if self.access_then().is_none() {
                    *self.access_then() = Some(Box::new(new));
                } else {
                    VariableValue::od(&mut self.access_then().as_mut().unwrap(), new);
                }
            }
        }
    }

    fn od(oth: &mut Box<VariableValue>, new: VariableValue) {
        match **oth {
            VariableValue::None => *oth = Box::new(new),
            _ => {
                if oth.access_then().is_none() {
                    *oth.access_then().as_mut().unwrap() = Box::new(new);
                } else {
                    VariableValue::od(&mut oth.access_then().as_mut().unwrap(), new)
                }
            }
        }
    }
    fn access_then(&mut self) -> &mut Option<Box<VariableValue>> {
        match self {
            VariableValue::FunctionValue(value) => &mut value.then,
            _ => {
                panic!("Detected non method type")
            }
        }
    }
}

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

    for lexem in lexems {
        let mut combined: Vec<Token> = Vec::new();
        for lex in lexem {
            for token in lex.data {
                match token {
                    Token::SemiColon => {
                        combined.push(token);
                        process_var_construct(&combined, lex.line).unwrap_or_else(
                            |(err, err_type): (String, ErrType)| {
                                match err_type {
                                    ErrType::Missing => CompilerError::SyntaxError(
                                        SyntaxError::MissingToken(&format!(
                                            "{} for identifier {}",
                                            err,
                                            combined.to_string()
                                        )),
                                    )
                                    .throw_with_file_info(
                                        &std::env::var(FILE_PATH).unwrap(),
                                        lex.line,
                                    ),

                                    ErrType::Syntax => CompilerError::SyntaxError(
                                        SyntaxError::SyntaxError(&format!(
                                            "{} for identifier {}",
                                            err,
                                            combined.to_string()
                                        )),
                                    )
                                    .throw_with_file_info(
                                        &std::env::var(FILE_PATH).unwrap(),
                                        lex.line,
                                    ),
                                    ErrType::Unexpected => CompilerError::SyntaxError(
                                        SyntaxError::UnexpectedToken(&format!(
                                            "{} for identifier {}",
                                            err,
                                            combined.to_string()
                                        )),
                                    )
                                    .throw_with_file_info(
                                        &std::env::var(FILE_PATH).unwrap(),
                                        lex.line,
                                    ),
                                }
                                unreachable!()
                            },
                        );
                    }
                    _ => combined.push(token),
                }
            }
        }
    }

    variables
}

fn process_var_construct(combined: &Vec<Token>, line: i32) -> Result<(), (String, ErrType)> {
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
                        .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), 0)
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

    if !raw_value.is_empty() {
        let processed = process_variable_value(raw_value, line);
        println!("{:#?}", processed);
    }

    Ok(())
}

fn process_variable_value(raw_value: Vec<Token>, line: i32) -> VariableValue {
    if raw_value.strip_spaces().is_empty() {
        CompilerError::SyntaxError(SyntaxError::SyntaxError(""))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
    }
    match &raw_value.strip_spaces()[0] {
        Token::Identifier(_identifier) => {
            if _identifier.starts_with("\"") || _identifier.starts_with("'") {
                /* QUOTATION VALIDATIONS */

                if raw_value.strip_spaces().len() != 1 {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Mismatch closing string. Expecting ; but found {}",
                        raw_value.strip_spaces().get(1).unwrap().to_string()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                }
                if _identifier.starts_with("\"") && !_identifier.ends_with("\"") {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Mismatch closing string. Expecting \" but found {}",
                        _identifier.chars().last().unwrap()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                }
                if _identifier.starts_with("'") && !_identifier.ends_with("'") {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Mismatch closing string. Expecting \" but found {}",
                        _identifier.chars().last().unwrap()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                }

                let val = &_identifier[1.._identifier.len() - 1];
                /* VALUE HERE */

                let variable_value = VariableValue::StringValue(StringValue {
                    value: StringVariable::Literal(val.to_string()),
                    then: None,
                });
                return variable_value;
            } else if raw_value.strip_spaces().len() == 1 {
                if let Ok(_val) = _identifier.parse::<usize>() {
                    let variable_value = VariableValue::IntegerValue(IntegerValue {
                        value: IntegerVariable::Literal(_identifier.to_owned()),
                        then: None,
                    });
                    return variable_value;
                } else {
                    let variable_value = VariableValue::IdentifierValue(_identifier.to_owned());
                    return variable_value;
                }
            } else if raw_value.strip_spaces()[1] == Token::OpenParenthesis {
                if *raw_value.strip_spaces().last().unwrap() != Token::CloseParenthesis {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                } else {
                    let raw_args = &raw_value[2..raw_value.len() - 1];

                    if raw_args.is_empty() {
                        let variable_value = VariableValue::FunctionValue(FunctionValue {
                            value: FunctionVariable {
                                arguments: None,
                                identifier: _identifier.to_string(),
                                r#type: FunctionValueType::Defined,
                            },
                            then: None,
                        });
                        return variable_value;
                    } else {
                        let mut arguments: Vec<VariableValue> = Vec::new();
                        let splitted_args = raw_args
                            .split(|pred| *pred == Token::Coma)
                            .collect::<Vec<_>>();

                        for split in splitted_args {
                            let construct = process_variable_value(split.to_vec(), line);
                            arguments.push(construct);
                        }

                        let variable_value = VariableValue::FunctionValue(FunctionValue {
                            value: FunctionVariable {
                                arguments: Some(arguments),
                                identifier: _identifier.to_string(),
                                r#type: FunctionValueType::Defined,
                            },
                            then: None,
                        });

                        return variable_value;
                    }
                }
            }
        }

        Token::String => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = VariableValue::StringValue(StringValue {
                value: StringVariable::TypeCast(Box::new(process_variable_value(
                    cast_value.to_vec(),
                    line,
                ))),
                then: nested,
            });
            return variable_value;
        }
        Token::Bytes(_size) => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = VariableValue::BytesValue(BytesValue {
                value: BytesVariable::TypeCast {
                    size: _size.to_owned(),
                    value: Box::new(process_variable_value(cast_value, line)),
                },
                then: nested,
            });

            return variable_value;
        }
        Token::Address => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = VariableValue::AddressValue(AddressValue {
                value: AddressVariable::TypeCast(Box::new(process_variable_value(
                    cast_value.to_vec(),
                    line,
                ))),
                then: nested,
            });
            return variable_value;
        }
        Token::Uint(_size) | Token::Int(_size) => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = VariableValue::IntegerValue(IntegerValue {
                value: IntegerVariable::TypeCast {
                    size: _size.to_owned(),
                    value: Box::new(process_variable_value(cast_value.to_vec(), line)),
                },
                then: nested,
            });
            return variable_value;
        }

        Token::OpenParenthesis => {
            if *raw_value.strip_spaces().last().unwrap() != Token::CloseParenthesis {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                    "Expecting ) but got {}",
                    raw_value.strip_spaces().last().unwrap().to_string()
                )))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
            }
            let cast_value = &raw_value.strip_spaces()[1..raw_value.strip_spaces().len() - 1];
            let variable_value = process_variable_value(cast_value.to_vec(), line);
            return VariableValue::Context(Box::new(variable_value));
        }
        _ => {}
    }
    VariableValue::None
}

fn process_type_cast(raw_value: Vec<Token>, line: i32) -> (Vec<Token>, Option<Box<VariableValue>>) {
    /* VALIDATION CHECKS */
    if raw_value.strip_spaces()[1] != Token::OpenParenthesis {
        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
            "Expecting ( but got {}",
            raw_value.strip_spaces()[1].to_string()
        )))
        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
    }

    if *raw_value.strip_spaces().last().unwrap() != Token::CloseParenthesis {
        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
            "Expecting ) but got {}",
            raw_value.strip_spaces().last().unwrap().to_string()
        )))
        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
    }
    let mut open_paren = 1;
    let mut close_index = 1;

    for tkn in &raw_value.strip_spaces()[2..] {
        match tkn {
            Token::OpenParenthesis => open_paren += 1,
            Token::CloseParenthesis => open_paren -= 1,
            _ => {}
        }
        close_index += 1;
        if open_paren == 0 {
            break;
        }
    }
    let mut nest: VariableValue = VariableValue::None;
    if raw_value.strip_spaces().get(close_index + 1).is_some() {
        match raw_value.strip_spaces()[close_index + 1] {
            Token::Dot => {
                let mut methods = Vec::new();
                let mut nest_open_paren = 0;
                let mut combined = Vec::new();
                let methods_slice = raw_value.strip_spaces();
                for tkn in &methods_slice[close_index + 1..] {
                    combined.push(tkn.to_owned());
                    match tkn {
                        Token::OpenParenthesis => {
                            nest_open_paren += 1;
                        }
                        Token::CloseParenthesis => {
                            nest_open_paren -= 1;
                            if nest_open_paren == 0 {
                                methods.push(combined.to_owned());
                                combined.clear();
                            }
                        }
                        _ => {}
                    }
                }

                if methods.is_empty() {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Unexpected ."))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                } else {
                    for method in methods {
                        if method.is_empty() {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError("Unexpected ."))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        } else {
                            match method[0] {
                                Token::Dot => nest
                                    .add_method(process_variable_value(method[1..].to_vec(), line)),
                                _ => {
                                    panic!("NOT A METHOD FOR PROCESS TYPE_CAST IN VARIABLE.RS")
                                }
                            }
                        }
                    }
                }
            }
            Token::CloseParenthesis => {}
            _ => {
                panic!("Unexpected panic in variable.rs for process_type_cast")
            }
        }
    }

    let cast_value = &raw_value.strip_spaces()[2..close_index];
    (
        cast_value.to_vec(),
        if let VariableValue::None = nest {
            None
        } else {
            Some(Box::new(nest))
        },
    )
}
