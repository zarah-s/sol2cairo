use crate::mods::{
    constants::constants::FILE_PATH,
    functions::helpers::global::{get_env_vars, validate_identifier},
    types::{
        compiler_errors::{CompilerError, ErrType, SyntaxError},
        line_descriptors::LineDescriptions,
        token::{Mutability, TStringExtension, TTokenTrait, TVecExtension, Token, Visibility},
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

enum BooleanVariable {
    Literal(String),
    TypeCast(Box<VariableValue>),
}
#[derive(Debug)]

struct BooleanValue {
    pub value: BooleanVariable,
    pub then: Option<Box<VariableValue>>,
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
enum ArgumentType {
    Positional(VariableValue),
    Named { key: String, value: VariableValue },
}
#[derive(Debug)]

struct FunctionVariable {
    pub identifier: String,
    pub arguments: Option<Vec<ArgumentType>>,
    pub r#type: FunctionValueType,
}
#[derive(Debug)]

struct FunctionValue {
    pub value: FunctionVariable,
    pub then: Option<Box<VariableValue>>,
}
#[derive(Debug)]
struct IdentifierValue {
    pub value: String,
    pub then: Option<Box<VariableValue>>,
}
// #[derive(Debug)]

// struct VariantValue {
//     pub identifier: String,
//     pub variants: Vec<String>,
// }
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
struct ArrayValue {
    pub variants: Vec<VariableValue>,
    pub then: Option<Box<VariableValue>>,
}

#[derive(Debug)]
struct VariantValue {
    pub variant: Box<VariableValue>,
    pub then: Option<Box<VariableValue>>,
}

#[derive(Debug)]
enum VariableValue {
    StringValue(StringValue),
    ArrayValue(ArrayValue),
    IntegerValue(IntegerValue),
    BytesValue(BytesValue),
    AddressValue(AddressValue),
    BooleanValue(BooleanValue),
    FunctionValue(FunctionValue),
    VariantValue(VariantValue),
    // StructValue(StructValue),
    ExpressionValue(ExpressionValue),
    // StructOrFunctionValue(FunctionValue),
    // LibOrStructOrEnumValue(VariantValue),
    // MappingValue(VariantValue),
    // GlobalVarValue(VariantValue),
    IdentifierValue(IdentifierValue),
    Context {
        value: Box<VariableValue>,
        then: Option<Box<VariableValue>>,
    },
    // Contractvalue(Contractvalue),
    InstanceValue(InstanceValue),
    // NestedValue(NestedValue),
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
            if _identifier.tokenize().is_string_literal() {
                /* QUOTATION VALIDATIONS */

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

                if raw_value.strip_spaces().len() != 1 {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Cannot call method on literal {}",
                        raw_value.to_string()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }

                let variable_value = VariableValue::StringValue(StringValue {
                    value: StringVariable::Literal(val.to_string()),
                    then: None,
                });
                return variable_value;
            } else if raw_value.strip_spaces().len() == 1 {
                if _identifier.tokenize().is_integer_literal() {
                    if raw_value.strip_spaces().len() != 1 {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "Cannot call method on literal {}",
                            raw_value.to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }

                    let variable_value = VariableValue::IntegerValue(IntegerValue {
                        value: IntegerVariable::Literal(_identifier.to_owned()),
                        then: None,
                    });
                    return variable_value;
                } else {
                    let variable_value = VariableValue::IdentifierValue(IdentifierValue {
                        value: _identifier.to_owned(),
                        then: None,
                    });
                    return variable_value;
                }
            } else if raw_value.strip_spaces().len() > 1
                && raw_value.strip_spaces()[1] == Token::OpenParenthesis
            {
                /* PROCESSING FUNCTION OR METHOD CALL */

                let strip_data = &raw_value.strip_spaces()[1..];

                let mut paren = 0;
                let mut stop_index = 0;
                for _strip in strip_data {
                    match _strip {
                        Token::OpenParenthesis => {
                            paren += 1;
                        }
                        Token::CloseParenthesis => {
                            paren -= 1;
                            if paren == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    stop_index += 1;
                }

                if paren != 0 {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
                let mut nested = VariableValue::None;

                let method_data = &strip_data[stop_index + 1..];

                if !method_data.is_empty() {
                    match method_data[0] {
                        Token::Dot => {
                            nested =
                                process_variable_value(strip_data[stop_index + 2..].to_vec(), line)
                        }
                        Token::OpenSquareBracket => {
                            nested =
                                process_variable_value(strip_data[stop_index + 1..].to_vec(), line);
                            if let VariableValue::ArrayValue(_value) = &nested {
                                /* VALIDATIONS */
                                if _value.variants.len() != 1 {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unprocessible entity",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                                }

                                if let Some(_) = _value.then {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unprocessible entity",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }
                            }
                        }
                        _ => CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "Unprocessible entity {}",
                            raw_value.to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line),
                    }
                }

                let raw_args = &strip_data[1..stop_index];

                if raw_args.is_empty() {
                    let variable_value = VariableValue::FunctionValue(FunctionValue {
                        value: FunctionVariable {
                            arguments: None,
                            identifier: _identifier.to_string(),
                            r#type: FunctionValueType::Defined,
                        },
                        then: if let VariableValue::None = nested {
                            None
                        } else {
                            Some(Box::new(nested))
                        },
                    });
                    return variable_value;
                } else {
                    let mut arguments: Vec<ArgumentType> = Vec::new();

                    match raw_args[0] {
                        Token::OpenBraces => {
                            /* VALIDATION */
                            if *raw_args.last().unwrap() != Token::CloseBraces {
                                CompilerError::SyntaxError(SyntaxError::MissingToken("}"))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }
                            let named_args = &raw_args[1..raw_args.len() - 1];
                            if named_args.is_empty() {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                    "Unprocessible entity for {}",
                                    raw_value.to_string()
                                )))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }

                            let splitted_named_args = named_args
                                .split(|pred| *pred == Token::Coma)
                                .collect::<Vec<_>>();

                            for _split in splitted_named_args {
                                if _split.is_empty() {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                        "Unprocessible entity for {}",
                                        raw_value.to_string()
                                    )))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }

                                let key_value_split = _split
                                    .split(|pred| *pred == Token::Colon)
                                    .collect::<Vec<_>>();

                                /* VALIDATIONS */
                                if key_value_split.len() != 2 {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                        "Unprocessible entity for {}",
                                        raw_value.to_string()
                                    )))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }

                                if key_value_split[0].len() != 1 {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                        "Unprocessible entity for {}",
                                        raw_value.to_string()
                                    )))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }

                                if key_value_split[1].is_empty() {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                        "Unprocessible entity for {}",
                                        raw_value.to_string()
                                    )))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }

                                let mut key = String::new();
                                let value =
                                    process_variable_value(key_value_split[1].to_vec(), line);

                                if let VariableValue::None = value {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                        "Unprocessible entity for {}",
                                        raw_value.to_string()
                                    )))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }

                                match &key_value_split[0][0] {
                                    Token::Identifier(_key) => {
                                        key.push_str(&_key);
                                    }
                                    _ => {
                                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                            &format!(
                                                "Unprocessible entity for {}",
                                                raw_value.to_string()
                                            ),
                                        ))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        );
                                    }
                                }

                                arguments.push(ArgumentType::Named { key, value });
                            }
                        }
                        _ => {
                            let splitted_args = raw_args
                                .split(|pred| *pred == Token::Coma)
                                .collect::<Vec<_>>();

                            for split in splitted_args {
                                if split.is_empty() {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unprocessible entity",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }

                                if split.to_vec().strip_spaces().first().unwrap().is_symbol() {
                                    match split.to_vec().strip_spaces().first().unwrap() {
                                        Token::Plus | Token::Minus | Token::OpenParenthesis => {}
                                        _ => {
                                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                                &format!(
                                                    "Unprocessible entity {}",
                                                    split
                                                        .to_vec()
                                                        .strip_spaces()
                                                        .first()
                                                        .unwrap()
                                                        .to_string()
                                                ),
                                            ))
                                            .throw_with_file_info(
                                                &get_env_vars(FILE_PATH).unwrap(),
                                                line,
                                            );
                                        }
                                    }
                                }
                                let construct = process_variable_value(split.to_vec(), line);
                                arguments.push(ArgumentType::Positional(construct));
                            }
                        }
                    }
                    let variable_value = VariableValue::FunctionValue(FunctionValue {
                        value: FunctionVariable {
                            arguments: Some(arguments),
                            identifier: _identifier.to_string(),
                            r#type: FunctionValueType::Defined,
                        },
                        then: if let VariableValue::None = nested {
                            None
                        } else {
                            Some(Box::new(nested))
                        },
                    });

                    return variable_value;
                }
            } else if raw_value.strip_spaces().len() > 1
                && raw_value.strip_spaces()[1] == Token::Dot
            {
                let methods = &raw_value.strip_spaces()[2..];
                if methods.is_empty() {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }

                let nest = process_variable_value(raw_value.strip_spaces()[2..].to_vec(), line);
                let variable_value = VariableValue::IdentifierValue(IdentifierValue {
                    value: _identifier.to_string(),
                    then: Some(Box::new(nest)),
                });

                return variable_value;
            } else if _identifier.tokenize().is_integer_literal() {
                if raw_value.strip_spaces().len() != 1 {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Cannot call method on literal {}",
                        raw_value.to_string()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
                let variable_value = VariableValue::IntegerValue(IntegerValue {
                    value: IntegerVariable::Literal(_identifier.to_owned()),
                    then: None,
                });
                return variable_value;
            }
        }

        Token::OpenSquareBracket => {
            let mut open_contex = 0;
            let mut iteration = 0;
            for _strip in raw_value.strip_spaces() {
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
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
            let mut nested = VariableValue::None;
            let raw_variants = &raw_value.strip_spaces()[1..iteration];
            let method_data = &raw_value.strip_spaces()[iteration + 1..];
            let mut variants: Vec<VariableValue> = Vec::new();

            if !raw_variants.is_empty() {
                let splits = raw_variants
                    .split(|pred| *pred == Token::Coma)
                    .collect::<Vec<_>>();
                for split in splits {
                    if split.is_empty() {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "Unprocessible entity {}",
                            raw_value.to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                    }
                    variants.push(process_variable_value(split.to_vec(), line));
                }
            }

            if !method_data.is_empty() {
                match method_data[0] {
                    Token::Dot => {
                        nested = process_variable_value(
                            raw_value.strip_spaces()[iteration + 2..].to_vec(),
                            line,
                        )
                    }
                    Token::OpenSquareBracket => {
                        nested = process_variable_value(
                            raw_value.strip_spaces()[iteration + 1..].to_vec(),
                            line,
                        );
                        if let VariableValue::ArrayValue(_value) = &nested {
                            /* VALIDATIONS */
                            if _value.variants.len() != 1 {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Unprocessible entity",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                            }

                            if let Some(_) = _value.then {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Unprocessible entity",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }
                        }
                    }
                    _ => CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Unprocessible entity {}",
                        raw_value.to_string()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line),
                }
            }

            let variable_value = VariableValue::ArrayValue(ArrayValue {
                variants,
                then: if let VariableValue::None = nested {
                    None
                } else {
                    Some(Box::new(nested))
                },
            });

            return variable_value;
        }

        Token::True => {
            if raw_value.strip_spaces().len() > 1 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                    "Cannot have method on boolean type",
                ))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
            let variable_value = VariableValue::BooleanValue(BooleanValue {
                value: BooleanVariable::Literal(Token::True.to_string()),
                then: None,
            });

            return variable_value;
        }

        Token::Bool => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = VariableValue::BooleanValue(BooleanValue {
                value: BooleanVariable::TypeCast(Box::new(process_variable_value(
                    cast_value.to_vec(),
                    line,
                ))),
                then: nested,
            });
            return variable_value;
        }

        Token::Hex => {
            if raw_value.strip_spaces().len() != 2 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }

            if !raw_value.strip_spaces()[1].is_string_literal() {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                    "Expecting string literal but got {}",
                    raw_value.strip_spaces()[1].to_string()
                )))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
            let val = &raw_value.strip_spaces()[1].to_string()
                [1..raw_value.strip_spaces()[1].to_string().len() - 1];

            let variable_value = VariableValue::BytesValue(BytesValue {
                value: BytesVariable::Literal(val.to_string()),
                then: None,
            });

            return variable_value;
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
            let mut open_paren = 1;
            let mut close_index = 0;

            for tkn in &raw_value.strip_spaces()[1..] {
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
            if open_paren != 0 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
            let cast_value = &raw_value.strip_spaces()[1..close_index];
            let mut nest = VariableValue::None;
            process_nested_methods(&raw_value.to_vec(), close_index, &mut nest, line);
            let variable_value = process_variable_value(cast_value.to_vec(), line);
            return VariableValue::Context {
                value: Box::new(variable_value),
                then: if let VariableValue::None = nest {
                    None
                } else {
                    Some(Box::new(nest))
                },
            };
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
    if open_paren != 0 {
        CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
    }
    let mut nest: VariableValue = VariableValue::None;
    process_nested_methods(&raw_value, close_index, &mut nest, line);
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

fn process_nested_methods(
    raw_value: &Vec<Token>,
    close_index: usize,
    nest: &mut VariableValue,
    line: i32,
) {
    if raw_value.strip_spaces().get(close_index + 1).is_some() {
        match raw_value.strip_spaces()[close_index + 1] {
            Token::Dot => {
                let methods_slice = &raw_value.strip_spaces()[close_index + 1..];
                process_raw_methods(methods_slice, line, nest);
            }
            // Token::CloseParenthesis => {}
            _ => {
                panic!("Unexpected panic in variable.rs for process_type_cast")
            }
        }
    }
}

fn process_raw_methods(methods_slice: &[Token], line: i32, nest: &mut VariableValue) {
    let mut methods = Vec::new();
    let mut nest_open_paren = 0;
    let mut combined = Vec::new();
    for tkn in methods_slice {
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
    if nest_open_paren != 0 {
        CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
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
                    Token::Dot => {
                        nest.add_method(process_variable_value(method[1..].to_vec(), line))
                    }
                    _ => {
                        panic!("NOT A METHOD FOR PROCESS TYPE_CAST IN VARIABLE.RS")
                    }
                }
            }
        }
    }
}
