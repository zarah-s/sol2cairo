use crate::mods::{
    constants::constants::FILE_PATH,
    errors::error::{CompilerError, SyntaxError},
    utils::{
        functions::global::{get_env_vars, validate_identifier},
        types::value::{
            AddressValue, AddressVariable, ArgumentType, ArrayValue, BooleanValue, BooleanVariable,
            BytesValue, BytesVariable, ExpressionTypes, ExpressionValue, ExpressionVariable,
            FunctionPTRInvocation, FunctionValue, FunctionValueType, FunctionVariable,
            IdentifierValue, InstanceValue, InstanceVariable, IntegerValue, IntegerVariable,
            KeywordValue, PayableValue, StringValue, StringVariable, Value, VariantValue,
        },
    },
};

use crate::mods::lexer::{
    lexer::{TStringExtension, TTokenTrait, TVecExtension},
    tokens::Token,
};

pub fn parse_value(raw_value: Vec<Token>, line: i32) -> Value {
    if raw_value.strip_spaces().is_empty() {
        CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
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

                let variable_value = Value::StringValue(StringValue {
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

                    let variable_value = Value::IntegerValue(IntegerValue {
                        value: IntegerVariable::Literal(_identifier.to_owned()),
                        then: None,
                    });
                    return variable_value;
                } else {
                    let variable_value = Value::IdentifierValue(IdentifierValue {
                        value: _identifier.to_owned(),
                        then: None,
                    });
                    return variable_value;
                }
            } else if raw_value.strip_spaces().len() > 1
                && raw_value.strip_spaces()[1] == Token::OpenParenthesis
            {
                validate_identifier(&_identifier).unwrap_or_else(|err| {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                });

                /* PROCESSING FUNCTION OR METHOD CALL */
                let strip_data = &raw_value.strip_spaces()[1..];

                let mut paren = 0;
                let mut iteration = 0;
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
                    iteration += 1;
                }

                if paren != 0 {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
                let mut nested = Value::None;

                let method_data = &strip_data[iteration + 1..];

                if !method_data.is_empty() {
                    nested = process_method_data_with_possible_fn_ptr_invocation(
                        || {},
                        &strip_data.to_vec(),
                        iteration,
                        line,
                        method_data,
                    );
                }

                let raw_args = &strip_data[1..iteration];

                if raw_args.is_empty() {
                    let variable_value = Value::FunctionValue(FunctionValue {
                        value: FunctionVariable {
                            arguments: None,
                            identifier: _identifier.to_string(),
                            r#type: FunctionValueType::Defined,
                        },
                        then: if let Value::None = nested {
                            None
                        } else {
                            Some(Box::new(nested))
                        },
                    });
                    return variable_value;
                } else {
                    let arguments = process_args(&raw_value, raw_args, line);

                    let variable_value = Value::FunctionValue(FunctionValue {
                        value: FunctionVariable {
                            arguments: Some(arguments),
                            identifier: _identifier.to_string(),
                            r#type: FunctionValueType::Defined,
                        },
                        then: if let Value::None = nested {
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
                validate_identifier(&_identifier).unwrap_or_else(|err| {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                });
                let methods = &raw_value.strip_spaces()[2..];
                if methods.is_empty() {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }

                let nest = parse_value(raw_value.strip_spaces()[2..].to_vec(), line);
                let variable_value = Value::IdentifierValue(IdentifierValue {
                    value: _identifier.to_string(),
                    then: Some(Box::new(Value::Dot(Box::new(nest)))),
                });

                return variable_value;
            } else if raw_value.strip_spaces().len() > 1
                && raw_value.strip_spaces()[1] == Token::OpenSquareBracket
            {
                validate_identifier(&_identifier).unwrap_or_else(|err| {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                });
                let methods = &raw_value.strip_spaces()[1..];
                if methods.is_empty() {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }

                let nest = parse_value(raw_value.strip_spaces()[1..].to_vec(), line);
                let variable_value = Value::IdentifierValue(IdentifierValue {
                    value: _identifier.to_string(),
                    then: Some(Box::new(nest)),
                });
                return variable_value;
            } else if raw_value.strip_spaces().len() > 1 {
                if _identifier.tokenize().is_integer_literal()
                    && raw_value.strip_spaces().len() == 2
                {
                    match raw_value.strip_spaces()[1] {
                        Token::Seconds
                        | Token::Minutes
                        | Token::Hours
                        | Token::Days
                        | Token::Weeks
                        | Token::Years
                        | Token::Wei
                        | Token::Gwei
                        | Token::Szabo
                        | Token::Finney
                        | Token::Ether => {
                            let variable_value = Value::UnitValue(
                                _identifier.to_owned(),
                                raw_value.strip_spaces()[1].to_owned(),
                            );
                            return variable_value;
                        }

                        _ => {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }
                    }
                } else {
                    let variable_value = Value::IdentifierValue(IdentifierValue {
                        value: _identifier.to_string(),
                        then: Some(Box::new(parse_value(
                            raw_value.strip_spaces()[1..].to_vec(),
                            line,
                        ))),
                    });

                    return variable_value;
                }
            } else {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
        }

        Token::Plus | Token::Minus | Token::Multiply | Token::Divide | Token::Modulu => {
            let operation = &raw_value.strip_spaces()[0];
            return process_math_operation(&raw_value, line, operation);
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

            let mut nested = Value::None;
            let raw_variants = &raw_value.strip_spaces()[1..iteration];
            let method_data = &raw_value.strip_spaces()[iteration + 1..];
            let mut variants: Vec<Value> = Vec::new();
            let mut single_variant = None;
            if !raw_variants.is_empty() {
                let splits = raw_variants.to_vec().split_coma();
                if splits.len() == 1 {
                    single_variant = Some(parse_value(splits[0].to_vec(), line));
                } else {
                    for split in splits {
                        if split.is_empty() {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                "Unprocessible entity {}",
                                raw_value.to_string()
                            )))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                        }
                        variants.push(parse_value(split.to_vec(), line));
                    }
                }
            }

            if !method_data.is_empty() {
                nested = process_method_data_with_possible_fn_ptr_invocation(
                    || {
                        if single_variant.is_none() {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                "Unprocessible entity {}",
                                raw_value.to_string()
                            )))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                        }
                    },
                    &raw_value.strip_spaces(),
                    iteration,
                    line,
                    method_data,
                );
            }

            let variable_value = if single_variant.is_none() {
                Value::ArrayValue(ArrayValue {
                    variants,
                    then: if let Value::None = nested {
                        None
                    } else {
                        Some(Box::new(nested))
                    },
                })
            } else {
                Value::VariantValue(VariantValue {
                    variant: Box::new(single_variant.unwrap()),
                    then: if let Value::None = nested {
                        None
                    } else {
                        Some(Box::new(nested))
                    },
                })
            };

            return variable_value;
        }

        Token::QMark => {
            let mut open_context = 1;
            let mut iteration = 0;

            for tkn in &raw_value.strip_spaces()[1..] {
                match tkn {
                    Token::QMark => open_context += 1,
                    Token::Colon => open_context -= 1,
                    _ => {}
                }
                iteration += 1;
                if open_context == 0 {
                    break;
                }
            }
            if open_context != 0 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                    "Missing else statement for ternary operator",
                ))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }

            let else_block = &raw_value.strip_spaces()[iteration + 1..];
            if else_block.is_empty() {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                    "Missing else statement for ternary operator",
                ))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }

            let variable_value = Value::ExpressionValue(ExpressionValue {
                value: ExpressionVariable {
                    r#type: ExpressionTypes::TernaryIf,
                    operand: Box::new(parse_value(
                        raw_value.strip_spaces()[1..iteration].to_vec(),
                        line,
                    )),
                },
                then: Some(Box::new(Value::ExpressionValue(ExpressionValue {
                    value: ExpressionVariable {
                        r#type: ExpressionTypes::TernaryEl,
                        operand: Box::new(parse_value(else_block.to_vec(), line)),
                    },
                    then: None,
                }))),
            });

            return variable_value;
        }

        Token::True | Token::False => {
            let mut nest: Option<Box<Value>> = None;

            if raw_value.strip_spaces().len() > 1 {
                match &raw_value.strip_spaces()[1] {
                    &Token::QMark | &Token::Equals | &Token::And | &Token::Or | &Token::Bang => (),
                    _ => {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Unprocessible entity for boolean type",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                }
                nest = Some(Box::new(parse_value(
                    raw_value.strip_spaces()[1..].to_vec(),
                    line,
                )));
            }

            let variable_value = Value::BooleanValue(BooleanValue {
                value: BooleanVariable::Literal(raw_value.strip_spaces()[0].to_string()),
                then: nest,
            });

            return variable_value;
        }

        Token::Gt | Token::Lt => {
            let stripped = raw_value.strip_spaces();
            if stripped.len() < 2 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }

            if stripped[1] == Token::Equals {
                let variable_value = Value::ExpressionValue(ExpressionValue {
                    value: ExpressionVariable {
                        r#type: if stripped[0] == Token::Gt {
                            ExpressionTypes::GtEq
                        } else {
                            ExpressionTypes::LtEq
                        },
                        operand: Box::new(parse_value(stripped[2..].to_vec(), line)),
                    },
                    then: None,
                });

                return variable_value;
            } else {
                let check_next = &stripped[1];
                if check_next == &stripped[0] {
                    let variable_value = Value::ExpressionValue(ExpressionValue {
                        value: ExpressionVariable {
                            r#type: if stripped[0] == Token::Gt {
                                ExpressionTypes::Shr
                            } else {
                                ExpressionTypes::Shl
                            },
                            operand: Box::new(parse_value(stripped[2..].to_vec(), line)),
                        },
                        then: None,
                    });

                    return variable_value;
                }
                let variable_value = Value::ExpressionValue(ExpressionValue {
                    value: ExpressionVariable {
                        r#type: if stripped[0] == Token::Gt {
                            ExpressionTypes::Gt
                        } else {
                            ExpressionTypes::Lt
                        },
                        operand: Box::new(parse_value(stripped[1..].to_vec(), line)),
                    },
                    then: None,
                });

                return variable_value;
            }
        }

        Token::Equals => {
            let stripped = raw_value.strip_spaces();
            if stripped.len() < 3 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
            if stripped[0] == Token::Equals && stripped[1] == Token::Equals {
                let variable_value = Value::ExpressionValue(ExpressionValue {
                    value: ExpressionVariable {
                        r#type: ExpressionTypes::Eq,
                        operand: Box::new(parse_value(stripped[2..].to_vec(), line)),
                    },
                    then: None,
                });

                return variable_value;
            } else {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
        }

        Token::This | Token::Msg | Token::Block | Token::Tx => {
            let stripped_spaces = raw_value.strip_spaces();
            if stripped_spaces.len() == 1 {
                let variable_value = Value::KeywordValue(KeywordValue {
                    value: stripped_spaces[0].to_owned(),
                    then: None,
                });
                return variable_value;
            } else {
                if let Token::Dot = stripped_spaces[1] {
                    let nest = parse_value(stripped_spaces[2..].to_vec(), line);

                    let variable_value = Value::KeywordValue(KeywordValue {
                        value: stripped_spaces[0].to_owned(),
                        then: Some(Box::new(Value::Dot(Box::new(nest)))),
                    });
                    return variable_value;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }
        }
        Token::Bool => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = Value::BooleanValue(BooleanValue {
                value: BooleanVariable::TypeCast(Box::new(parse_value(cast_value.to_vec(), line))),
                then: nested,
            });
            return variable_value;
        }
        Token::Payable => {
            let (cast_value, nested) = process_type_cast(raw_value, line);
            let payable_value = parse_value(cast_value.to_vec(), line);
            let variable_value = Value::PayableValue(PayableValue {
                value: Box::new(payable_value),
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

            let variable_value = Value::BytesValue(BytesValue {
                value: BytesVariable::Literal(val.to_string()),
                then: None,
            });

            return variable_value;
        }

        Token::Unicode => {
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

            let variable_value = Value::Unicode(val.to_string());

            return variable_value;
        }

        Token::String => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = Value::StringValue(StringValue {
                value: StringVariable::TypeCast(Box::new(parse_value(cast_value.to_vec(), line))),
                then: nested,
            });
            return variable_value;
        }
        Token::Bytes(_size) => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = Value::BytesValue(BytesValue {
                value: BytesVariable::TypeCast {
                    size: _size.to_owned(),
                    value: Box::new(parse_value(cast_value, line)),
                },
                then: nested,
            });

            return variable_value;
        }
        Token::Or | Token::Xor | Token::Not | Token::And | Token::Bang => {
            let stripped_value = raw_value.strip_spaces();

            if stripped_value.len() >= 2 {
                if let Token::Bang = stripped_value[0] {
                    if let Token::Equals = stripped_value[1] {
                        let variable_value = Value::ExpressionValue(ExpressionValue {
                            value: ExpressionVariable {
                                r#type: ExpressionTypes::NotEq,
                                operand: Box::new(parse_value(stripped_value[2..].to_vec(), line)),
                            },
                            then: None,
                        });

                        return variable_value;
                    } else {
                        let operand = parse_value(stripped_value[1..].to_vec(), line);
                        match operand {
                            Value::BooleanValue(_) => {}
                            _ => {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Built-in unary operator ! cannot be applied to type",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }
                        }

                        let variable_value = Value::ExpressionValue(ExpressionValue {
                            value: ExpressionVariable {
                                r#type: ExpressionTypes::LgNot,
                                operand: Box::new(operand),
                            },
                            then: None,
                        });

                        return variable_value;
                    }
                } else {
                    if stripped_value[0] == stripped_value[1] {
                        let variable_value = Value::ExpressionValue(ExpressionValue {
                            value: ExpressionVariable {
                                r#type: match stripped_value[0] {
                                    Token::Or => ExpressionTypes::LgOr,
                                    Token::And => ExpressionTypes::LgAnd,

                                    _ => {
                                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                            &format!(
                                                "Unprocessible entity for {}",
                                                stripped_value[0].to_string()
                                            ),
                                        ))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        );
                                        unreachable!()
                                    }
                                },
                                operand: Box::new(parse_value(stripped_value[2..].to_vec(), line)),
                            },
                            then: None,
                        });

                        return variable_value;
                    } else {
                        let variable_value = Value::ExpressionValue(ExpressionValue {
                            value: ExpressionVariable {
                                r#type: match stripped_value[0] {
                                    Token::Or => ExpressionTypes::BtOr,
                                    Token::And => ExpressionTypes::BtAnd,
                                    Token::Xor => ExpressionTypes::Xor,
                                    Token::Not => ExpressionTypes::BtNot,

                                    _ => {
                                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                            &format!(
                                                "Unprocessible entity for {}",
                                                stripped_value[0].to_string()
                                            ),
                                        ))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        );
                                        unreachable!()
                                    }
                                },
                                operand: Box::new(parse_value(stripped_value[1..].to_vec(), line)),
                            },
                            then: None,
                        });

                        return variable_value;
                    }
                }
            } else {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
        }
        Token::Address => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = Value::AddressValue(AddressValue {
                value: AddressVariable::TypeCast(Box::new(parse_value(cast_value.to_vec(), line))),
                then: nested,
            });
            return variable_value;
        }
        Token::Uint(_size) | Token::Int(_size) => {
            let (cast_value, nested) = process_type_cast(raw_value, line);

            let variable_value = Value::IntegerValue(IntegerValue {
                value: IntegerVariable::TypeCast {
                    size: _size.to_owned(),
                    value: Box::new(parse_value(cast_value.to_vec(), line)),
                },
                then: nested,
            });
            return variable_value;
        }
        Token::New => {
            let stripped_spaces = raw_value.strip_spaces();
            if stripped_spaces.len() < 4 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            } else {
                /* VALIDATION */
                if !stripped_spaces[1].is_data_type() {
                    match &stripped_spaces[1] {
                        Token::Identifier(_identifier) => {
                            if stripped_spaces[1].is_string_literal() {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Unprocessible entity",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }
                            validate_identifier(_identifier).unwrap_or_else(|err| {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            });
                        }
                        _ => {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }
                    }
                }

                match stripped_spaces[2] {
                    Token::OpenParenthesis => match &stripped_spaces[1] {
                        Token::Identifier(_identifier) => {
                            validate_identifier(&_identifier).unwrap_or_else(|err| {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            });
                            return process_contract_like_instance(
                                raw_value,
                                line,
                                &stripped_spaces[1],
                                || {},
                            );
                        }

                        Token::String => {
                            return process_contract_like_instance(
                                raw_value,
                                line,
                                &stripped_spaces[1],
                                || {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Wrong argument count for function call: 0 arguments given but expected 1.",
                                        ))
                                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                },
                            );
                        }

                        Token::Bytes(_size) => {
                            if _size.is_some() {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Contract or array type expected.",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }
                            return process_contract_like_instance(
                                raw_value,
                                line,
                                &stripped_spaces[1],
                                || {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Wrong argument count for function call: Expected 1.",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                },
                            );
                        }
                        _ => {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }
                    },
                    Token::OpenSquareBracket => {
                        let mut variant_open_contex = 0;
                        let mut variant_iteration = 0;

                        for _strip in &stripped_spaces {
                            match _strip {
                                Token::OpenSquareBracket => {
                                    variant_open_contex += 1;
                                }
                                Token::CloseSquareBracket => {
                                    variant_open_contex -= 1;
                                    if variant_open_contex == 0 {
                                        break;
                                    }
                                }
                                _ => {}
                            }
                            variant_iteration += 1;
                        }

                        if variant_open_contex != 0 {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }

                        let variants = &stripped_spaces[3..variant_iteration];
                        if !variants.is_empty() {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }
                        let arg_data = &stripped_spaces[variant_iteration + 1..];
                        if arg_data.is_empty() {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }

                        match arg_data[0] {
                            Token::OpenParenthesis => {
                                let mut arg_open_contex = 0;
                                let mut arg_iteration = 0;

                                for _strip in arg_data {
                                    match _strip {
                                        Token::OpenParenthesis => {
                                            arg_open_contex += 1;
                                        }
                                        Token::CloseParenthesis => {
                                            arg_open_contex -= 1;
                                            if arg_open_contex == 0 {
                                                break;
                                            }
                                        }
                                        _ => {}
                                    }
                                    arg_iteration += 1;
                                }

                                if arg_open_contex != 0 {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Missing )",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }

                                let raw_args = &arg_data[1..arg_iteration];
                                if raw_args.is_empty() {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unprocessible entity. Expecting size",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }
                                if raw_args[0] == Token::OpenBraces {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unprocessible entity",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }

                                let size = parse_value(raw_args.to_vec(), line);
                                let method_data = &arg_data[arg_iteration + 1..];
                                let mut nested = Value::None;

                                if !method_data.is_empty() {
                                    match method_data[0] {
                                        Token::Dot => {
                                            nested = Value::Dot(Box::new(parse_value(
                                                arg_data[arg_iteration + 2..].to_vec(),
                                                line,
                                            )))
                                        }
                                        Token::OpenSquareBracket => {
                                            nested = parse_value(
                                                arg_data[arg_iteration + 1..].to_vec(),
                                                line,
                                            );
                                            if let Value::ArrayValue(_value) = &nested {
                                                /* VALIDATIONS */
                                                if _value.variants.len() != 1 {
                                                    CompilerError::SyntaxError(
                                                        SyntaxError::SyntaxError(
                                                            "Unprocessible entity",
                                                        ),
                                                    )
                                                    .throw_with_file_info(
                                                        &get_env_vars(FILE_PATH).unwrap(),
                                                        line,
                                                    )
                                                }

                                                if let Some(_) = _value.then {
                                                    CompilerError::SyntaxError(
                                                        SyntaxError::SyntaxError(
                                                            "Unprocessible entity",
                                                        ),
                                                    )
                                                    .throw_with_file_info(
                                                        &get_env_vars(FILE_PATH).unwrap(),
                                                        line,
                                                    );
                                                }
                                            }
                                        }
                                        _ => CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                            &format!(
                                                "Unprocessible entity {}",
                                                raw_value.to_string()
                                            ),
                                        ))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        ),
                                    }
                                }

                                let variable_value = Value::InstanceValue(InstanceValue {
                                    value: InstanceVariable {
                                        r#type: stripped_spaces[1].to_string(),
                                        size: Some(Box::new(size)),
                                        arguments: None,
                                    },
                                    then: if let Value::None = nested {
                                        None
                                    } else {
                                        Some(Box::new(nested))
                                    },
                                });

                                return variable_value;
                            }
                            _ => {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Unprocessible entity",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }
                        }
                    }

                    _ => {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Unprocessible entity",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                }
            }
        }
        Token::OpenParenthesis => {
            let mut open_context = 1;
            let mut iteration = 0;

            for tkn in &raw_value.strip_spaces()[1..] {
                match tkn {
                    Token::OpenParenthesis => open_context += 1,
                    Token::CloseParenthesis => open_context -= 1,
                    _ => {}
                }
                iteration += 1;
                if open_context == 0 {
                    break;
                }
            }
            if open_context != 0 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
            let cast_value = &raw_value.strip_spaces()[1..iteration];
            let mut nest = Value::None;
            if raw_value.strip_spaces().get(iteration + 1).is_some() {
                nest = process_method_data_with_possible_fn_ptr_invocation(
                    || {},
                    &raw_value,
                    iteration,
                    line,
                    &raw_value.strip_spaces()[iteration + 1..],
                );
            }
            let variable_value = parse_value(cast_value.to_vec(), line);
            return Value::Context {
                value: Box::new(variable_value),
                then: if let Value::None = nest {
                    None
                } else {
                    Some(Box::new(nest))
                },
            };
        }
        _other => {
            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                "Unprocessibleddd entity. {}",
                raw_value.to_string()
            )))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
        }
    }
    Value::None
}

fn process_type_cast(raw_value: Vec<Token>, line: i32) -> (Vec<Token>, Option<Box<Value>>) {
    /* VALIDATION CHECKS */

    if raw_value.strip_spaces().len() < 2 {
        CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible Entity"))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
    }

    if raw_value.strip_spaces()[1] != Token::OpenParenthesis {
        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
            "Expecting ( but got {}",
            raw_value.strip_spaces()[1].to_string()
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
    let mut nest: Value = Value::None;
    if raw_value.strip_spaces().get(close_index + 1).is_some() {
        nest = process_method_data_with_possible_fn_ptr_invocation(
            || {},
            &raw_value,
            close_index,
            line,
            &raw_value.strip_spaces()[close_index + 1..],
        );
    }
    let cast_value = &raw_value.strip_spaces()[2..close_index];
    (
        cast_value.to_vec(),
        if let Value::None = nest {
            None
        } else {
            Some(Box::new(nest))
        },
    )
}

fn process_args(raw_value: &Vec<Token>, raw_args: &[Token], line: i32) -> Vec<ArgumentType> {
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

            let splitted_named_args = named_args.to_vec().split_coma();

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
                let value = parse_value(key_value_split[1].to_vec(), line);

                if let Value::None = value {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Unprocessible entity for {}",
                        raw_value.to_string()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }

                match &key_value_split[0][0] {
                    Token::Identifier(_key) => {
                        validate_identifier(_key).unwrap_or_else(|err| {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        });
                        key.push_str(&_key);
                    }
                    _ => {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "Unprocessible entity for {}",
                            raw_value.to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                }

                arguments.push(ArgumentType::Named { key, value });
            }
        }
        _ => {
            let splitted_args = raw_args.to_vec().split_coma();

            for split in splitted_args {
                if split.is_empty() {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }

                if split.to_vec().strip_spaces().first().unwrap().is_symbol() {
                    match split.to_vec().strip_spaces().first().unwrap() {
                        Token::Plus
                        | Token::Minus
                        | Token::OpenParenthesis
                        | Token::OpenSquareBracket => {
                            // TODO: NOTHING
                        }
                        _ => {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                "Unprocessible entity {}",
                                split.to_vec().strip_spaces().first().unwrap().to_string()
                            )))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }
                    }
                }
                let construct = parse_value(split.to_vec(), line);
                arguments.push(ArgumentType::Positional(construct));
            }
        }
    }

    arguments
}

/// target possible function ptr invocation
fn process_method_data_with_possible_fn_ptr_invocation(
    validation: impl Fn(),
    raw_value: &Vec<Token>,
    iteration: usize,
    line: i32,
    method_data: &[Token],
) -> Value {
    match method_data[0] {
        Token::Dot => Value::Dot(Box::new(parse_value(
            raw_value.strip_spaces()[iteration + 2..].to_vec(),
            line,
        ))),
        Token::OpenSquareBracket => {
            let nested = parse_value(raw_value.strip_spaces()[iteration + 1..].to_vec(), line);
            if let Value::ArrayValue(_value) = &nested {
                /* VALIDATIONS */
                if _value.variants.len() != 1 {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                }

                if let Some(_) = _value.then {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            nested
        }

        Token::OpenParenthesis => {
            validation();

            let mut fn_ptr_open_contex = 0;
            let mut fn_ptr_iteration = 0;
            for _strip in &method_data.to_vec().strip_spaces() {
                match _strip {
                    Token::OpenParenthesis => {
                        fn_ptr_open_contex += 1;
                    }
                    Token::CloseParenthesis => {
                        fn_ptr_open_contex -= 1;
                        if fn_ptr_open_contex == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
                fn_ptr_iteration += 1;
            }

            if fn_ptr_open_contex != 0 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing )"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }

            let mut fn_ptr_nested = Value::None;
            let fn_ptr_raw_variants = &method_data.to_vec().strip_spaces()[1..fn_ptr_iteration];
            let fn_ptr_method_data = &method_data.to_vec().strip_spaces()[fn_ptr_iteration + 1..];
            if !fn_ptr_method_data.is_empty() {
                fn_ptr_nested = process_method_data_with_possible_fn_ptr_invocation(
                    validation,
                    &method_data.to_vec(),
                    fn_ptr_iteration,
                    line,
                    fn_ptr_method_data,
                );
            }
            if !fn_ptr_raw_variants.is_empty() {
                let fn_ptr_arguments = process_args(&raw_value, fn_ptr_raw_variants, line);
                Value::FunctionPTRInvocation(FunctionPTRInvocation {
                    args: Some(fn_ptr_arguments),
                    then: if let Value::None = fn_ptr_nested {
                        None
                    } else {
                        Some(Box::new(fn_ptr_nested))
                    },
                })
            } else {
                Value::FunctionPTRInvocation(FunctionPTRInvocation {
                    args: None,
                    then: if let Value::None = fn_ptr_nested {
                        None
                    } else {
                        Some(Box::new(fn_ptr_nested))
                    },
                })
            }
        }
        Token::Plus | Token::Minus | Token::Multiply | Token::Divide => {
            process_math_operation(&method_data.to_vec(), line, &method_data[0])
        }
        _ => {
            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                "Unprocessible entity {}",
                raw_value.to_string()
            )))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            unreachable!()
        }
    }
    // nested
}

fn process_contract_like_instance(
    raw_value: Vec<Token>,
    line: i32,
    identifier: &Token,
    validation_on_empty_args: impl Fn(),
) -> Value {
    let stripped_spaces = raw_value.strip_spaces();
    let _identifier = identifier.to_string();

    let mut open_contex = 0;
    let mut iteration = 0;

    for _strip in &stripped_spaces {
        match _strip {
            Token::OpenParenthesis => {
                open_contex += 1;
            }
            Token::CloseParenthesis => {
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

    let mut nested: Value = Value::None;
    let raw_args: &[Token] = &stripped_spaces[3..iteration];
    let method_data: &[Token] = &stripped_spaces[iteration + 1..];

    if !method_data.is_empty() {
        match method_data[0] {
            Token::Dot => {
                nested = Value::Dot(Box::new(parse_value(
                    stripped_spaces[iteration + 2..].to_vec(),
                    line,
                )))
            }
            Token::OpenSquareBracket => {
                nested = parse_value(stripped_spaces[iteration + 1..].to_vec(), line);
                if let Value::ArrayValue(_value) = &nested {
                    /* VALIDATIONS */
                    if _value.variants.len() != 1 {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError("Unprocessible entity"))
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

    if raw_args.is_empty() {
        validation_on_empty_args();
        let variable_value = Value::InstanceValue(InstanceValue {
            value: InstanceVariable {
                r#type: _identifier.to_owned(),
                size: None,
                arguments: None,
            },
            then: if let Value::None = nested {
                None
            } else {
                Some(Box::new(nested))
            },
        });

        return variable_value;
    } else {
        let arguments = process_args(&raw_value, raw_args, line);
        if arguments.len() > 1 {
            validation_on_empty_args()
        }
        let variable_value = Value::InstanceValue(InstanceValue {
            value: InstanceVariable {
                r#type: _identifier.to_owned(),
                size: None,
                arguments: Some(arguments),
            },
            then: if let Value::None = nested {
                None
            } else {
                Some(Box::new(nested))
            },
        });

        return variable_value;
    }
}

fn process_math_operation(raw_value: &Vec<Token>, line: i32, operation: &Token) -> Value {
    let stripped_value = raw_value.strip_spaces();
    let operands = &stripped_value[1..];
    if operands.is_empty() {
        CompilerError::SyntaxError(SyntaxError::SyntaxError("Missing operands"))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
    }

    if operands.len() == 1 && operands[0] == *operation {
        let variable_value = Value::ExpressionValue(ExpressionValue {
            value: ExpressionVariable {
                r#type: match operation {
                    Token::Plus => ExpressionTypes::PostIncrement,
                    Token::Minus => ExpressionTypes::PostDecrement,
                    _ => {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "Unprocessible entity for {}",
                            operation.to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        unreachable!()
                    }
                },
                operand: Box::new(Value::None),
            },
            then: None,
        });
        return variable_value;
    } else if stripped_value.len() > 2 && stripped_value[0] == stripped_value[1] {
        if *operation == Token::Plus || *operation == Token::Minus {
            let variable_value = Value::ExpressionValue(ExpressionValue {
                value: ExpressionVariable {
                    r#type: match operation {
                        Token::Plus => ExpressionTypes::PreIncrement,
                        Token::Minus => ExpressionTypes::PreDecrement,

                        _ => {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                "Unprocessible entity for{}",
                                operation.to_string()
                            )))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            unreachable!()
                        }
                    },
                    operand: Box::new(parse_value(stripped_value[2..].to_vec(), line)),
                },
                then: None,
            });

            return variable_value;
        } else {
            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                "Unprocessible entity for {}",
                raw_value.to_string()
            )))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            unreachable!()
        }
    } else if stripped_value.len() > 1 && stripped_value[1] == Token::Equals {
        let variable_value = Value::ExpressionValue(ExpressionValue {
            value: ExpressionVariable {
                r#type: match operation {
                    Token::Plus => ExpressionTypes::PlusEq,
                    Token::Minus => ExpressionTypes::MinusEq,
                    Token::Multiply => ExpressionTypes::MulEq,
                    Token::Divide => ExpressionTypes::DivEq,
                    _ => {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "Unprocessible entity for {}",
                            operation.to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        unreachable!()
                    }
                },
                operand: Box::new(parse_value(stripped_value[2..].to_vec(), line)),
            },
            then: None,
        });

        return variable_value;
    } else {
        if operands[0] == *operation {
            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                "Unprocessible entity for {}",
                raw_value.to_string()
            )))
            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
        }
        let variable_value = Value::ExpressionValue(ExpressionValue {
            value: ExpressionVariable {
                r#type: match operation {
                    Token::Plus => ExpressionTypes::Add,
                    Token::Minus => ExpressionTypes::Sub,
                    Token::Multiply => ExpressionTypes::Mul,
                    Token::Divide => ExpressionTypes::Div,
                    Token::Modulu => ExpressionTypes::Mod,
                    _ => {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "Unprocessible entity for {}",
                            operation.to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        unreachable!()
                    }
                },
                operand: Box::new(parse_value(operands.to_vec(), line)),
            },
            then: None,
        });

        return variable_value;
    }
}
