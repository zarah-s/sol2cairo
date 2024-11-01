use crate::mods::{
    ast::{
        function::{FunctionHeader, FunctionType},
        mapping::{Mapping, MappingAST, MappingHeader},
        variable::{VariableAST, VariableType},
    },
    constants::constants::FILE_PATH,
    errors::error::{CompilerError, ErrType, SyntaxError},
    utils::{
        functions::{global::get_env_vars, value::parse_value},
        types::{
            line_descriptors::LineDescriptions,
            variant::{TVariant, Variant},
        },
    },
};

use crate::mods::lexer::{
    lexer::{TTokenTrait, TVecExtension},
    tokens::Token,
};

use super::{function::parse_function_header, mapping::process_mapping};

pub fn parse_variables(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<VariableAST> {
    let mut variables = Vec::new();

    for lexem in lexems {
        let mut combined: Vec<Token> = Vec::new();
        for lex in lexem {
            for token in lex.data {
                match token {
                    Token::SemiColon => {
                        combined.push(token);
                        let variable = process_var_construct(&combined, lex.line).unwrap_or_else(
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

                        variables.push(variable)
                    }

                    _ => combined.push(token),
                }
            }
        }
    }

    variables
}

fn process_var_construct(
    combined: &Vec<Token>,
    line: i32,
) -> Result<VariableAST, (String, ErrType)> {
    match combined.strip_spaces()[0] {
        Token::Mapping => {
            let mut mapping = Mapping::new();
            let mut mapping_header = MappingHeader::new();

            process_mapping(combined, &mut mapping, &mut mapping_header)?;

            let mapping_construct = MappingAST {
                header: mapping_header,
                map: mapping,
            };

            let variable_construct = VariableAST {
                value: None,
                variable_type: VariableType::Mapping(mapping_construct),
            };

            return Ok(variable_construct);
        }

        Token::Function => {
            let stripped = combined.strip_spaces();
            if Token::SemiColon != stripped[stripped.len() - 1] {
                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(&format!(
                    "Expecting ';' but got '{}'",
                    stripped[stripped.len() - 1].to_string()
                )))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }

            let function_def = stripped[..stripped.len() - 1].to_vec();
            let func_header = parse_function_header(function_def, line);

            let variable_construct = VariableAST {
                value: None,
                variable_type: VariableType::FunctionPTR(FunctionHeader {
                    r#type: FunctionType::Variable,
                    ..func_header
                }),
            };

            return Ok(variable_construct);
        }
        _ => {
            let equals_index = combined.iter().position(|pred| *pred == Token::Equals);

            if equals_index.is_some() {
                let var_definition = &combined[..equals_index.unwrap()];
                let variant = Variant::process_args(&var_definition).unwrap_or_else(|err| {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    unreachable!();
                });
                let value_definition = &combined[equals_index.unwrap() + 1..];
                let value = Some(parse_value(
                    value_definition[..value_definition.len() - 1].to_vec(),
                    line,
                ));
                let variable_ast = VariableAST {
                    value,
                    variable_type: VariableType::Straight(variant),
                };

                return Ok(variable_ast);
            } else {
                if let Token::SemiColon = combined[combined.len() - 1] {
                    let variant = Variant::process_args(&combined[..combined.len() - 1])
                        .unwrap_or_else(|err| {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            unreachable!();
                        });
                    let variable_ast = VariableAST {
                        value: None,
                        variable_type: VariableType::Straight(variant),
                    };

                    return Ok(variable_ast);
                } else {
                    return Err((";".to_string(), ErrType::Missing));
                }
            }
        }
    }
}
