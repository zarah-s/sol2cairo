use crate::mods::{
    functions::helpers::global::validate_identifier,
    types::{
        compiler_errors::{CompilerError, SyntaxError},
        line_descriptors::LineDescriptions,
        token::{Token, TokenTrait, VecExtension},
    },
};

#[derive(Debug)]
pub struct CustomErrorIdentifier {
    pub identifier: String,
    pub args: Option<Vec<String>>,
}

#[derive(Debug)]
enum ErrorState {
    None,
    Coma,
    Arg,
}

pub fn parse_custom_errors(
    lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>,
) -> Vec<CustomErrorIdentifier> {
    let mut custom_errors = Vec::new();

    for lexem in lexems {
        let mut error_identifier = String::new();
        /* SANITY CHECKS */

        {
            if lexem.is_empty() {
                continue;
            }

            if lexem.first().unwrap().data.is_empty() {
                continue;
            }
            let first_element = lexem.first().unwrap().data.first().unwrap();

            if *first_element != Token::Error {
                CompilerError::InternalError(&format!(
                    "Expecting error but found {}",
                    first_element.to_string()
                ))
                .throw_with_file_info("Contract.sol", lexem.first().unwrap().line)
            }
        }

        let mut header_index_stop = 0;

        /* VALIDATE HEADER */
        {
            let mut header_tokens: Vec<&Token> = Vec::new();
            let header_line = lexem.first().unwrap().line;
            let mut should_break = false;
            for lex in &lexem {
                if should_break {
                    break;
                }
                for token in &lex.data {
                    header_index_stop += 1;
                    if *token == Token::OpenParenthesis {
                        should_break = true;
                        break;
                    }
                    header_tokens.push(token);
                }
            }

            if header_tokens.strip_spaces().is_empty() {
                CompilerError::SyntaxError(
                    crate::mods::types::compiler_errors::SyntaxError::MissingToken("{"),
                )
                .throw_with_file_info("Contract.sol", header_line)
            }

            if header_tokens.strip_spaces().len() != 2 {
                CompilerError::SyntaxError(
                    crate::mods::types::compiler_errors::SyntaxError::SyntaxError(
                        header_tokens.to_string().trim(),
                    ),
                )
                .throw_with_file_info("Contract.sol", header_line)
            } else {
                if let Token::Identifier(identifier) = header_tokens.strip_spaces().last().unwrap()
                {
                    validate_identifier(&identifier).unwrap_or_else(|err| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                            .throw_with_file_info("Contract.sol", header_line)
                    });
                    error_identifier = identifier.to_owned();
                } else {
                    CompilerError::SyntaxError(
                        crate::mods::types::compiler_errors::SyntaxError::SyntaxError(&format!(
                            "Expecting identifier but found {}",
                            header_tokens.strip_spaces().last().unwrap().to_string()
                        )),
                    )
                    .throw_with_file_info("Contract.sol", header_line)
                }
            }
        }

        {
            let mut arguments: Vec<String> = Vec::new();
            let mut skipped_count = 0;
            let mut opened_paren = 1; /* validate header skips one count */
            let mut error_state = ErrorState::None;
            for (parent_index, lex) in lexem.iter().enumerate() {
                for (index, token) in lex.data.iter().enumerate() {
                    if skipped_count < header_index_stop {
                        skipped_count += 1;
                        continue;
                    }

                    match token {
                        Token::Space => {}
                        Token::Identifier(ref _variant) => {
                            if let ErrorState::Coma | ErrorState::None = error_state {
                                let _ = validate_identifier(&_variant).unwrap_or_else(|err| {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                                        .throw_with_file_info("Contract.sol", lex.line)
                                });
                                arguments.push(_variant.to_owned());
                                error_state = ErrorState::Arg;
                            } else {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", lex.line)
                            }
                        }

                        Token::Uint(_)
                        | Token::Int(_)
                        | Token::Bool
                        | Token::Bytes(_)
                        | Token::Address
                        | Token::String => {
                            if let ErrorState::Coma | ErrorState::None = error_state {
                                arguments.push(token.to_string());
                                error_state = ErrorState::Arg;
                            } else {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", lex.line)
                            }
                        }
                        Token::Coma => {
                            if let ErrorState::Arg = error_state {
                                error_state = ErrorState::Coma;
                            } else {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", lex.line)
                            }
                        }
                        Token::CloseParenthesis => {
                            opened_paren -= 1;
                            if opened_paren != 0 {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", lex.line)
                            }
                        }

                        Token::SemiColon => {
                            if parent_index != lexem.len() - 1 || index != lex.data.len() - 1 {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", lex.line)
                            }
                        }
                        _other => CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                            &_other.to_string(),
                        ))
                        .throw_with_file_info("Contract.sol", lex.line),
                    }
                }
            }

            let error_construct = CustomErrorIdentifier {
                identifier: error_identifier,
                args: if arguments.is_empty() {
                    None
                } else {
                    Some(arguments)
                },
            };

            custom_errors.push(error_construct);
        }
    }

    custom_errors
}
