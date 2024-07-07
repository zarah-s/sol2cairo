use crate::mods::{
    functions::helpers::global::validate_identifier,
    types::{
        compiler_errors::{CompilerError, SyntaxError},
        line_descriptors::LineDescriptions,
        token::{Token, TokenTrait, VecExtension},
    },
};

#[derive(Debug)]
pub struct EnumIdentifier {
    pub identifier: String,
    pub variants: Vec<String>,
}

#[derive(Debug)]
enum EnumState {
    None,
    Coma,
    Identifier,
}

pub fn parse_enums(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<EnumIdentifier> {
    let mut enums: Vec<EnumIdentifier> = Vec::new();

    for lexem in lexems {
        let mut enum_identifier = String::new();

        /* SANITY CHECKS */
        {
            if lexem.is_empty() {
                continue;
            }

            if lexem.first().unwrap().data.is_empty() {
                continue;
            }
            let first_element = lexem.first().unwrap().data.first().unwrap();

            if *first_element != Token::Enum {
                CompilerError::InternalError(&format!(
                    "Expecting enum but found {}",
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
                    if *token == Token::OpenBraces {
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
                    enum_identifier = identifier.to_owned();
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

        /* EXTRACT VARIANTS */
        {
            let mut variants: Vec<String> = Vec::new();
            let mut skipped_count = 0;
            let mut opened_brace = 1; /* validate header skips one count */
            let mut enum_state = EnumState::None;
            for lex in lexem {
                for token in lex.data {
                    if skipped_count < header_index_stop {
                        skipped_count += 1;
                        continue;
                    }

                    match token {
                        Token::Space => {}
                        Token::Identifier(ref _variant) => {
                            if let EnumState::Coma | EnumState::None = enum_state {
                                let _ = validate_identifier(&_variant).unwrap_or_else(|err| {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                                        .throw_with_file_info("Contract.sol", lex.line)
                                });
                                variants.push(_variant.to_owned());
                                enum_state = EnumState::Identifier;
                            } else {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", lex.line)
                            }
                        }
                        Token::Coma => {
                            if let EnumState::Identifier = enum_state {
                                enum_state = EnumState::Coma;
                            } else {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", lex.line)
                            }
                        }
                        Token::CloseBraces => {
                            opened_brace -= 1;
                            if opened_brace != 0 {
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

            let enum_construct = EnumIdentifier {
                identifier: enum_identifier,
                variants,
            };

            enums.push(enum_construct);
        }
    }

    enums
}
