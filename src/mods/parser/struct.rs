use crate::mods::ast::mapping::{Mapping, MappingAST, MappingHeader};
use crate::mods::ast::r#struct::{StructAST, StructHeader, StructType};
use crate::mods::errors::error::{CompilerError, ErrType, SyntaxError};
use crate::mods::utils::functions::global::validate_identifier;
use crate::mods::utils::types::variant::{TVariant, Variant};
use crate::mods::utils::types::visibility::Visibility;
use crate::mods::{
    constants::constants::FILE_PATH, utils::types::line_descriptors::LineDescriptions,
};

use crate::mods::parser::mapping::process_mapping;

use crate::mods::lexer::{
    lexer::{TTokenTrait, TVecExtension},
    tokens::Token,
};

pub fn parse_structs(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<StructAST> {
    let mut structs: Vec<StructAST> = Vec::new();
    for lexem in lexems {
        let mut struct_types: Vec<StructType> = Vec::new();
        let mut struct_identifier = String::new();
        /* SANITY CHECKS */
        {
            if lexem.is_empty() {
                continue;
            }

            if lexem.first().unwrap().data.is_empty() {
                continue;
            }
            let first_element = lexem.first().unwrap().data.first().unwrap();

            if *first_element != Token::Struct {
                CompilerError::InternalError(&format!(
                    "Expecting struct but found {}",
                    first_element.to_string()
                ))
                .throw_with_file_info(
                    &std::env::var(FILE_PATH).unwrap(),
                    lexem.first().unwrap().line,
                )
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
                CompilerError::SyntaxError(SyntaxError::MissingToken("{"))
                    .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), header_line)
            }

            if header_tokens.strip_spaces().len() != 2 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                    header_tokens.to_string().trim(),
                ))
                .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), header_line)
            } else {
                if let Token::Identifier(identifier) = header_tokens.strip_spaces().last().unwrap()
                {
                    validate_identifier(&identifier).unwrap_or_else(|err| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                            .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), header_line)
                    });
                    struct_identifier = identifier.to_owned();
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Expecting identifier but found {}",
                        header_tokens.strip_spaces().last().unwrap().to_string()
                    )))
                    .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), header_line)
                }
            }
        }

        let struct_line = lexem[0].line;
        /* PROCESS VARIANTS */
        {
            let mut skipped_count = 0;
            let mut combined: Vec<Token> = Vec::new();
            for lex in lexem {
                for token in lex.data {
                    if skipped_count < header_index_stop {
                        skipped_count += 1;
                        continue;
                    }
                    match token {
                        Token::SemiColon => {
                            combined.push(token);
                            let variant = process_variants(&combined).unwrap_or_else(
                                |(err, err_type): (String, ErrType)| {
                                    match err_type {
                                        ErrType::Missing => CompilerError::SyntaxError(
                                            SyntaxError::MissingToken(&format!(
                                                "{} for struct {}",
                                                err, struct_identifier
                                            )),
                                        )
                                        .throw_with_file_info(
                                            &std::env::var(FILE_PATH).unwrap(),
                                            lex.line,
                                        ),

                                        ErrType::Syntax => CompilerError::SyntaxError(
                                            SyntaxError::SyntaxError(&format!(
                                                "{} for struct {}",
                                                err, struct_identifier
                                            )),
                                        )
                                        .throw_with_file_info(
                                            &std::env::var(FILE_PATH).unwrap(),
                                            lex.line,
                                        ),
                                        ErrType::Unexpected => CompilerError::SyntaxError(
                                            SyntaxError::UnexpectedToken(&format!(
                                                "{} for struct {}",
                                                err, struct_identifier
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

                            struct_types.push(variant);
                            combined.clear();
                        }
                        Token::CloseBraces => {
                            if !combined.is_empty() {
                                CompilerError::SyntaxError(SyntaxError::MissingToken(";"))
                                    .throw_with_file_info(
                                        &std::env::var(FILE_PATH).unwrap(),
                                        lex.line,
                                    )
                            }
                        }

                        _ => {
                            combined.push(token);
                        }
                    }
                }
            }
        }

        let struct_construct = StructAST {
            header: StructHeader {
                identifier: struct_identifier,
            },
            types: struct_types,
            line: struct_line.to_string(),
        };

        structs.push(struct_construct);
    }

    structs
}

fn process_variants(combined: &Vec<Token>) -> Result<StructType, (String, ErrType)> {
    match &combined[0] {
        Token::Mapping => {
            let mut mapping = Mapping::new();
            let mut mapping_header = MappingHeader::new();
            process_mapping(combined, &mut mapping, &mut mapping_header)?;
            if let Visibility::None = mapping_header.visibility {
                // TODO: NOTHING
            } else {
                return Err((
                    format!(
                        "Invalid visibility declaration on struct mapping \"{}\"",
                        mapping_header.visibility.to_string()
                    ),
                    ErrType::Syntax,
                ));
            }
            let mapping_construct = StructType::Mapping(MappingAST {
                header: mapping_header,
                map: mapping,
            });

            return Ok(mapping_construct);
        }
        Token::Uint(_)
        | Token::Int(_)
        | Token::Bool
        | Token::Bytes(_)
        | Token::Address
        | Token::String
        | Token::Identifier(_) => {
            let variant = Variant::process_args(&combined[..combined.len() - 1]);
            if variant.is_err() {
                return Err((
                    format!(
                        "{} {}",
                        variant.err().unwrap(),
                        combined.to_vec().to_string()
                    ),
                    ErrType::Syntax,
                ));
            }

            let variant_construct = StructType::Variant(variant.unwrap());
            return Ok(variant_construct);
        }
        _ => {
            return Err((
                format!("Invalid variant declaration \"{}\"", combined.to_string()),
                ErrType::Syntax,
            ));
        }
    }
}
