use crate::mods::ast::mapping::{Mapping, MappingAST, MappingHeader};
use crate::mods::ast::r#struct::{StructAST, StructHeader, StructType, StructVariant};
use crate::mods::errors::error::{CompilerError, ErrType, SyntaxError};
use crate::mods::utils::functions::global::validate_identifier;
use crate::mods::utils::types::visibility::Visibility;
use crate::mods::{
    constants::constants::FILE_PATH, utils::types::line_descriptors::LineDescriptions,
};

use crate::mods::parser::mapping::process_mapping;

use crate::mods::lexer::{
    lexer::{TStringExtension, TTokenTrait, TVecExtension},
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
            let mut variant = StructVariant::new();
            process_non_mapping_variant(combined, &mut variant)?;

            let variant_construct = StructType::Variant(variant);
            return Ok(variant_construct);
        }
        _other => {
            // println!("{:?}", _other);
            return Err((
                format!("Invalid variant declaration \"{}\"", combined.to_string()),
                ErrType::Syntax,
            ));
        }
    }
}

enum Stage {
    TypeDeclaration,
    Name,
    Dot,
    None,
}
fn process_non_mapping_variant(
    combined: &Vec<Token>,
    variant: &mut StructVariant,
) -> Result<(), (String, ErrType)> {
    let mut is_array = false;
    let mut payable = false;
    let mut r#type = String::new();
    let mut size: Option<String> = None;
    let mut name = String::new();
    let mut stage = Stage::None;
    let stripped_spaces = combined.strip_spaces();
    for (index, tkn) in stripped_spaces.iter().enumerate() {
        match tkn {
            Token::Identifier(_identifier) => match stage {
                Stage::None | Stage::Dot => {
                    r#type.push_str(&tkn.to_string());
                    stage = Stage::TypeDeclaration;
                }

                Stage::TypeDeclaration => {
                    name.push_str(&_identifier);
                    stage = Stage::Name;
                }
                _ => {
                    return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                }
            },

            Token::SemiColon => match stage {
                Stage::Name => {}
                _ => {
                    return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                }
            },

            Token::Uint(_)
            | Token::Int(_)
            | Token::Bool
            | Token::Bytes(_)
            | Token::Address
            | Token::String => {
                if let Stage::None = stage {
                    r#type.push_str(&tkn.to_string());
                    stage = Stage::TypeDeclaration;
                } else {
                    return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                }
            }
            Token::Dot => {
                if let Stage::TypeDeclaration = stage {
                    r#type.push_str(&tkn.to_string());
                    stage = Stage::Dot;
                } else {
                    return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                }
            }

            Token::OpenSquareBracket => {
                if let Stage::TypeDeclaration = stage {
                    is_array = true;
                    let mut open_contex = 0;
                    let mut iteration = 0;
                    for _strip in &stripped_spaces[index..] {
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
                        return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                    }
                    let raw_size = &stripped_spaces[index + 1..index + iteration];
                    if !raw_size.is_empty() {
                        size = Some(raw_size.to_vec().to_string());
                    }
                    if stripped_spaces.len() < index + iteration + 1 {
                        return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                    }
                    let raw_name = &stripped_spaces[index + iteration + 1..];
                    if raw_name.len() != 2 {
                        return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                    } else {
                        if let Token::Identifier(_name) = &raw_name[0] {
                            validate_identifier(_name).unwrap();
                            name = _name.to_string()
                        } else {
                            return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                        }

                        if raw_name[1] != Token::SemiColon {
                            return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                        }
                    }

                    break;
                } else {
                    return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                }
            }

            Token::Payable => {
                if let Stage::TypeDeclaration = stage {
                    if let Token::Address = r#type.tokenize() {
                        payable = true;
                    } else {
                        return Err((
                            "Cannot declare payable for non-address type".to_string(),
                            ErrType::Syntax,
                        ));
                    }
                } else {
                    return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
                }
            }
            _ => {
                return Err(("Uprocessible entity".to_string(), ErrType::Syntax));
            }
        }
    }

    *variant = StructVariant {
        is_array,
        name,
        array_size: size,
        r#type,
        payable,
    };

    Ok(())
}
