use crate::mods::{
    constants::constants::FILE_PATH,
    functions::helpers::global::{process_name, process_size, process_type, validate_identifier},
    types::{
        compiler_errors::{CompilerError, ErrType, SyntaxError},
        identifiers::mapping::{process_mapping, Mapping},
        line_descriptors::LineDescriptions,
        token::{TTokenTrait, TVecExtension, Token, Visibility},
    },
};

use super::mapping::{MappingHeader, MappingIdentifier};

pub trait TStructIdentifier {
    fn is_storage(&self) -> bool;
    fn parse_structs(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<StructIdentifier>;
}

#[derive(Debug)]
struct StructHeader {
    pub identifier: String,
}

#[derive(Debug)]
pub struct StructIdentifier {
    pub header: StructHeader,
    pub line: String,
    pub types: Vec<StructType>,
}

#[derive(Debug)]
pub enum StructType {
    Mapping(MappingIdentifier),
    Variant(Variant),
}

#[derive(Debug)]
pub struct Variant {
    pub r#type: String,
    pub name: String,
    pub array_size: Option<String>,
    pub is_array: bool,
}

impl Variant {
    pub fn new() -> Self {
        Self {
            r#type: String::new(),
            name: String::new(),
            array_size: None,
            is_array: false,
        }
    }
}

impl TStructIdentifier for StructIdentifier {
    fn is_storage(&self) -> bool {
        for data in &self.types {
            match data {
                StructType::Mapping(_) => return true,
                _ => {}
            }
        }

        false
    }

    fn parse_structs(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<StructIdentifier> {
        parse_structs(lexems)
    }
}

fn parse_structs(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<StructIdentifier> {
    let mut structs: Vec<StructIdentifier> = Vec::new();
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
                CompilerError::SyntaxError(
                    crate::mods::types::compiler_errors::SyntaxError::MissingToken("{"),
                )
                .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), header_line)
            }

            if header_tokens.strip_spaces().len() != 2 {
                CompilerError::SyntaxError(
                    crate::mods::types::compiler_errors::SyntaxError::SyntaxError(
                        header_tokens.to_string().trim(),
                    ),
                )
                .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), header_line)
            } else {
                if let Token::Identifier(identifier) = header_tokens.strip_spaces().last().unwrap()
                {
                    validate_identifier(&identifier).unwrap_or_else(|err| {
                        CompilerError::SyntaxError(
                            crate::mods::types::compiler_errors::SyntaxError::SyntaxError(&err),
                        )
                        .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), header_line)
                    });
                    struct_identifier = identifier.to_owned();
                } else {
                    CompilerError::SyntaxError(
                        crate::mods::types::compiler_errors::SyntaxError::SyntaxError(&format!(
                            "Expecting identifier but found {}",
                            header_tokens.strip_spaces().last().unwrap().to_string()
                        )),
                    )
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
                                CompilerError::SyntaxError(
                                    crate::mods::types::compiler_errors::SyntaxError::MissingToken(
                                        ";",
                                    ),
                                )
                                .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), lex.line)
                            }
                        }

                        _ => {
                            combined.push(token);
                        }
                    }
                }
            }
        }
        let struct_construct = StructIdentifier {
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
            let mapping_construct = StructType::Mapping(MappingIdentifier {
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
            let mut variant = Variant::new();
            process_non_mapping_variant(combined, &mut variant)?;

            let variant_construct = StructType::Variant(variant);
            return Ok(variant_construct);
        }
        _other => {
            return Err((
                format!("Invalid variant declaration \"{}\"", combined.to_string()),
                ErrType::Syntax,
            ))
        }
    }
}

fn process_non_mapping_variant(
    combined: &Vec<Token>,
    variant: &mut Variant,
) -> Result<(), (String, ErrType)> {
    let mut is_array = false;
    let mut r#type = String::new();
    let mut size: Option<String> = None;
    let mut name = String::new();
    let open_bracket_index = combined
        .iter()
        .position(|pred| *pred == Token::OpenSquareBracket);

    if let Some(_bracket_index) = open_bracket_index {
        /* PROCESS TYPE */
        let backward_slice = &combined[.._bracket_index];
        process_type(backward_slice, &mut r#type, combined)?;
        is_array = true;
        /* PROCESS ARRAY SIZE */
        let close_bracket_index = &combined[_bracket_index + 1..]
            .iter()
            .position(|pred| *pred == Token::CloseSquareBracket);
        if let Some(_close_bracket_index) = close_bracket_index {
            size = process_size(combined, _bracket_index, *_close_bracket_index)?;
        } else {
            return Err((format!("] \"{}\"", combined.to_string()), ErrType::Missing));
        }

        /* PROCESS NAME */
        let name_definition = &combined[_bracket_index + 1..][close_bracket_index.unwrap() + 1..]
            .to_vec()
            .strip_spaces();

        process_name(name_definition, &mut name, combined)?;
    } else {
        if let Token::Dot = combined.strip_spaces()[1] {
            let slice = &combined[..3];
            process_type(slice, &mut r#type, combined)?;
            let name_definition = &combined[3..].to_vec().strip_spaces();
            process_name(name_definition, &mut name, combined)?;
        } else {
            process_type(&combined[..1], &mut r#type, combined)?;
            let name_definition = &combined[1..].to_vec().strip_spaces();
            process_name(name_definition, &mut name, combined)?;
        }
    }

    *variant = Variant {
        is_array,
        name,
        array_size: size,
        r#type,
    };

    Ok(())
}
