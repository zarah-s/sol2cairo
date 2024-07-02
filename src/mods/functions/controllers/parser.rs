use regex::Regex;

use crate::mods::types::{
    compiler_errors::{CompilerError, SyntaxError},
    line_descriptors::LineDescriptions,
    token::{Token, TokenTrait, VecExtension},
};

pub fn parse_structs(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) {
    for lexem in lexems {
        let mut struct_identifier = String::new();
        // println!("{:?}", lexem);

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
                    let variable_name_pattern = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
                    if variable_name_pattern.is_match(&identifier) {
                        struct_identifier = identifier.to_owned();
                    } else {
                        CompilerError::SyntaxError(
                            crate::mods::types::compiler_errors::SyntaxError::SyntaxError(
                                &format!("Invalid variable name pattern {}", identifier),
                            ),
                        )
                        .throw_with_file_info("Contract.sol", header_line)
                    }
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
            let mut state = State::Terminated;
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
                            state = State::Terminated;

                            process_variants(&combined).unwrap_or_else(
                                |(err, err_type): (String, ErrType)| match err_type {
                                    ErrType::Missing => {
                                        CompilerError::SyntaxError(SyntaxError::MissingToken(
                                            &format!("{} for struct {}", err, struct_identifier),
                                        ))
                                        .throw_with_file_info("Contract.sol", lex.line)
                                    }

                                    ErrType::Syntax => {
                                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                            &format!("{} for struct {}", err, struct_identifier),
                                        ))
                                        .throw_with_file_info("Contract.sol", lex.line)
                                    }
                                    ErrType::Unexpected => {
                                        CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                            &format!("{} for struct {}", err, struct_identifier),
                                        ))
                                        .throw_with_file_info("Contract.sol", lex.line)
                                    }
                                },
                            );
                            combined.clear();
                        }
                        Token::CloseBraces => {
                            if !combined.is_empty() {
                                CompilerError::SyntaxError(
                                    crate::mods::types::compiler_errors::SyntaxError::MissingToken(
                                        ";",
                                    ),
                                )
                                .throw_with_file_info("Contract.sol", lex.line)
                            }
                        }

                        _ => {
                            if let State::Terminated = state {
                                state = State::Initialized;
                            }
                            combined.push(token);
                        }
                    }
                }
            }
        }
    }
}

enum State {
    Initialized,
    Terminated,
}

fn process_variants<'a>(combined: &Vec<Token>) -> Result<(), (String, ErrType)> {
    match &combined[0] {
        Token::Mapping => {
            return Ok(());
        }
        Token::Uint(_)
        | Token::Int(_)
        | Token::Bool
        | Token::Bytes(_)
        | Token::Address
        | Token::String
        | Token::Identifier(_) => {
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
                    let size_definition = &combined[_bracket_index + 1..][..*_close_bracket_index];
                    if !size_definition.to_vec().strip_spaces().is_empty() {
                        for size_ in size_definition {
                            match size_ {
                                Token::Identifier(_)
                                | Token::Multiply
                                | Token::Modulu
                                | Token::Plus
                                | Token::Minus
                                | Token::Divide => {}
                                _token => {
                                    return Err((
                                        format!("{} ", _token.to_string()),
                                        ErrType::Unexpected,
                                    ));
                                }
                            }
                        }

                        size = Some(size_definition.to_vec().to_string())
                    }
                } else {
                    return Err((format!("] \"{}\"", combined.to_string()), ErrType::Missing));
                }

                /* PROCESS NAME */
                let name_definition = &combined[_bracket_index + 1..]
                    [close_bracket_index.unwrap() + 1..]
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
        }
        _other => {
            return Err((
                format!("Invalid variant declaration \"{}\"", combined.to_string()),
                ErrType::Syntax,
            ))
        }
    }
    return Ok(());
}

enum ErrType {
    Missing,
    Syntax,
    Unexpected,
}

fn process_type(
    slice: &[Token],
    r#type: &mut String,
    combined: &Vec<Token>,
) -> Result<(), (String, ErrType)> {
    if slice.len() == 3 {
        if slice.contains(&Token::Dot) {
            for slc in slice {
                match slc {
                    Token::Identifier(identifier) => {
                        let variable_name_pattern =
                            Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
                        if !variable_name_pattern.is_match(&identifier) {
                            return Err((
                                format!("Invalid type identifier \"{}\"", identifier),
                                ErrType::Syntax,
                            ));
                        }
                        r#type.push_str(&identifier);
                    }
                    Token::Dot => r#type.push_str(&Token::Dot.to_string()),
                    _ => {
                        return Err((
                            format!("Invalid variant declaration \"{}\"", combined.to_string()),
                            ErrType::Syntax,
                        ));
                    }
                }
            }
        } else {
            return Err((
                format!("Invalid variant declaration \"{}\"", combined.to_string()),
                ErrType::Syntax,
            ));
        }
    } else if slice.len() == 1 {
        let variable_name_pattern = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
        if !variable_name_pattern.is_match(&slice.first().unwrap().to_string()) {
            return Err((
                format!("Invalid variant declaration \"{}\"", combined.to_string()),
                ErrType::Syntax,
            ));
        }
        r#type.push_str(&slice.first().unwrap().to_string())
    } else {
        return Err((
            format!("Invalid variant declaration \"{}\"", combined.to_string()),
            ErrType::Syntax,
        ));
    }

    Ok(())
}

fn process_name(
    name_definition: &[Token],
    name: &mut String,
    combined: &Vec<Token>,
) -> Result<(), (String, ErrType)> {
    if name_definition.len() == 2 {
        if let Token::Identifier(name_) = name_definition.first().unwrap() {
            let variable_name_pattern = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap();
            if !variable_name_pattern.is_match(&name_) {
                return Err((
                    format!("Invalid name identifier \"{}\"", name_),
                    ErrType::Syntax,
                ));
            }
            name.push_str(&name_);
        }

        if *name_definition.last().unwrap() != Token::SemiColon {
            return Err((format!("; \"{}\"", combined.to_string()), ErrType::Missing));
        }
    } else {
        return Err((
            format!("Invalid variant declaration \"{}\"", combined.to_string()),
            ErrType::Syntax,
        ));
    }

    Ok(())
}
