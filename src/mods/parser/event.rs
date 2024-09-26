use crate::mods::{
    ast::event::{Arg, EventAST, EventVariants},
    constants::constants::FILE_PATH,
    errors::error::{CompilerError, SyntaxError},
    utils::{
        functions::global::{
            get_env_vars, process_name, process_size, process_type, validate_identifier,
        },
        types::line_descriptors::LineDescriptions,
    },
};

use crate::mods::lexer::{
    lexer::{TTokenTrait, TVecExtension},
    tokens::Token,
};

pub fn parse_events(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<EventAST> {
    let mut events = Vec::new();

    for lexem in lexems {
        let mut event_identifier = String::new();
        /* SANITY CHECKS */

        {
            if lexem.is_empty() {
                continue;
            }

            if lexem.first().unwrap().data.is_empty() {
                continue;
            }
            let first_element = lexem.first().unwrap().data.first().unwrap();

            if *first_element != Token::Event {
                CompilerError::InternalError(&format!(
                    "Expecting event but found {}",
                    first_element.to_string()
                ))
                .throw_with_file_info(
                    &get_env_vars(FILE_PATH).unwrap(),
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
                    if *token == Token::OpenParenthesis {
                        should_break = true;
                        break;
                    }
                    header_tokens.push(token);
                    header_index_stop += 1;
                }
            }

            if header_tokens.strip_spaces().is_empty() {
                CompilerError::SyntaxError(SyntaxError::MissingToken("{"))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), header_line)
            }

            if header_tokens.strip_spaces().len() != 2 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                    header_tokens.to_string().trim(),
                ))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), header_line)
            } else {
                if let Token::Identifier(identifier) = header_tokens.strip_spaces().last().unwrap()
                {
                    validate_identifier(&identifier).unwrap_or_else(|err| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), header_line)
                    });
                    event_identifier = identifier.to_owned();
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "Expecting identifier but found {}",
                        header_tokens.strip_spaces().last().unwrap().to_string()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), header_line)
                }
            }
        }

        {
            let mut variants: Vec<EventVariants> = Vec::new();
            let mut skipped_count = 0;
            let mut raw_args = Vec::new();
            let line = lexem.first().unwrap().line;
            for lex in &lexem {
                for token in &lex.data {
                    if skipped_count < header_index_stop {
                        skipped_count += 1;
                        continue;
                    }

                    raw_args.push(token.to_owned());
                }
            }

            if raw_args[0] != Token::OpenParenthesis {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                    "Misssing ( for {}",
                    raw_args.to_string()
                )))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
            }

            if raw_args[raw_args.len() - 1] != Token::SemiColon {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                    "Misssing ; for {}",
                    raw_args.to_string()
                )))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
            }

            if raw_args[raw_args.len() - 2] != Token::CloseParenthesis {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                    "Misssing ) for {}",
                    raw_args.to_string()
                )))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
            }

            let stripped = &raw_args[1..&raw_args.len() - 2]
                .split(|pred| *pred == Token::Coma)
                .collect::<Vec<_>>();

            for combined in stripped {
                let mut is_array = false;
                let mut r#type = String::new();
                let mut size: Option<String> = None;
                let mut name: Option<String> = None;
                let mut indexed = false;
                let open_bracket_index = combined
                    .iter()
                    .position(|pred| *pred == Token::OpenSquareBracket);

                if let Some(_bracket_index) = open_bracket_index {
                    /* PROCESS TYPE */
                    let backward_slice = &combined[.._bracket_index];

                    process_type(backward_slice, &mut r#type, &combined.to_vec()).unwrap_or_else(
                        |(msg, _)| {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(&msg))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);

                            unreachable!()
                        },
                    );
                    is_array = true;
                    /* PROCESS ARRAY SIZE */
                    let close_bracket_index = &combined[_bracket_index + 1..]
                        .iter()
                        .position(|pred| *pred == Token::CloseSquareBracket);
                    if let Some(_close_bracket_index) = close_bracket_index {
                        size =
                            process_size(&combined.to_vec(), _bracket_index, *_close_bracket_index)
                                .unwrap_or_else(|(msg, _)| {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&msg))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        );

                                    unreachable!()
                                });
                    } else {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "] \"{}\"",
                            combined.to_vec().to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }

                    /* PROCESS NAME */
                    let mut name_definition = combined[_bracket_index + 1..]
                        [close_bracket_index.unwrap() + 1..]
                        .to_vec()
                        .strip_spaces();

                    if !name_definition.is_empty() {
                        if *name_definition.first().unwrap() == Token::Indexed {
                            indexed = true;
                            name_definition.remove(0);
                        }
                        if !name_definition.is_empty() {
                            let mut _name = String::new();
                            process_name(
                                &[name_definition.to_owned(), vec![Token::SemiColon]].concat(),
                                &mut _name,
                                &combined.to_vec(),
                            )
                            .unwrap_or_else(|(msg, _)| {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(&msg))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);

                                unreachable!()
                            });

                            name = Some(_name);
                        }
                    }
                } else {
                    if let Some(_) = combined.to_vec().strip_spaces().get(1) {
                        if let Token::Dot = combined.to_vec().strip_spaces()[1] {
                            let slice = &combined[..3];
                            process_type(slice, &mut r#type, &combined.to_vec()).unwrap_or_else(
                                |(msg, _)| {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&msg))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        );

                                    unreachable!()
                                },
                            );
                            let mut name_definition = combined[3..].to_vec().strip_spaces();
                            if !name_definition.is_empty() {
                                if *name_definition.first().unwrap() == Token::Indexed {
                                    indexed = true;
                                    name_definition.remove(0);
                                }
                                if !name_definition.is_empty() {
                                    let mut _name = String::new();

                                    process_name(
                                        &[name_definition.to_vec(), vec![Token::SemiColon]]
                                            .concat(),
                                        &mut _name,
                                        &combined.to_vec(),
                                    )
                                    .unwrap_or_else(
                                        |(msg, _)| {
                                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                                &msg,
                                            ))
                                            .throw_with_file_info(
                                                &get_env_vars(FILE_PATH).unwrap(),
                                                line,
                                            );

                                            unreachable!()
                                        },
                                    );
                                    name = Some(_name);
                                }
                            }
                        } else {
                            process_type(&combined[..1], &mut r#type, &combined.to_vec())
                                .unwrap_or_else(|(msg, _)| {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&msg))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        );

                                    unreachable!()
                                });
                            let mut name_definition = combined[1..].to_vec().strip_spaces();
                            if !name_definition.is_empty() {
                                if *name_definition.first().unwrap() == Token::Indexed {
                                    indexed = true;
                                    name_definition.remove(0);
                                }
                                if !name_definition.is_empty() {
                                    let mut _name = String::new();

                                    process_name(
                                        &[name_definition.to_vec(), vec![Token::SemiColon]]
                                            .concat(),
                                        &mut _name,
                                        &combined.to_vec(),
                                    )
                                    .unwrap_or_else(
                                        |(msg, _)| {
                                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                                &msg,
                                            ))
                                            .throw_with_file_info(
                                                &get_env_vars(FILE_PATH).unwrap(),
                                                line,
                                            );

                                            unreachable!()
                                        },
                                    );

                                    name = Some(_name);
                                }
                            }
                        }
                    } else {
                        process_type(&combined[..1], &mut r#type, &combined.to_vec())
                            .unwrap_or_else(|(msg, _)| {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(&msg))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);

                                unreachable!()
                            });
                        let mut name_definition = combined[1..].to_vec().strip_spaces();
                        if !name_definition.is_empty() {
                            if *name_definition.first().unwrap() == Token::Indexed {
                                indexed = true;
                                name_definition.remove(0);
                            }
                            if !name_definition.is_empty() {
                                let mut _name = String::new();

                                process_name(
                                    &[name_definition.to_vec(), vec![Token::SemiColon]].concat(),
                                    &mut _name,
                                    &combined.to_vec(),
                                )
                                .unwrap_or_else(|(msg, _)| {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&msg))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        );

                                    unreachable!()
                                });

                                name = Some(_name);
                            }
                        }
                    }
                }

                let arg = EventVariants {
                    indexed,
                    variant: Arg {
                        is_array,
                        name,
                        array_size: size,
                        r#type,
                    },
                };
                variants.push(arg);
            }

            let event_construct = EventAST {
                identifier: event_identifier,
                variants,
                line: lexem[0].line.to_string(),
            };

            events.push(event_construct);
        }
    }

    events
}
