use crate::mods::ast::function::{FunctionHeader, FunctionHeaderState};
use crate::mods::constants::constants::FILE_PATH;
use crate::mods::errors::error::{CompilerError, SyntaxError};
use crate::mods::lexer::tokens::Token;
use crate::mods::utils::functions::global::get_env_vars;
use crate::mods::utils::types::line_descriptors::LineDescriptions;

pub fn parse_functions(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) {
    for function_lexem in lexems {
        let mut header_iteration = 0;
        {
            let mut function_header: Vec<Token> = Vec::new();
            let mut line = 0;
            let mut should_break = false;
            for line_desc in function_lexem {
                if line == 0 {
                    line = line_desc.line;
                }
                if should_break {
                    break;
                }
                for token in line_desc.data {
                    match token {
                        Token::OpenBraces => {
                            should_break = true;
                            break;
                        }
                        _ => {}
                    }
                    function_header.push(token);
                    header_iteration += 1;
                }
            }

            parse_function_header(function_header, line);
        }
    }
}

fn parse_function_header(header_tokens: Vec<Token>, line: i32) {
    if header_tokens[0] != Token::Function {
        CompilerError::SyntaxError(SyntaxError::SyntaxError(
            "Unprocessible entity for function declaration",
        ))
        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
    }

    let mut state = FunctionHeaderState::None;

    let mut function_header = FunctionHeader::new();
    let mut pad = 0;

    for (index, token) in header_tokens.iter().enumerate() {
        if pad > index {
            continue;
        }
        match token {
            Token::Function => {
                if let FunctionHeaderState::None = state {
                    state = FunctionHeaderState::Keyword;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::Identifier(_identifier) => {
                if let FunctionHeaderState::Keyword = state {
                    function_header.name = _identifier.to_owned();
                    state = FunctionHeaderState::Identifier;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }
            Token::OpenParenthesis => {
                if let FunctionHeaderState::Identifier = state {
                    let mut iteration = 0;
                    let mut open_context = 0;

                    for tkns in &header_tokens[index..] {
                        match tkns {
                            Token::OpenParenthesis => open_context += 1,
                            Token::CloseParenthesis => {
                                open_context -= 1;
                                if open_context == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }

                        iteration += 1;
                    }

                    if open_context != 0 {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Unprocessible entity for function declaration",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    let raw_args = &header_tokens[index + 1..iteration + index];

                    if !raw_args.is_empty() {
                        let splitted_args = raw_args
                            .split(|pred| *pred == Token::Coma)
                            .collect::<Vec<_>>();

                        if splitted_args.is_empty() {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity for function declaration",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }

                        for split in splitted_args {
                            if split.is_empty() {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Unexpected trailing comma in parameter list",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }

                            // println!("{:?}", split); continue here
                        }
                    }

                    state = FunctionHeaderState::Args;
                } else if let FunctionHeaderState::Returns = state {
                    state = FunctionHeaderState::Returns;

                    // HANDLE RETURNS
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }
            Token::Space => {}
            _ => {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                    "Unprocessible here entity for function declaration",
                ))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
        }
    }

    // println!("{:?}", header_tokensc);
}
