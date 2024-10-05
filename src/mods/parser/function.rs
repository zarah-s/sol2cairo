use crate::mods::ast::function::{FunctionHeader, FunctionHeaderState, ModifierCall};
use crate::mods::constants::constants::FILE_PATH;
use crate::mods::errors::error::{CompilerError, SyntaxError};
use crate::mods::lexer::lexer::{TStringExtension, TTokenTrait, TVecExtension};
use crate::mods::lexer::tokens::Token;
use crate::mods::utils::functions::global::get_env_vars;
use crate::mods::utils::types::line_descriptors::LineDescriptions;
use crate::mods::utils::types::variant::{TVariant, Variant};

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

            let header = parse_function_header(function_header, line);
            // println!("{:#?}", header);
        }
    }
}

fn parse_function_header(header_tokens: Vec<Token>, line: i32) -> FunctionHeader {
    let header_tokens = header_tokens.strip_spaces();
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

            Token::Gasless => {
                if let FunctionHeaderState::None
                | FunctionHeaderState::Keyword
                | FunctionHeaderState::Identifier
                | FunctionHeaderState::Returns = state
                {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                } else {
                    if function_header.gasless {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Double declaration for function gasless",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    function_header.gasless = true;
                    state = FunctionHeaderState::Gasless;
                }
            }

            Token::Identifier(_identifier) => {
                if let FunctionHeaderState::Keyword = state {
                    function_header.name = Some(_identifier.to_owned());
                    state = FunctionHeaderState::Identifier;
                } else {
                    if let FunctionHeaderState::None
                    | FunctionHeaderState::Keyword
                    | FunctionHeaderState::Identifier
                    | FunctionHeaderState::Returns = state
                    {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Unprocessible entity for function declaration",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    } else {
                        if let Some(_tkn) = header_tokens.get(index + 1) {
                            match _tkn {
                                Token::OpenParenthesis => {
                                    let mut iteration = 0;
                                    let mut open_context = 0;

                                    for tkns in &header_tokens[index + 1..] {
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
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        );
                                    }

                                    let raw_args = &header_tokens[index + 2..iteration + index + 1];
                                    let mut modifier_args: Option<Vec<String>> = None;
                                    if !raw_args.is_empty() {
                                        let splitted = raw_args
                                            .split(|pred| *pred == Token::Coma)
                                            .collect::<Vec<_>>();

                                        for (index, split) in splitted.iter().enumerate() {
                                            if split.is_empty() {
                                                if index == splitted.len() - 1 {
                                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                                        "Unexpected trailing comma in parameter list",
                                                    ))
                                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                                } else {
                                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                                        "Unprocessible entity for function declaration",
                                                    ))
                                                    .throw_with_file_info(
                                                        &get_env_vars(FILE_PATH).unwrap(),
                                                        line,
                                                    );
                                                }
                                            }

                                            if modifier_args.is_none() {
                                                modifier_args = Some(Vec::new());
                                            }

                                            modifier_args
                                                .as_mut()
                                                .unwrap()
                                                .push(split.to_vec().to_string());
                                        }
                                    }

                                    if function_header.modifiers.is_none() {
                                        function_header.modifiers = Some(Vec::new());
                                    }
                                    function_header.modifiers.as_mut().unwrap().push(
                                        ModifierCall {
                                            arguments: modifier_args,
                                            identifier: _identifier.to_owned(),
                                        },
                                    );

                                    pad = index + iteration + 2;
                                    state = FunctionHeaderState::Modifiers;
                                }

                                _ => {
                                    if function_header.modifiers.is_none() {
                                        function_header.modifiers = Some(Vec::new());
                                    }
                                    function_header.modifiers.as_mut().unwrap().push(
                                        ModifierCall {
                                            arguments: None,
                                            identifier: _identifier.to_owned(),
                                        },
                                    );
                                    state = FunctionHeaderState::Modifiers;
                                }
                            }
                        } else {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity for function declaration",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }
                    }
                }
            }

            Token::Returns => {
                if let FunctionHeaderState::None
                | FunctionHeaderState::Keyword
                | FunctionHeaderState::Identifier
                | FunctionHeaderState::Returns = state
                {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                } else {
                    if function_header.returns.is_some() {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Unprocessible entity for function declaration",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }

                    if let Some(_next) = header_tokens.get(index + 1) {
                        match _next {
                            Token::OpenParenthesis => {
                                state = FunctionHeaderState::Returns;
                            }
                            _ => {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Unprocessible entity for function declaration",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }
                        }
                    } else {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Unprocessible entity for function declaration",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
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

                        for (index, split) in splitted_args.iter().enumerate() {
                            if split.is_empty() {
                                if index == splitted_args.len() - 1 {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unexpected trailing comma in parameter list",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                } else {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unprocessible entity for function declaration",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }
                            }

                            let arg = Variant::process_args(split);
                            if arg.is_err() {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                    "{} {}",
                                    arg.as_ref().err().unwrap(),
                                    split.to_vec().to_string()
                                )))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                            }
                            if function_header.arguments.is_none() {
                                function_header.arguments = Some(Vec::new());
                            }
                            function_header
                                .arguments
                                .as_mut()
                                .unwrap()
                                .push(arg.unwrap());
                        }
                    }

                    state = FunctionHeaderState::Args;
                    pad = index + iteration + 1;
                } else if let FunctionHeaderState::Returns = state {
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

                        for (index, split) in splitted_args.iter().enumerate() {
                            if split.is_empty() {
                                if index == splitted_args.len() - 1 {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unexpected trailing comma in parameter list",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                } else {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                        "Unprocessible entity for function declaration",
                                    ))
                                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                                }
                            }

                            let arg = Variant::process_args(split);
                            if arg.is_err() {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                                    "{} {}",
                                    arg.as_ref().err().unwrap(),
                                    split.to_vec().to_string()
                                )))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                            }
                            if function_header.returns.is_none() {
                                function_header.returns = Some(Vec::new());
                            }
                            function_header.returns.as_mut().unwrap().push(arg.unwrap());
                        }
                    }

                    pad = index + iteration + 1;
                    state = FunctionHeaderState::Returns;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::External | Token::Internal | Token::Public | Token::Private => {
                if let FunctionHeaderState::None
                | FunctionHeaderState::Keyword
                | FunctionHeaderState::Identifier
                | FunctionHeaderState::Returns = state
                {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                } else {
                    if function_header.visibility.is_some() {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Double declaration for function visibility",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    function_header.visibility = Some(token.clone());
                    state = FunctionHeaderState::Visibility;
                }
            }

            Token::Virtual | Token::Override => {
                if let FunctionHeaderState::None
                | FunctionHeaderState::Keyword
                | FunctionHeaderState::Identifier
                | FunctionHeaderState::Returns = state
                {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                } else {
                    if function_header.inheritance.is_some() {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Double declaration for function inheritance",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    function_header.inheritance = Some(token.clone());
                    state = FunctionHeaderState::Inheritance;
                }
            }

            Token::Pure | Token::View => {
                if let FunctionHeaderState::None
                | FunctionHeaderState::Keyword
                | FunctionHeaderState::Identifier
                | FunctionHeaderState::Returns = state
                {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                } else {
                    if function_header.mutability.is_some() {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Double declaration for function mutability",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    function_header.mutability = Some(token.clone());
                    state = FunctionHeaderState::Mutability;
                }
            }

            _ => {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                    "Unprocessible here entity for function declaration",
                ))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
        }
    }

    function_header
}
