use crate::mods::ast::function::{
    ArgType, FunctionHeader, FunctionHeaderState, FunctionType, ModifierCall,
};
use crate::mods::constants::constants::FILE_PATH;
use crate::mods::errors::error::{CompilerError, SyntaxError};
use crate::mods::lexer::lexer::TVecExtension;
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
            for line_desc in &function_lexem {
                if line == 0 {
                    line = line_desc.line;
                }
                if should_break {
                    break;
                }
                for token in &line_desc.data {
                    match token {
                        Token::OpenBraces => {
                            should_break = true;
                            break;
                        }
                        _ => {}
                    }
                    function_header.push(token.to_owned());
                    header_iteration += 1;
                }
            }

            let _header = parse_function_header(function_header, line);
            let mut skipped_count = 0;
            let mut combined: Vec<&Token> = Vec::new();
            let mut opened_context = 0;
            let mut opened_scope = 1;
            for body in &function_lexem {
                for token in &body.data {
                    if skipped_count < header_iteration + 1 {
                        skipped_count += 1;
                        continue;
                    }
                    combined.push(token);

                    match token {
                        Token::OpenBraces => {
                            opened_scope += 1;
                        }

                        Token::CloseBraces => {
                            opened_scope -= 1;

                            if opened_scope == 1 {
                                process_function_body(&combined).unwrap_or_else(|err| {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            body.line,
                                        );
                                });
                                combined.clear();
                            }
                        }
                        Token::OpenParenthesis => {
                            opened_context += 1;
                        }

                        Token::CloseParenthesis => {
                            opened_context -= 1;
                        }
                        Token::SemiColon => {
                            if opened_context == 0 && opened_scope == 1 {
                                process_function_body(&combined).unwrap_or_else(|err| {
                                    CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            body.line,
                                        );
                                });
                                combined.clear();
                            }
                        }

                        _ => {}
                    }
                }
            }

            if opened_context != 0 || opened_scope != 0 {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                    "Unexpected context in function declaration",
                ))
                .throw_with_file_info(
                    &get_env_vars(FILE_PATH).unwrap(),
                    function_lexem.first().unwrap().line,
                );
            }

            if !combined.is_empty() && !&combined[..combined.len() - 1].is_empty() {
                process_function_body(&combined[..combined.len() - 1].to_vec()).unwrap_or_else(
                    |err| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                            .throw_with_file_info(
                                &get_env_vars(FILE_PATH).unwrap(),
                                function_lexem.last().unwrap().line,
                            );
                    },
                );
                combined.clear();
            }
        }
    }
}

fn process_function_body(tokens: &Vec<&Token>) -> Result<(), &'static str> {
    println!("{:?}\n\n\n\n", tokens);

    Ok(())
}

pub fn parse_function_header(header_tokens: Vec<Token>, line: i32) -> FunctionHeader {
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
                | FunctionHeaderState::Array
                | FunctionHeaderState::MemLocation
                | FunctionHeaderState::VarName
                | FunctionHeaderState::VarVisibility
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
                    if function_header.name.is_none() {
                        if let FunctionHeaderState::Visibility
                        | FunctionHeaderState::Array
                        | FunctionHeaderState::MemLocation
                        | FunctionHeaderState::VarVisibility
                        | FunctionHeaderState::Returns = state
                        {
                            function_header.var_name = Some(_identifier.to_string());
                            state = FunctionHeaderState::VarName;
                        } else {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity for function declaration",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }
                    } else {
                        if let FunctionHeaderState::None
                        | FunctionHeaderState::Keyword
                        | FunctionHeaderState::Identifier
                        | FunctionHeaderState::Array
                        | FunctionHeaderState::MemLocation
                        | FunctionHeaderState::VarName
                        | FunctionHeaderState::VarVisibility
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

                                        let raw_args =
                                            &header_tokens[index + 2..iteration + index + 1];
                                        let mut modifier_args: Option<Vec<String>> = None;
                                        if !raw_args.is_empty() {
                                            let splitted = raw_args.to_vec().split_coma();

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
            }

            Token::Returns => {
                if let FunctionHeaderState::None
                | FunctionHeaderState::Keyword
                | FunctionHeaderState::Identifier
                | FunctionHeaderState::Array
                | FunctionHeaderState::MemLocation
                | FunctionHeaderState::VarName
                | FunctionHeaderState::VarVisibility
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

            Token::OpenSquareBracket => {
                if let FunctionHeaderState::Visibility | FunctionHeaderState::Returns = state {
                    let mut iteration = 0;
                    let mut open_context = 0;

                    for tkn in &header_tokens[index..] {
                        match tkn {
                            Token::OpenSquareBracket => open_context += 1,
                            Token::CloseSquareBracket => {
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

                    let _size = &header_tokens[index + 1..iteration + index];

                    if !_size.is_empty() {
                        function_header.size = Some(_size.to_vec().to_string())
                    }

                    function_header.is_array = true;
                    pad = iteration + index + 1;
                    state = FunctionHeaderState::Array;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::Memory | Token::Calldata | Token::Storage => {
                if let FunctionHeaderState::Array = state {
                    function_header.location = Some(token.to_owned());
                    state = FunctionHeaderState::MemLocation;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::OpenParenthesis => {
                if let FunctionHeaderState::Identifier | FunctionHeaderState::Keyword = state {
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
                        let splitted_args = raw_args.to_vec().split_coma();

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

                            match split.first().unwrap() {
                                Token::Function => {
                                    let function_header_arg =
                                        parse_function_header(split.to_vec(), line);

                                    if function_header.arguments.is_none() {
                                        function_header.arguments = Some(Vec::new());
                                    }
                                    function_header.arguments.as_mut().unwrap().push(
                                        ArgType::Function(FunctionHeader {
                                            r#type: FunctionType::Variable,
                                            ..function_header_arg
                                        }),
                                    );
                                }
                                _ => {
                                    let arg = Variant::process_args(split);
                                    if arg.is_err() {
                                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                            &format!(
                                                "{} {}",
                                                arg.as_ref().err().unwrap(),
                                                split.to_vec().to_string()
                                            ),
                                        ))
                                        .throw_with_file_info(
                                            &get_env_vars(FILE_PATH).unwrap(),
                                            line,
                                        )
                                    }
                                    if function_header.arguments.is_none() {
                                        function_header.arguments = Some(Vec::new());
                                    }
                                    function_header
                                        .arguments
                                        .as_mut()
                                        .unwrap()
                                        .push(ArgType::Variant(arg.unwrap()));
                                }
                            }
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
                        let splitted_args = raw_args.to_vec().split_coma();

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
                if let FunctionHeaderState::Visibility
                | FunctionHeaderState::Returns
                | FunctionHeaderState::Array = state
                {
                    if function_header.name.is_some() {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Unprocessible entity for function declaration",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }

                    function_header.var_visibility = Some(token.to_owned());
                    state = FunctionHeaderState::VarVisibility;
                } else {
                    if let FunctionHeaderState::None
                    | FunctionHeaderState::Keyword
                    | FunctionHeaderState::Identifier
                    | FunctionHeaderState::Array
                    | FunctionHeaderState::MemLocation
                    | FunctionHeaderState::VarName
                    | FunctionHeaderState::VarVisibility
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
            }

            Token::Virtual | Token::Override => {
                if let FunctionHeaderState::None
                | FunctionHeaderState::Keyword
                | FunctionHeaderState::Identifier
                | FunctionHeaderState::Array
                | FunctionHeaderState::MemLocation
                | FunctionHeaderState::VarName
                | FunctionHeaderState::VarVisibility
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
                | FunctionHeaderState::Array
                | FunctionHeaderState::MemLocation
                | FunctionHeaderState::VarName
                | FunctionHeaderState::VarVisibility
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
                    "Unprocessible entity for function declaration",
                ))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
        }
    }

    function_header
}
