use crate::mods::ast::function::{
    Argument, FunctionArgState, FunctionHeader, FunctionHeaderState, ModifierCall,
};
use crate::mods::constants::constants::FILE_PATH;
use crate::mods::errors::error::{CompilerError, SyntaxError};
use crate::mods::lexer::lexer::{TStringExtension, TTokenTrait, TVecExtension};
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
    let mut has_updated_visibility = false;
    let mut has_updated_mutability = false;
    let mut has_updated_inheritance = false;

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
                    function_header.name = _identifier.to_owned();
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

                                        for split in splitted {
                                            if split.is_empty() {
                                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                                    "Unexpected trailing comma in parameter list",
                                                ))
                                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
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

                        for split in splitted_args {
                            if split.is_empty() {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Unexpected trailing comma in parameter list",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }

                            let arg = process_args(split, line);
                            if function_header.arguments.is_none() {
                                function_header.arguments = Some(Vec::new());
                            }
                            function_header.arguments.as_mut().unwrap().push(arg);
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

                        for split in splitted_args {
                            if split.is_empty() {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    "Unexpected trailing comma in parameter list",
                                ))
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                            }

                            let arg = process_args(split, line);
                            if function_header.returns.is_none() {
                                function_header.returns = Some(Vec::new());
                            }
                            function_header.returns.as_mut().unwrap().push(arg);
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
                    if has_updated_visibility {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Double declaration for function visibility",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    function_header.visibility = Some(token.clone());
                    state = FunctionHeaderState::Visibility;
                    has_updated_visibility = true;
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
                    if has_updated_inheritance {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Double declaration for function inheritance",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    function_header.inheritance = Some(token.clone());
                    state = FunctionHeaderState::Inheritance;
                    has_updated_inheritance = true;
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
                    if has_updated_mutability {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Double declaration for function mutability",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    function_header.mutability = Some(token.clone());
                    state = FunctionHeaderState::Mutability;
                    has_updated_mutability = true;
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
    println!("{:#?}", function_header);
}

/// Process function args
fn process_args(raw_args: &[Token], line: i32) -> Argument {
    let mut state = FunctionArgState::None;
    let mut r#type = String::new();
    let mut is_array = false;
    let mut size: Option<String> = None;
    let mut pad = 0;
    let mut payable_address = false;
    let mut name: Option<String> = None;
    let mut location: Option<Token> = None;
    for (index, token) in raw_args.iter().enumerate() {
        if pad > index {
            continue;
        }
        match token {
            Token::Uint(_)
            | Token::Int(_)
            | Token::Bool
            | Token::Bytes(_)
            | Token::Address
            | Token::String => {
                if let FunctionArgState::None = state {
                    r#type.push_str(&token.to_string());
                    state = FunctionArgState::Type;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::Payable => {
                if let FunctionArgState::Type | FunctionArgState::Location = state {
                    if r#type.tokenize() != Token::Address {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "payable can only be specified for address types",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
                    payable_address = true;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::OpenSquareBracket => {
                if let FunctionArgState::Type | FunctionArgState::Location = state {
                    let mut iteration = 0;
                    let mut open_context = 0;

                    for tkn in &raw_args[index..] {
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
                        CompilerError::SyntaxError(SyntaxError::MissingToken(&format!(
                            "] \"{}\"",
                            raw_args.to_vec().to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }

                    let _size = &raw_args[index + 1..iteration + index];
                    if !_size.is_empty() {
                        size = Some(_size.to_vec().to_string());
                    }
                    is_array = true;
                    pad = index + iteration + 1;
                    state = FunctionArgState::Array;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::Memory | Token::Storage | Token::Calldata => {
                if let FunctionArgState::Type | FunctionArgState::Array = state {
                    location = Some(token.clone());
                    state = FunctionArgState::Location;
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::Identifier(_identifier) => {
                if let FunctionArgState::None
                | FunctionArgState::Location
                | FunctionArgState::Array
                | FunctionArgState::Type = state
                {
                    match state {
                        FunctionArgState::None => {
                            r#type.push_str(&token.to_string());
                            state = FunctionArgState::Type;
                        }

                        _ => {
                            name = Some(token.to_string());
                            state = FunctionArgState::Name;
                        }
                    }
                } else {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(
                        "Unprocessible entity for function declaration",
                    ))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }
            }

            Token::Dot => {
                if let FunctionArgState::Type = state {
                    r#type.push_str(".");
                    if let Some(variant) = raw_args.get(index + 1) {
                        if let Token::Identifier(variant_value) = variant {
                            r#type.push_str(&variant_value);
                            pad = index + 2;
                        } else {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                "Unprocessible entity for function declaration",
                            ))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        }
                    } else {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(
                            "Unprocessible entity for function declaration",
                        ))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                    }
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
                    "Unprocessible entity for function declaration",
                ))
                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
            }
        }
    }

    let argument = Argument {
        is_array,
        location,
        name,
        payable_address,
        size,
        r#type,
    };
    argument
}
