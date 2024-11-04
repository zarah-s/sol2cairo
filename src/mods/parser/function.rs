use crate::mods::ast::function::{
    ArgRet, ArgType, ConditionType, Conditionals, FunctionArm, FunctionHeader, FunctionHeaderState,
    FunctionType, If, ModifierCall,
};
use crate::mods::ast::mapping::{Mapping, MappingAST, MappingHeader};
use crate::mods::constants::constants::FILE_PATH;
use crate::mods::errors::error::{CompilerError, SyntaxError};
use crate::mods::lexer::lexer::TVecExtension;
use crate::mods::lexer::tokens::Token;
use crate::mods::parser::mapping::process_mapping;
use crate::mods::utils::functions::global::get_env_vars;
use crate::mods::utils::functions::value::parse_value;
use crate::mods::utils::types::line_descriptors::LineDescriptions;
use crate::mods::utils::types::variant::{TVariant, Variant};

pub fn parse_functions(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) {
    for function_lexem in lexems {
        let mut header_iteration = 0;
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
        let mut combined: Vec<Token> = Vec::new();
        let mut opened_context = 0;
        let mut opened_scope = 1;

        let mut function_arms = Vec::new();
        for body in &function_lexem {
            for token in &body.data {
                if skipped_count < header_iteration + 1 {
                    skipped_count += 1;
                    continue;
                }
                combined.push(token.clone());

                match token {
                    Token::OpenBraces => {
                        opened_scope += 1;
                    }

                    Token::CloseBraces => {
                        opened_scope -= 1;

                        if opened_scope == 1 {
                            let arm = process_function_body(&combined).unwrap_or_else(|err| {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                                    .throw_with_file_info(
                                        &get_env_vars(FILE_PATH).unwrap(),
                                        body.line,
                                    );
                                unreachable!()
                            });

                            function_arms.push(arm);

                            // println!("{:#?}", res);
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
                            let arm = process_function_body(&combined).unwrap_or_else(|err| {
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                                    .throw_with_file_info(
                                        &get_env_vars(FILE_PATH).unwrap(),
                                        body.line,
                                    );
                                unreachable!()
                            });

                            function_arms.push(arm);
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
            let arm = process_function_body(&combined[..combined.len() - 1].to_vec())
                .unwrap_or_else(|err| {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(err)).throw_with_file_info(
                        &get_env_vars(FILE_PATH).unwrap(),
                        function_lexem.last().unwrap().line,
                    );
                    unreachable!()
                });
            function_arms.push(arm);
            combined.clear();
        }
        println!("{:#?}", function_arms);
    }
}

fn process_function_body(tokens: &Vec<Token>) -> Result<FunctionArm, &'static str> {
    let stripped_tokens = tokens.strip_spaces();

    if stripped_tokens.len() == 0 {
        return Err("Unprocessible entity");
    }

    let mut conditional: Option<Conditionals> = None;

    match &stripped_tokens[0] {
        Token::OpenBraces => {
            let raw_arms = &stripped_tokens[1..stripped_tokens.len() - 1];
            let arms = prepare_function_body(raw_arms.to_vec(), 0);

            return Ok(FunctionArm::Scope(Box::new(arms)));
        }
        Token::Else => {
            if stripped_tokens.len() < 2 {
                return Err("Unprocessible entity for conditional statement");
            }

            match stripped_tokens[1] {
                Token::If => {
                    let res = process_function_body(&stripped_tokens[1..].to_vec());
                    if res.is_err() {
                        return Err(res.err().unwrap());
                    }

                    match res.unwrap() {
                        FunctionArm::If(_val) => {
                            return Ok(FunctionArm::If(If {
                                r#type: ConditionType::ElIf,
                                arm: _val.arm,
                                condition: _val.condition,
                            }))
                        }
                        _ => {
                            return Err("Unprocessible entity for conditional statement");
                        }
                    }
                }
                _ => {
                    let raw_arms = &stripped_tokens[2..stripped_tokens.len() - 1];

                    if raw_arms.len() > 0 {
                        let arms = prepare_function_body(raw_arms.to_vec(), 0);

                        return Ok(FunctionArm::El(Some(arms)));
                    }

                    return Ok(FunctionArm::El(None));
                }
            }
        }
        Token::If => {
            if conditional.is_some() {
                return Err("Unprocessible entity for conditional statement");
            }

            conditional = Some(Conditionals::new());

            if stripped_tokens.len() > 1 && stripped_tokens[1] != Token::OpenParenthesis {
                return Err("Unprocessible entity for conditional statement");
            }

            let mut opened_context = 0;
            let mut iteration = 0;

            for tkn in &stripped_tokens {
                match tkn {
                    Token::OpenParenthesis => opened_context += 1,
                    Token::CloseParenthesis => {
                        opened_context -= 1;
                        if opened_context == 0 {
                            break;
                        }
                    }
                    _ => {}
                }

                iteration += 1;
            }

            if opened_context != 0 {
                return Err("Unprocessible entity for conditional statement");
            }

            let raw_condition = &stripped_tokens[2..iteration];
            let condition = parse_value(raw_condition.to_vec(), 0);
            // conditional.as_mut().unwrap().condition = Some(condition);

            let raw_arms = &stripped_tokens[iteration + 2..stripped_tokens.len() - 1];
            let arms = prepare_function_body(raw_arms.to_vec(), 0);
            return Ok(FunctionArm::If(If {
                r#type: ConditionType::If,
                condition: Some(condition),
                arm: Some(arms),
            }));
            // conditional.as_mut().unwrap().arm = Some(returns);
            // return Ok(FunctionArm::Conditionals(conditional.unwrap()));
        }
        _ => {}
    }

    return Ok(FunctionArm::None);
}

pub fn parse_function_header(header_tokens: Vec<Token>, line: i32) -> FunctionHeader {
    let header_tokens = header_tokens.strip_spaces();

    if header_tokens.len() == 0 {
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
            Token::Function
            | Token::Modifier
            | Token::Constructor
            | Token::Receive
            | Token::Fallback => {
                if let FunctionHeaderState::None = state {
                    match token {
                        Token::Modifier => {
                            function_header.r#type = FunctionType::Modifier;
                        }
                        Token::Constructor => {
                            function_header.r#type = FunctionType::Constructor;
                        }
                        Token::Receive => {
                            function_header.r#type = FunctionType::Receive;
                        }
                        Token::Fallback => {
                            function_header.r#type = FunctionType::Fallback;
                        }
                        _ => {}
                    }
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
                            if header_tokens[0] == Token::Constructor {
                                if let FunctionHeaderState::Args | FunctionHeaderState::Modifiers =
                                    state
                                {
                                    process_modifier_call(
                                        &header_tokens,
                                        index,
                                        line,
                                        &mut function_header,
                                        _identifier,
                                        &mut pad,
                                        &mut state,
                                    );
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
                            process_modifier_call(
                                &header_tokens,
                                index,
                                line,
                                &mut function_header,
                                _identifier,
                                &mut pad,
                                &mut state,
                            );
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

                            parse_args_types(split, &mut function_header, line, ArgRet::Arg);
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

                            parse_args_types(split, &mut function_header, line, ArgRet::Ret);
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

fn process_modifier_call(
    header_tokens: &Vec<Token>,
    index: usize,
    line: i32,
    function_header: &mut FunctionHeader,
    _identifier: &String,
    pad: &mut usize,
    state: &mut FunctionHeaderState,
) {
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
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                }

                let raw_args = &header_tokens[index + 2..iteration + index + 1];
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
                                .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
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
                function_header
                    .modifiers
                    .as_mut()
                    .unwrap()
                    .push(ModifierCall {
                        arguments: modifier_args,
                        identifier: _identifier.to_owned(),
                    });

                *pad = index + iteration + 2;
                *state = FunctionHeaderState::Modifiers;
            }

            _ => {
                if function_header.modifiers.is_none() {
                    function_header.modifiers = Some(Vec::new());
                }
                function_header
                    .modifiers
                    .as_mut()
                    .unwrap()
                    .push(ModifierCall {
                        arguments: None,
                        identifier: _identifier.to_owned(),
                    });
                *state = FunctionHeaderState::Modifiers;
            }
        }
    } else {
        CompilerError::SyntaxError(SyntaxError::SyntaxError(
            "Unprocessible entity for function declaration",
        ))
        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
    }
}

fn parse_args_types(
    split: &Vec<Token>,
    function_header: &mut FunctionHeader,
    line: i32,
    arg_ret: ArgRet,
) {
    match arg_ret {
        ArgRet::Arg => match split.first().unwrap() {
            Token::Function => {
                let function_header_arg = parse_function_header(split.to_vec(), line);

                if function_header.arguments.is_none() {
                    function_header.arguments = Some(Vec::new());
                }
                function_header
                    .arguments
                    .as_mut()
                    .unwrap()
                    .push(ArgType::Function(FunctionHeader {
                        r#type: FunctionType::Variable,
                        ..function_header_arg
                    }));
            }
            Token::Mapping => {
                let mut new_split = split.clone();
                new_split.push(Token::SemiColon);
                let mut mapping = Mapping::new();
                let mut mapping_header = MappingHeader::new();
                let find_memory_index = new_split.iter().position(|pred| {
                    *pred == Token::Storage || *pred == Token::Memory || *pred == Token::Calldata
                });
                let mut mem_location: Option<Token> = None;
                if let Some(memory_index) = find_memory_index {
                    mem_location = Some(new_split[memory_index].clone());
                    new_split.remove(memory_index);
                }

                process_mapping(&new_split, &mut mapping, &mut mapping_header).unwrap_or_else(
                    |(err, _)| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "{} {}",
                            err,
                            split.to_vec().to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                    },
                );

                if function_header.arguments.is_none() {
                    function_header.arguments = Some(Vec::new());
                }
                function_header
                    .arguments
                    .as_mut()
                    .unwrap()
                    .push(ArgType::Mapping {
                        mem_location,
                        mapping: MappingAST {
                            header: mapping_header,
                            map: mapping,
                        },
                    });
            }

            _ => {
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
                    .push(ArgType::Variant(arg.unwrap()));
            }
        },

        ArgRet::Ret => match split.first().unwrap() {
            Token::Function => {
                let function_header_arg = parse_function_header(split.to_vec(), line);

                if function_header.returns.is_none() {
                    function_header.returns = Some(Vec::new());
                }
                function_header
                    .returns
                    .as_mut()
                    .unwrap()
                    .push(ArgType::Function(FunctionHeader {
                        r#type: FunctionType::Variable,
                        ..function_header_arg
                    }));
            }
            Token::Mapping => {
                let mut new_split = split.clone();
                new_split.push(Token::SemiColon);
                let mut mapping = Mapping::new();
                let mut mapping_header = MappingHeader::new();
                let find_memory_index = new_split.iter().position(|pred| {
                    *pred == Token::Storage || *pred == Token::Memory || *pred == Token::Calldata
                });
                let mut mem_location: Option<Token> = None;
                if let Some(memory_index) = find_memory_index {
                    mem_location = Some(new_split[memory_index].clone());
                    new_split.remove(memory_index);
                }

                process_mapping(&new_split, &mut mapping, &mut mapping_header).unwrap_or_else(
                    |(err, _)| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "{} {}",
                            err,
                            split.to_vec().to_string()
                        )))
                        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                    },
                );

                if function_header.returns.is_none() {
                    function_header.returns = Some(Vec::new());
                }
                function_header
                    .returns
                    .as_mut()
                    .unwrap()
                    .push(ArgType::Mapping {
                        mem_location,
                        mapping: MappingAST {
                            header: mapping_header,
                            map: mapping,
                        },
                    });
            }

            _ => {
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
                function_header
                    .returns
                    .as_mut()
                    .unwrap()
                    .push(ArgType::Variant(arg.unwrap()));
            }
        },
    }
}

fn prepare_function_body(raw_tokens: Vec<Token>, line: i32) -> Vec<FunctionArm> {
    let mut combined: Vec<Token> = Vec::new();
    let mut opened_context = 0;
    let mut opened_scope = 0;
    let mut result = Vec::new();

    for token in &raw_tokens {
        combined.push(token.clone());

        match token {
            Token::OpenBraces => {
                opened_scope += 1;
            }

            Token::CloseBraces => {
                opened_scope -= 1;

                if opened_scope == 0 {
                    result.push(process_function_body(&combined).unwrap_or_else(|err| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        unreachable!()
                    }));
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
                if opened_context == 0 && opened_scope == 0 {
                    result.push(process_function_body(&combined).unwrap_or_else(|err| {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                            .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                        unreachable!()
                    }));
                    combined.clear();
                }
            }

            _ => {}
        }
    }

    if opened_context != 0 || opened_scope != 0 {
        CompilerError::SyntaxError(SyntaxError::SyntaxError(
            "Unexpected context in function declaration",
        ))
        .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
    }

    if !combined.is_empty() {
        result.push(
            process_function_body(&combined[..combined.len()].to_vec()).unwrap_or_else(|err| {
                CompilerError::SyntaxError(SyntaxError::SyntaxError(err))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line);
                unreachable!()
            }),
        );
        combined.clear();
    }

    result
}
