use std::env;

use crate::mods::{
    constants::constants::FILE_PATH,
    errors::error::{CompilerError, SyntaxError},
    lexer::{
        lexer::{TTokenTrait, TVecExtension},
        tokens::Token,
    },
    parser::{
        custom_error::parse_custom_errors, event::parse_events, function::parse_functions,
        lib_implementation::parse_lib_implementations, r#enum::parse_enums,
        r#struct::parse_structs, variable::parse_variables,
    },
    utils::types::{
        context::{TContextFn, TerminationTypeContext, VariantContext},
        line_descriptors::{LineDescriptions, TStringDescriptor, TTokenDescriptor},
    },
};

use super::{global::validate_identifier, process_file_contents::process_file_contents};

pub async fn compile_source_code(args: Vec<String>) {
    let file_path = &args.last();
    let parsable_structure = process_file_contents(&args).await;
    env::set_var(FILE_PATH, file_path.unwrap());
    let mut imports: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut libraries: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut interfaces: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut contracts: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut custom_errors: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    seperate_variants(
        parsable_structure,
        &mut imports,
        &mut interfaces,
        &mut contracts,
        &mut libraries,
        &mut custom_errors,
    );

    for library in libraries {
        let (structs, vars, enums, functions, errors, events, lib_implementations, lib_header) =
            seperate_variant_variants(library, false);
        if lib_header.len() != 1 {
            let mut stringified_components = String::new();

            for comp in &lib_header {
                for lex in comp {
                    for tkn in &lex.data {
                        stringified_components.push_str(&tkn.to_string());
                    }
                }
            }
            CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                "Invalid declaration {}",
                stringified_components
            )))
            .throw_with_file_info(
                &std::env::var(FILE_PATH).unwrap(),
                lib_header.first().unwrap().first().unwrap().line,
            );
        }
        let mut lib_identifier = String::new();
        /* VALIDATE HEADER */
        for lexem in lib_header {
            {
                let mut header_tokens: Vec<&Token> = Vec::new();
                let header_line = lexem.first().unwrap().line;
                let mut should_break = false;
                for lex in &lexem {
                    if should_break {
                        break;
                    }
                    for token in &lex.data {
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
                    if let Token::Identifier(identifier) =
                        header_tokens.strip_spaces().last().unwrap()
                    {
                        validate_identifier(&identifier).unwrap_or_else(|err| {
                            CompilerError::SyntaxError(SyntaxError::SyntaxError(&err))
                                .throw_with_file_info(
                                    &std::env::var(FILE_PATH).unwrap(),
                                    header_line,
                                )
                        });
                        lib_identifier = identifier.to_owned();
                    } else {
                        CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                            "Expecting identifier but found {}",
                            header_tokens.strip_spaces().last().unwrap().to_string()
                        )))
                        .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), header_line)
                    }
                }
            }
        }

        let _structs = parse_structs(structs);
        let _ = parse_enums(enums);

        let _errs = parse_custom_errors(errors);
        let _events = parse_events(events);
        let _ = parse_lib_implementations(lib_implementations);
        let _ret = parse_variables(vars);
        // println!("{:#?}", functions);
        parse_functions(functions);
        // println!("{:#?}", _structs);
        // println!(
        //     "STRUCTS=>{:#?}\n\nVARS=>{:#?}\n\nENUMS=>{:#?}\n\nFUNCTIONS=>{:#?}\n\nERRORS=>{:#?}\n\nIMPL=>{:#?}\n\nHEADER=>{:#?}\n\n\n\n\n\n\n\n\n\n\n\n\n\n",
        //     structs, vars, enums, functions, errors, lib_implementations, lib_header
        // )
    }

    // for contract in contracts {
    //     let (structs, vars, enums, functions, errors, lib_implementations, contract_header) =
    //         seperate_variant_variants(contract);

    //     println!(
    //         "STRUCTS=>{:#?}\n\nVARS=>{:#?}\n\nENUMS=>{:#?}\n\nFUNCTIONS=>{:#?}\n\nERRORS=>{:#?}\n\nIMPL=>{:#?}\n\nHEADER=>{:#?}\n\n\n\n\n\n\n\n\n\n\n\n\n\n",
    //         structs, vars, enums, functions, errors, lib_implementations, lib_header
    //     )
    // }

    for interface in interfaces {
        // let (structs, vars, enums, functions, errors, lib_implementations, lib_header) =
        //     seperate_variant_variants(interface, true);

        // println!(
        //     "STRUCTS=>{:#?}\n\nVARS=>{:#?}\n\nENUMS=>{:#?}\n\nFUNCTIONS=>{:#?}\n\nERRORS=>{:#?}\n\nIMPL=>{:#?}\n\nHEADER=>{:#?}\n\n\n\n\n\n\n\n\n\n\n\n\n\n",
        //     structs, vars, enums, functions, errors, lib_implementations, lib_header
        // )
    }
}

/* SEPERATE SOLIDITY FILE VARIANTS LIKE LIBRARIES, INTERFACES, CUSTOM_ERRORS, CONTRACTS */
fn seperate_variants(
    parsable_structure: Vec<LineDescriptions<String>>,
    imports: &mut Vec<Vec<LineDescriptions<Vec<Token>>>>,
    interfaces: &mut Vec<Vec<LineDescriptions<Vec<Token>>>>,
    contracts: &mut Vec<Vec<LineDescriptions<Vec<Token>>>>,
    libraries: &mut Vec<Vec<LineDescriptions<Vec<Token>>>>,
    custom_errors: &mut Vec<Vec<LineDescriptions<Vec<Token>>>>,
) {
    let mut is_import_brace = false;
    let mut opened_braces_count = 0;
    let mut tokens: Vec<Token> = Vec::new();
    let mut combined: Vec<LineDescriptions<Vec<Token>>> = Vec::new();
    let mut context = VariantContext::None;
    for (parent_index, line_desc) in parsable_structure.iter().enumerate() {
        let lexems = line_desc.lex();
        for (index, token) in lexems.data.iter().enumerate() {
            tokens.push(token.clone());
            match token {
                Token::Pragma => {
                    if parent_index > 0 {
                        validate_clash(
                            context,
                            &tokens,
                            &parsable_structure.get(parent_index - 1),
                            None,
                        );
                    }
                    context = VariantContext::Header;
                }
                Token::Error => {
                    if opened_braces_count == 0 {
                        if parent_index > 0 {
                            validate_clash(
                                context,
                                &tokens,
                                &parsable_structure.get(parent_index - 1),
                                None,
                            );
                        }

                        context = VariantContext::Error;
                    }
                }
                Token::Abstract => {
                    if parent_index > 0 {
                        validate_clash(
                            context,
                            &tokens,
                            &parsable_structure.get(parent_index - 1),
                            None,
                        );
                    }

                    context = VariantContext::Contract;
                }
                Token::Library => {
                    if parent_index > 0 {
                        validate_clash(
                            context,
                            &tokens,
                            &parsable_structure.get(parent_index - 1),
                            None,
                        );
                    }

                    context = VariantContext::Library;
                }
                Token::Import => {
                    if parent_index > 0 {
                        validate_clash(
                            context,
                            &tokens,
                            &parsable_structure.get(parent_index - 1),
                            None,
                        );
                    }

                    context = VariantContext::Import;
                }

                Token::Interface => {
                    if parent_index > 0 {
                        validate_clash(
                            context,
                            &tokens,
                            &parsable_structure.get(parent_index - 1),
                            None,
                        );
                    }

                    context = VariantContext::Interface;
                }
                Token::Contract => {
                    if context != VariantContext::None {
                        if !tokens.is_empty() {
                            if tokens.strip_spaces()[0] != Token::Abstract {
                                validate_clash(context, &tokens, &Some(&lexems.to_string()), None);
                            }
                        }
                    }
                    context = VariantContext::Contract;
                }

                Token::SemiColon => {
                    if opened_braces_count == 0 {
                        if !tokens.is_empty() && context != VariantContext::Header {
                            combined.push(LineDescriptions {
                                data: tokens.clone(),
                                line: lexems.line,
                            });
                            tokens.clear();
                        }
                        match context {
                            VariantContext::Import => {
                                imports.push(combined.clone());
                                combined.clear();
                            }
                            VariantContext::Header => {
                                tokens.clear();
                            }

                            VariantContext::Error => {
                                custom_errors.push(combined.clone());
                                combined.clear();
                            }

                            _ => {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info(
                                    &std::env::var(FILE_PATH).unwrap(),
                                    lexems.line,
                                );
                            }
                        }
                        context = VariantContext::None;
                    }
                }

                Token::OpenBraces => {
                    if index > 0 {
                        let stripped = lexems.data.strip_spaces();
                        let prev = stripped.first();
                        if prev.is_some() && *prev.unwrap() == Token::Import {
                            is_import_brace = true;
                        } else {
                            opened_braces_count += 1;
                        }
                    } else {
                        if !combined.is_empty() && context != VariantContext::None {
                            let stripped = combined.last().unwrap().data.strip_spaces();
                            let prev = stripped.first();
                            if prev.is_some() && *prev.unwrap() == Token::Import {
                                is_import_brace = true;
                            } else {
                                opened_braces_count += 1;
                            }
                        } else {
                            CompilerError::SyntaxError(SyntaxError::UnexpectedToken("{"))
                                .throw_with_file_info(
                                    &std::env::var(FILE_PATH).unwrap(),
                                    lexems.line,
                                );
                        }
                    }
                }
                Token::CloseBraces => {
                    if opened_braces_count == 0 && !is_import_brace {
                        CompilerError::SyntaxError(SyntaxError::MissingToken("{"))
                            .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), lexems.line);
                    }
                    if !is_import_brace {
                        opened_braces_count -= 1;
                        if opened_braces_count == 0 {
                            if !tokens.is_empty() {
                                combined.push(LineDescriptions {
                                    data: tokens.clone(),
                                    line: lexems.line,
                                });
                                tokens.clear();
                            }
                            match context {
                                VariantContext::Library => {
                                    libraries.push(combined.clone());
                                    combined.clear();
                                }
                                VariantContext::Interface => {
                                    interfaces.push(combined.clone());
                                    combined.clear();
                                }

                                VariantContext::Contract => {
                                    contracts.push(combined.clone());
                                    combined.clear();
                                }
                                _ => {
                                    unreachable!()
                                }
                            }
                            context = VariantContext::None;
                        }
                    } else {
                        is_import_brace = false;
                    }
                }

                Token::Space => {}

                _ => {
                    if opened_braces_count == 0 {
                        match context {
                            VariantContext::Import
                            | VariantContext::Header
                            | VariantContext::Error => {}

                            _ => match context {
                                VariantContext::Contract | VariantContext::Interface => {
                                    if token.is_keyword() {
                                        if *token != Token::Is {
                                            CompilerError::SyntaxError(
                                                SyntaxError::UnexpectedToken(&format!(
                                                    "{}. Expecting {}",
                                                    token.to_string(),
                                                    "{"
                                                )),
                                            )
                                            .throw_with_file_info(
                                                &std::env::var(FILE_PATH).unwrap(),
                                                lexems.line,
                                            );
                                        }
                                    } else if token.is_symbol() {
                                        if *token != Token::Coma {
                                            CompilerError::SyntaxError(
                                                SyntaxError::UnexpectedToken(&format!(
                                                    "{}. Expecting {}",
                                                    token.to_string(),
                                                    "{"
                                                )),
                                            )
                                            .throw_with_file_info(
                                                &std::env::var(FILE_PATH).unwrap(),
                                                lexems.line,
                                            );
                                        }
                                    } else {
                                        if tokens.contains(&Token::Contract)
                                            || tokens.contains(&Token::Interface)
                                        {
                                            let stripped_spaces = tokens.strip_spaces();
                                            if let Token::Identifier(_) = token {
                                                if stripped_spaces[stripped_spaces.len() - 2]
                                                    != Token::Interface
                                                    && stripped_spaces[stripped_spaces.len() - 2]
                                                        != Token::Contract
                                                    && stripped_spaces[stripped_spaces.len() - 2]
                                                        != Token::Coma
                                                    && stripped_spaces[stripped_spaces.len() - 2]
                                                        != Token::Is
                                                {
                                                    CompilerError::SyntaxError(
                                                        SyntaxError::UnexpectedToken(&format!(
                                                            "{}. Expecting {}",
                                                            token.to_string(),
                                                            "{"
                                                        )),
                                                    )
                                                    .throw_with_file_info(
                                                        &std::env::var(FILE_PATH).unwrap(),
                                                        lexems.line,
                                                    );
                                                }
                                            } else if let Token::Is | Token::Coma = token {
                                                // TODO: NOTHING
                                            } else {
                                                CompilerError::SyntaxError(
                                                    SyntaxError::UnexpectedToken(&format!(
                                                        "{}. Expecting {}",
                                                        token.to_string(),
                                                        "{"
                                                    )),
                                                )
                                                .throw_with_file_info(
                                                    &std::env::var(FILE_PATH).unwrap(),
                                                    lexems.line,
                                                );
                                            }
                                        } else {
                                            let last_token =
                                                combined.last().unwrap().data.last().unwrap();
                                            if *last_token != Token::Interface
                                                && *last_token != Token::Contract
                                                && *last_token != Token::Coma
                                                && *last_token != Token::Is
                                            {
                                                CompilerError::SyntaxError(
                                                    SyntaxError::UnexpectedToken(&format!(
                                                        "{}. Expecting {}",
                                                        token.to_string(),
                                                        "{"
                                                    )),
                                                )
                                                .throw_with_file_info(
                                                    &std::env::var(FILE_PATH).unwrap(),
                                                    lexems.line,
                                                );
                                            } else {
                                                for (index, tkn) in
                                                    tokens.strip_spaces().iter().enumerate()
                                                {
                                                    match tkn {
                                                        Token::Coma => {}
                                                        Token::Identifier(_) => {
                                                            if index > 0 {
                                                                if *tokens.get(index - 1).unwrap()
                                                                    != Token::Coma
                                                                {
                                                                    CompilerError::SyntaxError(
                                                                        SyntaxError::UnexpectedToken(&format!(
                                                                            "{}. Expecting {}",
                                                                            token.to_string(),
                                                                            "{"
                                                                        )),
                                                                    )
                                                                    .throw_with_file_info(
                                                                        &std::env::var(FILE_PATH).unwrap(),
                                                                        lexems.line,
                                                                    );
                                                                }
                                                            }
                                                        }
                                                        _ => {
                                                            CompilerError::SyntaxError(
                                                                SyntaxError::UnexpectedToken(
                                                                    &format!(
                                                                        "{}. Expecting {}",
                                                                        tkn.to_string(),
                                                                        "{"
                                                                    ),
                                                                ),
                                                            )
                                                            .throw_with_file_info(
                                                                &std::env::var(FILE_PATH).unwrap(),
                                                                lexems.line,
                                                            );
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }

                                VariantContext::Library => {
                                    if token.is_keyword() || token.is_symbol() {
                                        CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                            &format!("{}. Expecting {}", token.to_string(), "{"),
                                        ))
                                        .throw_with_file_info(
                                            &std::env::var(FILE_PATH).unwrap(),
                                            lexems.line,
                                        );
                                    } else {
                                        if let Token::Identifier(_) = token {
                                            if tokens.contains(&Token::Library) {
                                                let mut identifier_count = 0;
                                                for tkn in &tokens {
                                                    match tkn {
                                                        Token::Identifier(_) => {
                                                            if identifier_count > 0 {
                                                                CompilerError::SyntaxError(
                                                                    SyntaxError::UnexpectedToken(
                                                                        &format!(
                                                                            "{}. Expecting {}",
                                                                            token.to_string(),
                                                                            "{"
                                                                        ),
                                                                    ),
                                                                )
                                                                .throw_with_file_info(
                                                                    &std::env::var(FILE_PATH)
                                                                        .unwrap(),
                                                                    lexems.line,
                                                                );
                                                            } else {
                                                                identifier_count += 1;
                                                            }
                                                        }
                                                        _ => {}
                                                    }
                                                }
                                            } else {
                                                if let Token::Identifier(_) = combined
                                                    .last()
                                                    .unwrap()
                                                    .data
                                                    .strip_spaces()
                                                    .last()
                                                    .unwrap()
                                                {
                                                    CompilerError::SyntaxError(
                                                        SyntaxError::UnexpectedToken(&format!(
                                                            "{}. Expecting {}",
                                                            token.to_string(),
                                                            "{"
                                                        )),
                                                    )
                                                    .throw_with_file_info(
                                                        &std::env::var(FILE_PATH).unwrap(),
                                                        lexems.line,
                                                    );
                                                }
                                            }
                                        }
                                    }
                                }

                                _ => {}
                            },
                        }
                    }
                }
            }

            if let VariantContext::None = context {
                if !tokens.strip_spaces().is_empty() {
                    CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                        &tokens.strip_spaces()[0].to_string(),
                    ))
                    .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), lexems.line);
                }
            }
        }

        if !tokens.is_empty() {
            combined.push(LineDescriptions {
                line: lexems.line,
                data: tokens.clone(),
            });
            tokens.clear();
        }
    }

    if context != VariantContext::None {
        CompilerError::SyntaxError(SyntaxError::MissingToken(match context {
            VariantContext::Contract | VariantContext::Interface | VariantContext::Library => "}",
            _ => ";",
        }))
        .throw_with_file_info(
            &std::env::var(FILE_PATH).unwrap(),
            combined.last().unwrap().line,
        );
    }
}

/* VALIDATES CLASH DUE TO MISSING TOKEN E.G ";" OR "}" */
fn validate_clash<T: TContextFn>(
    context: T,
    tokens: &Vec<Token>,
    lexems: &Option<&LineDescriptions<String>>,
    opened_braces_count: Option<i32>,
) {
    context.validate_clash(tokens, lexems, opened_braces_count);
}

fn seperate_variant_variants(
    line_desc: Vec<LineDescriptions<Vec<Token>>>,
    is_interface: bool,
) -> (
    Vec<Vec<LineDescriptions<Vec<Token>>>>,
    Vec<Vec<LineDescriptions<Vec<Token>>>>,
    Vec<Vec<LineDescriptions<Vec<Token>>>>,
    Vec<Vec<LineDescriptions<Vec<Token>>>>,
    Vec<Vec<LineDescriptions<Vec<Token>>>>,
    Vec<Vec<LineDescriptions<Vec<Token>>>>,
    Vec<Vec<LineDescriptions<Vec<Token>>>>,
    Vec<Vec<LineDescriptions<Vec<Token>>>>,
) {
    let mut opened_braces_count = 0;
    let mut terminator_type = TerminationTypeContext::None;
    let mut structs: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut vars: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut enums: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut functions: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut errors: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut events: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut lib_implementations: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut lib_header: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut tokens: Vec<Token> = Vec::new();
    let mut combined: Vec<LineDescriptions<Vec<Token>>> = Vec::new();

    // let mut is_mapping = false;
    for (parent_index, _line_desc) in line_desc.iter().enumerate() {
        for (index, token) in _line_desc.data.iter().enumerate() {
            tokens.push(token.clone());
            match token {
                Token::Struct => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                            Some(opened_braces_count),
                        )
                    }
                    terminator_type = TerminationTypeContext::Struct
                }
                Token::Enum => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                            Some(opened_braces_count),
                        )
                    }
                    terminator_type = TerminationTypeContext::Enum;
                }
                Token::Function
                | Token::Receive
                | Token::Fallback
                | Token::Constructor
                | Token::Modifier => {
                    if TerminationTypeContext::Struct != terminator_type {
                        // if is_interface {
                        //     terminator_type = TerminationTypeContext::Variable
                        // } else {
                        terminator_type = TerminationTypeContext::Function
                        // }
                    }
                }
                Token::Error => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                            Some(opened_braces_count),
                        )
                    }
                    terminator_type = TerminationTypeContext::Error
                }
                Token::Using => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                            Some(opened_braces_count),
                        )
                    }
                    terminator_type = TerminationTypeContext::Implementation
                }
                Token::Uint(_)
                | Token::Int(_)
                | Token::Bool
                | Token::Bytes(_)
                | Token::Address
                | Token::String
                | Token::Identifier(_) => {
                    if opened_braces_count == 1 && terminator_type == TerminationTypeContext::None {
                        if parent_index > 0 {
                            validate_clash(
                                terminator_type,
                                &tokens,
                                &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                                Some(opened_braces_count),
                            )
                        }
                        terminator_type = TerminationTypeContext::Variable
                    }
                }
                Token::Mapping => {
                    if let TerminationTypeContext::None = terminator_type {
                        //TODO: NOTHING
                    } else {
                        if index > 0 {
                            let mut space_count = 0;
                            for c in &_line_desc.data[..index] {
                                match c {
                                    Token::Space => space_count += 1,
                                    _ => {}
                                }
                            }
                            let _raw = _line_desc.data.strip_spaces();
                            let find = _raw.get(index - (1 + space_count));
                            if let Some(_tkn) = find {
                                match _tkn {
                                    Token::Gt => (),
                                    _ => match terminator_type {
                                        TerminationTypeContext::Function => {}
                                        _ => {
                                            CompilerError::SyntaxError(SyntaxError::MissingToken(
                                                ";",
                                            ))
                                            .throw_with_file_info(
                                                &std::env::var(FILE_PATH).unwrap(),
                                                _line_desc.line,
                                            );
                                        }
                                    },
                                }
                            }
                        } else {
                            if parent_index > 0 && opened_braces_count == 1 {
                                CompilerError::SyntaxError(SyntaxError::MissingToken(";"))
                                    .throw_with_file_info(
                                        &std::env::var(FILE_PATH).unwrap(),
                                        line_desc.get(parent_index - 1).unwrap().line,
                                    );
                            }
                        }
                    }
                    if opened_braces_count == 1 {
                        match terminator_type {
                            TerminationTypeContext::Function => {}
                            _ => terminator_type = TerminationTypeContext::Variable,
                        }
                    }
                }

                Token::Event => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                            Some(opened_braces_count),
                        )
                    }
                    terminator_type = TerminationTypeContext::Event
                }

                Token::SemiColon => {
                    // is_mapping = false;
                    if opened_braces_count == 1 {
                        if !tokens.is_empty() {
                            combined.push(LineDescriptions {
                                data: tokens.clone(),
                                line: _line_desc.line,
                            });
                            tokens.clear();
                        }
                        match terminator_type {
                            TerminationTypeContext::Variable => {
                                if is_interface {
                                    functions.push(combined.clone());
                                    combined.clear();
                                } else {
                                    vars.push(combined.clone());
                                    combined.clear();
                                }
                            }

                            TerminationTypeContext::Error => {
                                errors.push(combined.clone());
                                combined.clear();
                            }

                            TerminationTypeContext::Function => {
                                vars.push(combined.clone());
                                combined.clear();
                            }
                            TerminationTypeContext::Event => {
                                events.push(combined.clone());
                                combined.clear();
                            }
                            TerminationTypeContext::Implementation => {
                                lib_implementations.push(combined.clone());
                                combined.clear();
                            }

                            _ => {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info(
                                    &std::env::var(FILE_PATH).unwrap(),
                                    _line_desc.line,
                                );
                            }
                        }
                        terminator_type = TerminationTypeContext::None;
                    }
                }
                Token::OpenBraces => {
                    if index > 0 {
                        opened_braces_count += 1;
                        if opened_braces_count == 1 {
                            combined.push(LineDescriptions {
                                data: tokens.clone(),
                                line: _line_desc.line,
                            });
                            tokens.clear();

                            lib_header.push(combined.clone());
                            combined.clear();
                        } else {
                            combined.push(LineDescriptions {
                                data: tokens.clone(),
                                line: _line_desc.line,
                            });
                            tokens.clear();
                        }
                    } else {
                        if !combined.is_empty() && terminator_type != TerminationTypeContext::None {
                            opened_braces_count += 1;
                            if opened_braces_count == 1 {
                                combined.push(LineDescriptions {
                                    data: tokens.clone(),
                                    line: _line_desc.line,
                                });
                                tokens.clear();

                                lib_header.push(combined.clone());
                                combined.clear();
                            }
                        } else {
                            opened_braces_count += 1;
                            if opened_braces_count == 1 {
                                combined.push(LineDescriptions {
                                    data: tokens.clone(),
                                    line: _line_desc.line,
                                });
                                tokens.clear();

                                lib_header.push(combined.clone());
                                combined.clear();
                            } else {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken("{"))
                                    .throw_with_file_info(
                                        &std::env::var(FILE_PATH).unwrap(),
                                        _line_desc.line,
                                    );
                            }
                        }
                    }
                }

                Token::CloseBraces => {
                    opened_braces_count -= 1;
                    if opened_braces_count == 1 {
                        match terminator_type {
                            TerminationTypeContext::Struct => {
                                if !tokens.is_empty() {
                                    combined.push(LineDescriptions {
                                        data: tokens.clone(),
                                        line: _line_desc.line,
                                    });
                                    tokens.clear();
                                }
                                structs.push(combined.clone());
                                combined.clear();
                                terminator_type = TerminationTypeContext::None;
                            }
                            TerminationTypeContext::Enum => {
                                if !tokens.is_empty() {
                                    combined.push(LineDescriptions {
                                        data: tokens.clone(),
                                        line: _line_desc.line,
                                    });
                                    tokens.clear();
                                }
                                enums.push(combined.clone());
                                combined.clear();
                                terminator_type = TerminationTypeContext::None;
                            }

                            TerminationTypeContext::Function => {
                                if !tokens.is_empty() {
                                    combined.push(LineDescriptions {
                                        data: tokens.clone(),
                                        line: _line_desc.line,
                                    });
                                    tokens.clear();
                                }
                                functions.push(combined.clone());
                                combined.clear();
                                terminator_type = TerminationTypeContext::None;
                            }
                            TerminationTypeContext::Variable => {}
                            _other => {
                                if !tokens.is_empty() {
                                    combined.push(LineDescriptions {
                                        data: tokens.clone(),
                                        line: _line_desc.line,
                                    });
                                    tokens.clear();
                                }
                                let mut stringified_error = String::new();
                                for _combined in &combined {
                                    stringified_error.push_str(&_combined.data.to_string());
                                }
                                CompilerError::SyntaxError(SyntaxError::SyntaxError(
                                    &stringified_error,
                                ))
                                .throw_with_file_info(
                                    &std::env::var(FILE_PATH).unwrap(),
                                    combined.first().unwrap().line,
                                );
                                terminator_type = TerminationTypeContext::None;
                            }
                        }
                    } else if opened_braces_count == 0 {
                        if tokens.len() == 1 {
                            tokens.clear();
                        } else {
                            tokens.remove(tokens.len() - 1);
                        }
                    }
                }

                _ => {
                    //TODO: NOTHING
                }
            }

            if let TerminationTypeContext::None = terminator_type {
                if !tokens.strip_spaces().is_empty() {
                    let first_token = tokens.first();
                    match first_token {
                        None => {
                            CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                &tokens.strip_spaces()[0].to_string(),
                            ))
                            .throw_with_file_info(
                                &std::env::var(FILE_PATH).unwrap(),
                                _line_desc.line,
                            );
                        }
                        Some(initial) => {
                            if opened_braces_count > 0 {
                                match initial {
                                    Token::Library
                                    | Token::Contract
                                    | Token::Interface
                                    | Token::Modifier => {}
                                    _ => {
                                        CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                            &tokens.strip_spaces()[0].to_string(),
                                        ))
                                        .throw_with_file_info(
                                            &std::env::var(FILE_PATH).unwrap(),
                                            _line_desc.line,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if !tokens.is_empty() {
            combined.push(LineDescriptions {
                line: _line_desc.line,
                data: tokens.clone(),
            });
            tokens.clear();
        }
    }

    assert!(opened_braces_count == 0, "Missing {}", "}");

    if terminator_type != TerminationTypeContext::None {
        CompilerError::SyntaxError(SyntaxError::MissingToken(match terminator_type {
            TerminationTypeContext::Struct
            | TerminationTypeContext::Function
            | TerminationTypeContext::Enum => "}",
            _ => ";",
        }))
        .throw_with_file_info(
            &std::env::var(FILE_PATH).unwrap(),
            combined.last().unwrap().line,
        );
    }

    (
        structs,
        vars,
        enums,
        functions,
        errors,
        events,
        lib_implementations,
        lib_header,
    )
}
