use crate::mods::{
    functions::controllers::process_file_contents::process_file_contents,
    types::{
        compiler_errors::{CompilerError, SyntaxError},
        context::{ContextFn, TerminationTypeContext, VariantContext},
        line_descriptors::{LineDescriptions, StringDescriptor, TokenDescriptor},
        token::{Token, TokenTrait, VecExtension},
    },
};

pub async fn compile_source_code(args: Vec<String>) {
    let parsable_structure = process_file_contents(args).await;
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
        // let (structs, vars, enums, functions, errors, lib_implementations, lib_header) =
        //     seperate_variant_variants(library, false);

        // println!(
        //     "STRUCTS=>{:#?}\n\nVARS=>{:#?}\n\nENUMS=>{:#?}\n\nFUNCTIONS=>{:#?}\n\nERRORS=>{:#?}\n\nIMPL=>{:#?}\n\nHEADER=>{:#?}\n\n\n\n\n\n\n\n\n\n\n\n\n\n",
        //     structs, vars, enums, functions, errors, lib_implementations, lib_header
        // )
    }

    // for contract in contracts {
    //     let (structs, vars, enums, functions, errors, lib_implementations, lib_header) =
    //         seperate_variant_variants(contract);

    //     println!(
    //         "STRUCTS=>{:#?}\n\nVARS=>{:#?}\n\nENUMS=>{:#?}\n\nFUNCTIONS=>{:#?}\n\nERRORS=>{:#?}\n\nIMPL=>{:#?}\n\nHEADER=>{:#?}\n\n\n\n\n\n\n\n\n\n\n\n\n\n",
    //         structs, vars, enums, functions, errors, lib_implementations, lib_header
    //     )
    // }

    for interface in interfaces {
        let (structs, vars, enums, functions, errors, lib_implementations, lib_header) =
            seperate_variant_variants(interface, true);

        println!(
            "STRUCTS=>{:#?}\n\nVARS=>{:#?}\n\nENUMS=>{:#?}\n\nFUNCTIONS=>{:#?}\n\nERRORS=>{:#?}\n\nIMPL=>{:#?}\n\nHEADER=>{:#?}\n\n\n\n\n\n\n\n\n\n\n\n\n\n",
            structs, vars, enums, functions, errors, lib_implementations, lib_header
        )
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
                        validate_clash(context, &tokens, &parsable_structure.get(parent_index - 1));
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
                            );
                        }

                        context = VariantContext::Error;
                    }
                }
                Token::Abstract => {
                    if parent_index > 0 {
                        validate_clash(context, &tokens, &parsable_structure.get(parent_index - 1));
                    }

                    context = VariantContext::Contract;
                }
                Token::Library => {
                    if parent_index > 0 {
                        validate_clash(context, &tokens, &parsable_structure.get(parent_index - 1));
                    }

                    context = VariantContext::Library;
                }
                Token::Import => {
                    if parent_index > 0 {
                        validate_clash(context, &tokens, &parsable_structure.get(parent_index - 1));
                    }

                    context = VariantContext::Import;
                }

                Token::Interface => {
                    if parent_index > 0 {
                        validate_clash(context, &tokens, &parsable_structure.get(parent_index - 1));
                    }

                    context = VariantContext::Interface;
                }
                Token::Contract => {
                    if context != VariantContext::None {
                        if !tokens.is_empty() {
                            if tokens.strip_spaces()[0] != Token::Abstract {
                                validate_clash(context, &tokens, &Some(&lexems.to_string()));
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
                                .throw_with_file_info("Contract.sol", lexems.line);
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
                                .throw_with_file_info("Contract.sol", lexems.line);
                        }
                    }
                }
                Token::CloseBraces => {
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
                                _ => {}
                            }
                            context = VariantContext::None;
                        }
                    } else {
                        is_import_brace = false;
                    }
                }

                _ => {}
            }

            if let VariantContext::None = context {
                if !tokens.strip_spaces().is_empty() {
                    CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                        &tokens.strip_spaces()[0].to_string(),
                    ))
                    .throw_with_file_info("Contract.sol", lexems.line);
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
        .throw_with_file_info("Contract.sol", combined.last().unwrap().line);
    }
}

/* VALIDATES CLASH DUE TO MISSING TOKEN E.G ";" OR "}" */
fn validate_clash<T: ContextFn>(
    context: T,
    tokens: &Vec<Token>,
    lexems: &Option<&LineDescriptions<String>>,
) {
    context.validate_clash(tokens, lexems);
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
) {
    let mut opened_braces_count = 0;
    let mut terminator_type = TerminationTypeContext::None;
    let mut structs: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut vars: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut enums: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut functions: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut errors: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut lib_implementations: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut lib_header: Vec<Vec<LineDescriptions<Vec<Token>>>> = Vec::new();
    let mut tokens: Vec<Token> = Vec::new();
    let mut combined: Vec<LineDescriptions<Vec<Token>>> = Vec::new();

    for (parent_index, _line_desc) in line_desc.iter().enumerate() {
        for (index, token) in _line_desc.data.iter().enumerate() {
            tokens.push(token.clone());
            match token {
                Token::Struct => {
                    terminator_type = {
                        if parent_index > 0 {
                            validate_clash(
                                terminator_type,
                                &tokens,
                                &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                            )
                        }
                        TerminationTypeContext::Struct
                    }
                }
                Token::Enum => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                        )
                    }
                    terminator_type = TerminationTypeContext::Enum;
                }
                Token::Function | Token::Receive | Token::Fallback | Token::Constructor => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                        )
                    }
                    if is_interface {
                        terminator_type = TerminationTypeContext::Variable
                    } else {
                        terminator_type = TerminationTypeContext::Function
                    }
                }
                Token::Error => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
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
                            )
                        }
                        terminator_type = TerminationTypeContext::Variable
                    }
                }
                Token::Mapping => {
                    if parent_index > 0 {
                        validate_clash(
                            terminator_type,
                            &tokens,
                            &Some(&line_desc.get(parent_index - 1).unwrap().to_string()),
                        )
                    }
                    terminator_type = TerminationTypeContext::Variable
                }

                Token::SemiColon => {
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
                            TerminationTypeContext::Implementation => {
                                lib_implementations.push(combined.clone());
                                combined.clear();
                            }

                            _ => {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &token.to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", _line_desc.line);
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
                            CompilerError::SyntaxError(SyntaxError::UnexpectedToken("{"))
                                .throw_with_file_info("Contract.sol", _line_desc.line);
                        }
                    }
                }

                Token::CloseBraces => {
                    opened_braces_count -= 1;

                    if opened_braces_count == 1 {
                        if !tokens.is_empty() {
                            combined.push(LineDescriptions {
                                data: tokens.clone(),
                                line: _line_desc.line,
                            });
                            tokens.clear();
                        }
                        match terminator_type {
                            TerminationTypeContext::Struct => {
                                structs.push(combined.clone());
                                combined.clear();
                            }
                            TerminationTypeContext::Enum => {
                                enums.push(combined.clone());
                                combined.clear();
                            }

                            TerminationTypeContext::Function => {
                                functions.push(combined.clone());
                                combined.clear();
                            }
                            _ => {}
                        }
                        terminator_type = TerminationTypeContext::None;
                    } else if opened_braces_count == 0 {
                        if tokens.len() == 1 {
                            tokens.clear();
                        } else {
                            tokens.remove(tokens.len() - 1);
                        }
                    }
                }

                _ => {}
            }

            if let TerminationTypeContext::None = terminator_type {
                if !tokens.strip_spaces().is_empty() {
                    let first_token = tokens.first();
                    match first_token {
                        None => {
                            CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                &tokens.strip_spaces()[0].to_string(),
                            ))
                            .throw_with_file_info("Contract.sol", _line_desc.line);
                        }
                        Some(initial) => match initial {
                            Token::Library | Token::Contract | Token::Interface => {}
                            _ => {
                                CompilerError::SyntaxError(SyntaxError::UnexpectedToken(
                                    &tokens.strip_spaces()[0].to_string(),
                                ))
                                .throw_with_file_info("Contract.sol", _line_desc.line);
                            }
                        },
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
        .throw_with_file_info("Contract.sol", combined.last().unwrap().line);
    }
    (
        structs,
        vars,
        enums,
        functions,
        errors,
        lib_implementations,
        lib_header,
    )
}
