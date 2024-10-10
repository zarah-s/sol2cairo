use crate::mods::{
    ast::custom_error::CustomErrorAST,
    constants::constants::FILE_PATH,
    errors::error::{CompilerError, SyntaxError},
    utils::{
        functions::global::{get_env_vars, validate_identifier},
        types::{
            line_descriptors::LineDescriptions,
            variant::{TVariant, Variant},
        },
    },
};

use crate::mods::lexer::{
    lexer::{TTokenTrait, TVecExtension},
    tokens::Token,
};

pub fn parse_custom_errors(lexems: Vec<Vec<LineDescriptions<Vec<Token>>>>) -> Vec<CustomErrorAST> {
    let mut custom_errors = Vec::new();

    for lexem in lexems {
        let mut error_identifier = String::new();
        /* SANITY CHECKS */

        {
            if lexem.is_empty() {
                continue;
            }

            if lexem.first().unwrap().data.is_empty() {
                continue;
            }
            let first_element = lexem.first().unwrap().data.first().unwrap();

            if *first_element != Token::Error {
                CompilerError::InternalError(&format!(
                    "Expecting error but found {}",
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
                    error_identifier = identifier.to_owned();
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
            let mut arguments: Vec<Variant> = Vec::new();
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

            let stripped = &raw_args[1..&raw_args.len() - 2].to_vec().split_coma();

            for combined in stripped {
                let arg = Variant::process_args(combined);
                if arg.is_err() {
                    CompilerError::SyntaxError(SyntaxError::SyntaxError(&format!(
                        "{} {}",
                        arg.as_ref().err().unwrap(),
                        raw_args.to_string()
                    )))
                    .throw_with_file_info(&get_env_vars(FILE_PATH).unwrap(), line)
                }
                arguments.push(arg.unwrap());
            }

            let error_construct = CustomErrorAST {
                identifier: error_identifier,
                line: lexem[0].line.to_string(),
                args: if arguments.is_empty() {
                    None
                } else {
                    Some(arguments)
                },
            };

            custom_errors.push(error_construct);
        }
    }

    custom_errors
}
