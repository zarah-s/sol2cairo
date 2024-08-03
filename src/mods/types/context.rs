use crate::mods::constants::constants::FILE_PATH;

use super::{
    compiler_errors::{CompilerError, SyntaxError},
    line_descriptors::{LineDescriptions, TStringDescriptor},
    token::Token,
};

#[derive(PartialEq, Debug)]
pub enum VariantContext {
    Import,
    Interface,
    Library,
    Header,
    Contract,
    Error,
    None,
}

#[derive(PartialEq, Debug)]
pub enum TerminationTypeContext {
    None,
    Struct,
    Enum,
    Variable,
    Implementation,
    Function,
    Error,
}

pub trait TContextFn {
    fn validate_clash(
        &self,
        tokens: &Vec<Token>,
        lexems: &Option<&LineDescriptions<String>>,
        opened_braces_count: Option<i32>,
    );
}

impl TContextFn for VariantContext {
    fn validate_clash(
        &self,
        tokens: &Vec<Token>,
        lexems: &Option<&LineDescriptions<String>>,
        _: Option<i32>,
    ) {
        if let Some(_lexems) = lexems {
            if *self != Self::None && !tokens.is_empty() {
                CompilerError::SyntaxError(SyntaxError::MissingToken(match self {
                    Self::Contract | Self::Interface | Self::Library => "}",
                    _ => ";",
                }))
                .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), _lexems.lex().line);
            }
        } else {
            CompilerError::InternalError("Unprocessible entity").throw();
        }
    }
}

impl TContextFn for TerminationTypeContext {
    fn validate_clash(
        &self,
        tokens: &Vec<Token>,
        lexems: &Option<&LineDescriptions<String>>,
        opened_braces_count: Option<i32>,
    ) {
        if let Some(_lexems) = lexems {
            if opened_braces_count.unwrap() != 1 {
                CompilerError::SyntaxError(SyntaxError::MissingToken("{"))
                    .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), _lexems.line);
            }
            if *self != Self::None && !tokens.is_empty() {
                CompilerError::SyntaxError(SyntaxError::MissingToken(match self {
                    Self::Struct | Self::Enum | Self::Function => "}",
                    _ => ";",
                }))
                .throw_with_file_info(&std::env::var(FILE_PATH).unwrap(), _lexems.lex().line);
            }
        } else {
            CompilerError::InternalError("Unprocessible entity").throw();
        }
    }
}
