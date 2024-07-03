use crate::mods::types::token::Token;

pub fn extract_data_type_from_token(token: &Token) -> Option<&Token> {
    match token {
        Token::Uint(_)
        | Token::Int(_)
        | Token::Bool
        | Token::Bytes(_)
        | Token::Address
        | Token::String => Some(token),

        _ => None,
    }
}
