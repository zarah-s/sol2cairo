use crate::mods::utils::types::variant::Variant;

#[derive(Debug)]
pub struct EventAST {
    pub line: String,
    pub identifier: String,
    pub variants: Vec<Variant>,
}
