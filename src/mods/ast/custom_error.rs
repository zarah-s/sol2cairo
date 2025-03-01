use crate::mods::utils::types::variant::Variant;

#[derive(Debug)]
pub struct CustomErrorAST {
    pub identifier: String,
    pub line: String,
    pub args: Option<Vec<Variant>>,
}
