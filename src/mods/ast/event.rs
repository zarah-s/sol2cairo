use crate::mods::utils::types::variant::Variant;

#[derive(Debug)]
pub struct EventAST {
    pub line: String,
    pub anonymous: bool,
    pub identifier: String,
    pub variants: Option<Vec<Variant>>,
}

impl EventAST {
    pub fn new() -> Self {
        Self {
            line: String::new(),
            anonymous: false,
            identifier: String::new(),
            variants: None,
        }
    }
}
