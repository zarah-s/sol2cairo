#[derive(Debug)]
pub struct EnumAST {
    pub identifier: String,
    pub line: String,
    pub variants: Vec<String>,
}
