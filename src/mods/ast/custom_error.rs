#[derive(Debug)]
pub struct Arg {
    pub r#type: String,
    pub name: Option<String>,
    pub array_size: Option<String>,
    pub is_array: bool,
    pub payable: bool,
}

#[derive(Debug)]
pub struct CustomErrorAST {
    pub identifier: String,
    pub line: String,
    pub args: Option<Vec<Arg>>,
}
