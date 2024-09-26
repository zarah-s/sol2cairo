#[derive(Debug)]
pub struct Arg {
    pub r#type: String,
    pub name: Option<String>,
    pub array_size: Option<String>,
    pub is_array: bool,
}

#[derive(Debug)]
pub struct EventVariants {
    pub indexed: bool,
    pub variant: Arg,
}

#[derive(Debug)]
pub struct EventAST {
    pub line: String,
    pub identifier: String,
    pub variants: Vec<EventVariants>,
}
