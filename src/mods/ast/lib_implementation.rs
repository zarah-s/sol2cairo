#[derive(Debug)]
pub struct LibraryImplementation {
    pub library_identifier: String,
    pub line: String,
    pub data_type: String,
    pub is_array: bool,
    pub array_size: Option<String>,
}

pub enum LibImplState {
    None,
    Declaration,
    LibDefinition,
    For,
    DataType,
}
