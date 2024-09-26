pub mod lexer {
    pub mod lexer;
    pub mod tokens;
}

pub mod utils {
    pub mod types {
        pub mod context;
        pub mod line_descriptors;
        pub mod mutability;
        pub mod value;
        pub mod visibility;
    }
    pub mod functions {
        pub mod global;
        pub mod process_file_contents;
        pub mod sub_main;
    }
}

pub mod errors {
    pub mod error;
}

pub mod ast {
    pub mod custom_error;
    pub mod r#enum;
    pub mod event;
    pub mod function;
    pub mod lib_implementation;
    pub mod mapping;
    pub mod r#struct;
    pub mod variable;
}

pub mod parser {
    pub mod custom_error;
    pub mod r#enum;
    pub mod event;
    pub mod function;
    pub mod lib_implementation;
    pub mod mapping;
    pub mod r#struct;
    pub mod variable;
}

pub mod constants {
    pub mod constants;
}
