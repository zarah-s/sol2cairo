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

// pub mod parser {
//     pub mod
// }
// pub mod parser {
//     pub mod custom_error;
// pub mod r#enum;
// pub mod event;
// pub mod functions;
// pub mod lib_implementation;
// pub mod mapping;
// pub mod r#struct;
// pub mod variable;
// }

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

// pub mod types {
//     pub mod compiler_errors;
//     // pub mod context;
//     // pub mod line_descriptors;
// }

pub mod constants {
    pub mod constants;
}

pub mod functions {
    pub mod helpers {
        // pub mod error_helper;
        // pub mod global;
        // pub mod token_helper;
    }

    pub mod controllers {

        // pub mod process_enum;
        // pub mod process_file_contents;
        // pub mod process_function;
        // pub mod process_state_variables;
        // pub mod process_struct;
        // pub mod parser;
        // pub mod strip_comments;
        // pub mod structure_to_line_descriptors;
        // pub mod sub_main;
    }
}

pub mod implementations {
    // pub mod conditionals;
    // pub mod line_descriptors;
    // pub mod mapping;
}
