use crate::mods::{
    lexer::tokens::Token,
    utils::types::{value::Value, variant::Variant},
};

use super::{mapping::MappingAST, variable::VariableAST};

/// Distinguish between argument or return args for reusable function
#[derive(Debug)]
pub enum ArgRet {
    Arg,
    Ret,
}

#[derive(Debug)]
pub enum FunctionHeaderState {
    Keyword,
    Args,
    Visibility,
    Mutability,
    Inheritance,
    Modifiers,
    Identifier,
    Returns,
    Gasless,
    MemLocation,
    VarVisibility,
    VarName,
    Array,
    None,
}

#[derive(Debug)]
pub struct ModifierCall {
    pub identifier: String,
    pub arguments: Option<Vec<String>>,
}

#[derive(Debug)]
pub enum FunctionType {
    Variable,
    Interface,
    Modifier,
    Constructor,
    Receive,
    Fallback,
}

#[derive(Debug)]
pub struct FunctionHeader {
    pub name: Option<String>,
    pub gasless: bool,
    pub mutability: Option<Token>,
    pub visibility: Option<Token>,
    pub returns: Option<Vec<ArgType>>,
    /// Inheritance for virtual and override
    pub inheritance: Option<Token>,
    pub r#type: FunctionType,
    pub arguments: Option<Vec<ArgType>>,
    pub modifiers: Option<Vec<ModifierCall>>,
    /// for variable type function headers
    pub is_array: bool,
    pub size: Option<String>,
    pub location: Option<Token>,
    pub var_visibility: Option<Token>,
    pub var_name: Option<String>,
}

#[derive(Debug)]
pub enum ArgType {
    /// accepts function pointer as argument
    Function(FunctionHeader),
    /// normal function args
    Variant(Variant),

    /// accepts mapping pointer as argument
    Mapping {
        mem_location: Option<Token>,
        mapping: MappingAST,
    },
}

#[derive(Debug)]
pub struct EventEmitter {
    pub identifier: String,
    pub values: Vec<Value>,
}

#[derive(Debug)]
pub enum VariableAssignOperation {
    Push,
    Pop,
    Assign,
}

#[derive(Debug)]
pub struct MappingAssign {
    pub identifier: String,
    pub value: Option<Value>,
    pub variants: Vec<Value>,
    pub operation: VariableAssignOperation,
}

#[derive(Debug)]
pub struct Require {
    pub condition: Value,
    pub message: Option<Value>,
}

#[derive(Debug)]
pub struct TuppleAssignment {
    pub variables: Vec<VariableAST>,
    pub value: Vec<Value>,
}

#[derive(Debug)]
pub struct ElIf {
    pub condition: Vec<Token>,
    pub arm: Vec<FunctionArm>,
}

#[derive(Debug)]
pub struct Conditionals {
    pub condition: Value,
    pub arm: Vec<FunctionArm>,
    pub elif: Option<Vec<ElIf>>,
    pub el: Option<Vec<FunctionArm>>,
}

#[derive(Debug)]
pub struct Return {
    pub value: Value,
}

#[derive(Debug)]
pub struct Delete {
    pub identifier: String,
    pub variants: Option<Vec<Value>>,
}

#[derive(Debug)]
pub enum LoopType {
    For,
    While,
}

#[derive(Debug)]
pub struct Loop {
    pub initiator: VariableAST,
    pub condition: Value,
    pub iterator: Option<Value>,
    pub arms: Vec<FunctionArm>,
    pub r#type: LoopType,
}

#[derive(Debug)]
pub enum FunctionArm {
    VariableIdentifier(VariableAST),
    VariableAssign(VariableAST),
    EventEmitter(EventEmitter),
    MappingAssign(MappingAssign),
    TuppleAssignment(TuppleAssignment),
    FunctionCall(Value),
    FunctionExecution,
    Break,
    Continue,
    Require(Require),
    Conditionals(Conditionals),
    Return(Return),
    Delete(Delete),
    Revert(Option<Value>),
    Assert(Value),
    Loop(Loop),
    Scope(Box<FunctionArm>),
}

impl FunctionHeader {
    pub fn new() -> Self {
        Self {
            arguments: None,
            gasless: false,
            modifiers: None,
            mutability: None,
            name: None,
            inheritance: None,
            returns: None,
            visibility: None,
            r#type: FunctionType::Interface,
            is_array: false,
            location: None,
            size: None,
            var_name: None,
            var_visibility: None,
        }
    }
}
