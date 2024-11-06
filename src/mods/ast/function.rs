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
pub enum VariableAssignOperation {
    Push,
    Pop,
    Assign,
}

#[derive(Debug)]
pub struct VariantAssign {
    pub identifier: String,
    pub value: Option<Value>,
    pub variants: Vec<Value>,
    pub operation: VariableAssignOperation,
}

#[derive(Debug)]
pub struct Require {
    pub condition: Option<Value>,
    pub message: Option<Value>,
}

#[derive(Debug)]
pub struct TuppleAssignment {
    pub identifiers: Vec<VariableAST>,
    pub value: Box<FunctionArm>,
}

#[derive(Debug)]
pub enum ConditionType {
    If,
    ElIf,
}

#[derive(Debug)]
pub struct If {
    pub r#type: ConditionType,
    pub condition: Option<Value>,
    pub arm: Option<Vec<FunctionArm>>,
}

#[derive(Debug)]
pub struct Conditionals {
    pub condition: Option<Value>,
    pub arm: Option<Vec<FunctionArm>>,
    pub elif: Option<Vec<If>>,
    pub el: Option<Vec<FunctionArm>>,
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
    VariableAssign(VariableAST),
    VariantAssign(VariantAssign),
    TuppleAssignment(TuppleAssignment),
    FunctionCall(Value),
    FunctionExecution,
    Break,
    Continue,
    Loop(Loop),
    Context(Vec<FunctionArm>),
    MemoryAssign(Value),
    VariableIdentifier(VariableAST),
    EventEmitter(Value),
    If(If),
    El(Option<Vec<FunctionArm>>),
    Require(Require),
    // Conditionals(Conditionals),
    Return(Value),
    Delete(Value),
    Revert(Option<Value>),
    Assert(Value),
    Scope(Vec<FunctionArm>),
    None,
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

impl Conditionals {
    pub fn new() -> Self {
        Self {
            condition: None,
            arm: None,
            elif: None,
            el: None,
        }
    }
}

impl Require {
    pub fn new() -> Self {
        Self {
            condition: None,
            message: None,
        }
    }
}
