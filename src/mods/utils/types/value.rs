use crate::mods::lexer::tokens::Token;

// #[derive(Debug)]
// pub enum TypeCast {
//     Value(Box<Value>),
//     Cast(Box<TypeCast>),
// }
#[derive(Debug)]

pub enum StringVariable {
    Literal(String),
    TypeCast(Box<Value>),
}

#[derive(Debug)]

pub struct StringValue {
    pub value: StringVariable,
    pub then: Option<Box<Value>>,
}
#[derive(Debug)]

pub enum IntegerVariable {
    Literal(String),
    TypeCast {
        size: Option<u16>,
        value: Box<Value>,
    },
}
#[derive(Debug)]

pub struct IntegerValue {
    pub value: IntegerVariable,
    pub then: Option<Box<Value>>,
}
#[derive(Debug)]

pub enum BytesVariable {
    Literal(String),
    TypeCast {
        size: Option<u16>,
        value: Box<Value>,
    },
}

#[derive(Debug)]
pub struct BytesValue {
    pub value: BytesVariable,
    pub then: Option<Box<Value>>,
}

#[derive(Debug)]

pub enum AddressVariable {
    // Literal(String),
    TypeCast(Box<Value>),
}

#[derive(Debug)]

pub struct AddressValue {
    pub value: AddressVariable,
    pub then: Option<Box<Value>>,
}

#[derive(Debug)]

pub enum BooleanVariable {
    Literal(String),
    TypeCast(Box<Value>),
}
#[derive(Debug)]

pub struct BooleanValue {
    pub value: BooleanVariable,
    pub then: Option<Box<Value>>,
}
#[derive(Debug)]

pub enum ExpressionTypes {
    PostIncrement,
    PostDecrement,
    PreIncrement,
    PreDecrement,
    Eq,
    Mod,
    Shl,
    Shr,
    TernaryIf,
    TernaryEl,
    Add,
    Sub,
    Mul,
    Div,
    Gt,
    Lt,
    GtEq,
    NotEq,
    LtEq,
    PlusEq,
    MinusEq,
    DivEq,
    MulEq,
    LgOr,
    BtOr,
    LgAnd,
    BtAnd,
    Xor,
    BtNot,
    LgNot,
}
#[derive(Debug)]

pub struct ExpressionVariable {
    pub r#type: ExpressionTypes,
    pub operand: Box<Value>,
}

#[derive(Debug)]
pub struct ExpressionValue {
    pub value: ExpressionVariable,
    pub then: Option<Box<Value>>,
}

#[derive(Debug)]

pub enum FunctionValueType {
    // Global,
    Defined,
}
// #[derive(Debug)]

// pub struct ContractInstanceValue {
//     pub identifier: String,
//     pub arguments: Vec<Value>,
// }
// #[derive(Debug)]

// pub struct ContractFunctionValue {
//     pub identifier: String,
//     pub arguments: Vec<Value>,
//     pub function_identifier: String,
//     pub function_args: Vec<Value>,
// }
// #[derive(Debug)]

// pub enum Contractvalue {
//     ContractFunctionValue(ContractFunctionValue),
//     ContractInstanceValue(ContractInstanceValue),
// }

#[derive(Debug)]
pub enum ArgumentType {
    Positional(Value),
    Named { key: String, value: Value },
}
#[derive(Debug)]

pub struct FunctionVariable {
    pub identifier: String,
    pub arguments: Option<Vec<ArgumentType>>,
    pub r#type: FunctionValueType,
}
#[derive(Debug)]

pub struct FunctionValue {
    pub value: FunctionVariable,
    pub then: Option<Box<Value>>,
}
#[derive(Debug)]
pub struct IdentifierValue {
    pub value: String,
    pub then: Option<Box<Value>>,
}

#[derive(Debug)]
pub struct KeywordValue {
    pub value: Token,
    pub then: Option<Box<Value>>,
}
#[derive(Debug)]

pub struct FunctionPTRInvocation {
    pub args: Option<Vec<ArgumentType>>,
    pub then: Option<Box<Value>>,
}

// #[derive(Debug)]

// pub struct StructInstanceValue {
//     pub identifier: String,
//     pub variants: Vec<[String; 2]>,
// }

// #[derive(Debug)]

// pub enum StructValue {
//     Instance(StructInstanceValue),
//     Variant(VariantValue),
// }

#[derive(Debug)]
pub struct InstanceVariable {
    pub r#type: String,
    pub arguments: Option<Vec<ArgumentType>>,
    pub size: Option<Box<Value>>,
}

#[derive(Debug)]

pub struct InstanceValue {
    pub value: InstanceVariable,
    pub then: Option<Box<Value>>,
}

// #[derive(Debug)]

// pub enum NestType {
//     Variant,
//     Method,
// }

// #[derive(Debug)]

// pub struct NestedValue {
//     pub r#type: NestType,
//     pub value: Box<Value>,
// }

#[derive(Debug)]
pub struct ArrayValue {
    pub variants: Vec<Value>,
    pub then: Option<Box<Value>>,
}

#[derive(Debug)]
pub struct VariantValue {
    pub variant: Box<Value>,
    pub then: Option<Box<Value>>,
}

#[derive(Debug)]
pub struct PayableValue {
    pub value: Box<Value>,
    pub then: Option<Box<Value>>,
}

#[derive(Debug)]
pub enum Value {
    StringValue(StringValue),
    ArrayValue(ArrayValue),
    IntegerValue(IntegerValue),
    BytesValue(BytesValue),
    AddressValue(AddressValue),
    BooleanValue(BooleanValue),
    FunctionValue(FunctionValue),
    VariantValue(VariantValue),
    FunctionPTRInvocation(FunctionPTRInvocation),
    ExpressionValue(ExpressionValue),
    KeywordValue(KeywordValue),
    PayableValue(PayableValue),
    UnitValue(String, Token),

    IdentifierValue(IdentifierValue),
    Context {
        value: Box<Value>,
        then: Option<Box<Value>>,
    },
    InstanceValue(InstanceValue),
    None,
}

// impl Value {
//     fn add_method(&mut self, new: Value) {
//         match self {
//             Self::None => {
//                 *self = new;
//             }
//             _ => {
//                 if self.access_then().is_none() {
//                     *self.access_then() = Some(Box::new(new));
//                 } else {
//                     Value::od(&mut self.access_then().as_mut().unwrap(), new);
//                 }
//             }
//         }
//     }

//     fn od(oth: &mut Box<Value>, new: Value) {
//         match **oth {
//             Value::None => *oth = Box::new(new),
//             _ => {
//                 if oth.access_then().is_none() {
//                     *oth.access_then().as_mut().unwrap() = Box::new(new);
//                 } else {
//                     Value::od(&mut oth.access_then().as_mut().unwrap(), new)
//                 }
//             }
//         }
//     }
//     fn access_then(&mut self) -> &mut Option<Box<Value>> {
//         match self {
//             Value::FunctionValue(value) => &mut value.then,
//             _ => {
//                 panic!("Detected non method type")
//             }
//         }
//     }
// }
