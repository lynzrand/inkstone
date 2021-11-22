use std::sync::Arc;

use smol_str::SmolStr;
use util::ArcByPtr;

pub mod inst;
pub mod util;

#[derive(Debug)]
pub struct Function {
    pub name: Option<SmolStr>,
    pub inst: Vec<u8>,
    pub param_cnt: u32,
    pub binds_self: bool,
    pub has_rest_param: bool,
    pub constants: Vec<Constant>,
    pub labels: Vec<u32>,
    pub metadata: Option<FunctionMetadata>,
}

#[derive(Debug)]
pub struct FunctionMetadata {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    Int64(i64),
    Float64(u64),
    String(SmolStr),
    Symbol(SmolStr),
    FunctionBody(ArcByPtr<Function>),
}
