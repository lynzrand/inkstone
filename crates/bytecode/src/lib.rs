use std::sync::Arc;

use inst::InstContainerFormatter;
use smol_str::SmolStr;
use util::ArcByPtr;

pub mod inst;
pub mod util;

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

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.name)
            .field("inst", &InstContainerFormatter(&self.inst))
            .field("param_cnt", &self.param_cnt)
            .field("binds_self", &self.binds_self)
            .field("has_rest_param", &self.has_rest_param)
            .field("constants", &self.constants)
            .field("labels", &self.labels)
            .field("metadata", &self.metadata)
            .finish()
    }
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
