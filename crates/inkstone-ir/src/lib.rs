pub mod inst;

use inst::Inst;
use slab::Slab;
use std::fmt::Display;

#[derive(Debug)]
pub struct Function {
    pub name: Option<String>,
    pub inst: Vec<u8>,
    pub param_cnt: u32,
    pub binds_self: bool,
    pub has_rest_param: bool,
    pub constants: Vec<()>,
}

pub struct ConstantTable {}

pub struct FunctionMetadata {}
