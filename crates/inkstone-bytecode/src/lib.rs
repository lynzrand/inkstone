pub mod inst;

#[derive(Debug)]
pub struct Function {
    pub name: Option<String>,
    pub inst: Vec<u8>,
    pub param_cnt: u32,
    pub binds_self: bool,
    pub has_rest_param: bool,
    pub constants: Vec<()>,
    pub metadata: Option<FunctionMetadata>,
}

#[derive(Debug)]
pub struct ConstantTable {}

#[derive(Debug)]
pub struct FunctionMetadata {}

pub mod reexports {
    pub use num_enum::{FromPrimitive, TryFromPrimitive};
}
