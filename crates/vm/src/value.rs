use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use smol_str::SmolStr;

use crate::gc::Gc;

pub enum Val {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Gc<String>),
    Tuple(TupleRef),
    Array(Gc<Array>),
    Object(Gc<Object>),
    Scope(Gc<Scope>),
    Closure(Gc<Closure>),
    Symbol(u64),
}

impl Val {
    /// Duplicate the value using runtime
    pub fn dup(&self) -> Val {
        todo!()
    }
}

pub struct TupleRef {
    pub arity: u16,
    pub val: *mut Val,
}

pub struct Array {
    pub val: VecDeque<Val>,
}

pub struct Object {
    pub proto: Gc<Object>,
    pub val: HashMap<u64, Val>,
}

pub struct Closure {
    pub func: Gc<Function>,
    pub scope: Gc<Scope>,
}

pub struct Scope {
    pub parent: Gc<Scope>,
    pub val: Vec<Val>,
}

pub struct Function {
    pub name: Option<SmolStr>,
    pub inst: Vec<u8>,
    pub param_cnt: u32,
    pub binds_self: bool,
    pub has_rest_param: bool,
    pub constants: Vec<Val>,
    pub labels: Vec<u32>,
}
