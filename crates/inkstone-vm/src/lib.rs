use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use smol_str::SmolStr;

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

pub struct TupleRef {
    arity: u16,
    val: *mut Val,
}

pub struct Array {
    val: VecDeque<Val>,
}

pub struct Object {
    proto: Gc<Object>,
    val: HashMap<u64, Val>,
}

pub struct Closure {
    func: Gc<Function>,
    scope: Gc<Scope>,
}

pub struct Scope {
    parent: Gc<Scope>,
    val: Vec<Val>,
}

pub struct Gc<T>(*mut GcValue<T>);

pub struct GcValue<T> {
    val: T,
}

pub struct Function {}
