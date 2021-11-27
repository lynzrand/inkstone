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

    /// Returns `true` if the val is [`Nil`].
    ///
    /// [`Nil`]: Val::Nil
    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
    }

    /// Returns `true` if the val is [`Bool`].
    ///
    /// [`Bool`]: Val::Bool
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(..))
    }

    /// Convert this value to boolean.
    ///
    /// The only falsy values are:
    ///
    /// - `false` (boolean false)
    /// - `nil` (nil pointer)
    /// - `0` (Integer zero)
    /// - `0.0` (Float zero)
    pub fn to_bool(&self) -> bool {
        !(
            // value is false, nil or 0
            matches!(self, Val::Bool(false) | Val::Nil | Val::Int(0)) 
            // value is float 0.0
            || self.as_float().map_or(false, |v| v == 0.0)
        )
    }

    /// Returns `true` if the val is [`Int`].
    ///
    /// [`Int`]: Val::Int
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(..))
    }

    pub fn as_int(&self) -> Option<i64> {
        if let Self::Int(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `true` if the val is [`Float`].
    ///
    /// [`Float`]: Val::Float
    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float(..))
    }

    pub fn as_float(&self) -> Option<f64> {
        if let Self::Int(i) = self {
            Some(*i as f64)
        } else if let Self::Float(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `true` if the val is [`String`].
    ///
    /// [`String`]: Val::String
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(..))
    }

    pub fn as_string(&self) -> Option<&Gc<String>> {
        if let Self::String(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the val is [`Tuple`].
    ///
    /// [`Tuple`]: Val::Tuple
    pub fn is_tuple(&self) -> bool {
        matches!(self, Self::Tuple(..))
    }

    pub fn as_tuple(&self) -> Option<&TupleRef> {
        if let Self::Tuple(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the val is [`Array`].
    ///
    /// [`Array`]: Val::Array
    pub fn is_array(&self) -> bool {
        matches!(self, Self::Array(..))
    }

    pub fn as_array(&self) -> Option<&Gc<Array>> {
        if let Self::Array(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the val is [`Object`].
    ///
    /// [`Object`]: Val::Object
    pub fn is_object(&self) -> bool {
        matches!(self, Self::Object(..))
    }

    pub fn as_object(&self) -> Option<&Gc<Object>> {
        if let Self::Object(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the val is [`Scope`].
    ///
    /// [`Scope`]: Val::Scope
    pub fn is_scope(&self) -> bool {
        matches!(self, Self::Scope(..))
    }

    pub fn as_scope(&self) -> Option<&Gc<Scope>> {
        if let Self::Scope(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the val is [`Closure`].
    ///
    /// [`Closure`]: Val::Closure
    pub fn is_closure(&self) -> bool {
        matches!(self, Self::Closure(..))
    }

    pub fn as_closure(&self) -> Option<&Gc<Closure>> {
        if let Self::Closure(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the val is [`Symbol`].
    ///
    /// [`Symbol`]: Val::Symbol
    pub fn is_symbol(&self) -> bool {
        matches!(self, Self::Symbol(..))
    }

    pub fn as_symbol(&self) -> Option<u64> {
        if let Self::Symbol(v) = self {
            Some(*v)
        } else {
            None
        }
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
