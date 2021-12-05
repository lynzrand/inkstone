mod tuple;

use std::collections::{HashMap, VecDeque};

use inkstone_util::by_ptr::ByPtr;
use inkstone_util::string::ArcStr;

use crate::gc::{Gc, RawGcPtr, Trace};
use crate::vm::frame::{self, Frame};

/// A symbol is a pointer to a singleton string. It is never garbage collected.
type Symbol = ByPtr<ArcStr>;

#[derive(Clone)]
pub enum Val {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(ArcStr),
    Symbol(Symbol),
    Tuple(TupleRef),
    Array(Gc<Array>),
    Object(Gc<Object>),
    Closure(Gc<Closure>),
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

    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(v) = self {
            Some(*v)
        } else {
            None
        }
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

    pub fn as_string(&self) -> Option<&str> {
        if let Self::String(v) = self {
            Some(v.as_str())
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

    pub fn as_symbol(&self) -> Option<&ByPtr<ArcStr>> {
        if let Self::Symbol(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn try_into_string(self) -> Result<ArcStr, Self> {
        if let Self::String(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_tuple(self) -> Result<TupleRef, Self> {
        if let Self::Tuple(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_array(self) -> Result<Gc<Array>, Self> {
        if let Self::Array(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_object(self) -> Result<Gc<Object>, Self> {
        if let Self::Object(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_closure(self) -> Result<Gc<Closure>, Self> {
        if let Self::Closure(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

impl Trace for Val {
    fn trace(&self, tracer: vtable::VRefMut<crate::gc::GcTracerVTable>) {
        match self {
            // these variants don't hold a GC-ed pointer.
            Val::Nil
            | Val::Bool(_)
            | Val::Int(_)
            | Val::Float(_)
            | Val::Symbol(_)
            | Val::String(_) => {}

            // these variants hold a GC-ed pointer, so they need to call the
            // trace method on the pointer
            Val::Tuple(tuple) => tuple.trace(tracer),
            Val::Array(arr) => arr.trace(tracer),
            Val::Object(obj) => obj.trace(tracer),
            Val::Closure(closure) => closure.trace(tracer),
        }
    }
}

#[derive(Clone)]
pub struct TupleRef {
    ptr: RawGcPtr,
}

impl TupleRef {
    pub fn header(&self) -> &TupleHeader {
        unsafe { self.ptr.value().cast().as_ref() }
    }

    pub fn values(&self) -> &[Val] {
        let arity = self.header().arity;
        unsafe {
            let val_start = self
                .ptr
                .value()
                .as_ptr()
                .add(std::mem::size_of::<TupleHeader>());
            let val_start = val_start as *mut Val;
            std::slice::from_raw_parts(val_start, arity as usize)
        }
    }

    pub fn values_mut(&mut self) -> &mut [Val] {
        let arity = self.header().arity;
        unsafe {
            let val_start = self
                .ptr
                .value()
                .as_ptr()
                .add(std::mem::size_of::<TupleHeader>());
            let val_start = val_start as *mut Val;
            std::slice::from_raw_parts_mut(val_start, arity as usize)
        }
    }

    pub fn raw_ptr(&self) -> &RawGcPtr {
        &self.ptr
    }
}

impl Trace for TupleRef {
    fn trace(&self, tracer: vtable::VRefMut<crate::gc::GcTracerVTable>) {
        self.ptr.trace(tracer)
    }
}

pub struct TupleHeader {
    pub arity: u16,
    pub val: [Val; 0],
}

pub struct Array {
    pub val: VecDeque<Val>,
}

pub struct Object {
    pub proto: Gc<Object>,
    pub val: HashMap<Symbol, Val>,
}

pub struct Closure {
    pub func: Gc<Function>,
    /// Upvalue captures
    pub capture: Box<[Gc<UpValue>]>,
}

pub enum UpValue {
    Local(Gc<Frame>, u32),
    Detached(Val),
}

pub struct Function {
    pub name: Option<ArcStr>,
    pub inst: Vec<u8>,
    pub param_cnt: u32,
    pub locals_cnt: u32,
    pub binds_self: bool,
    pub has_rest_param: bool,
    pub constants: Vec<Val>,
    pub labels: Vec<u32>,
}

impl Trace for Array {
    fn trace(&self, mut tracer: vtable::VRefMut<crate::gc::GcTracerVTable>) {
        for val in &self.val {
            val.trace(tracer.borrow_mut());
        }
    }
}

impl Trace for Object {
    fn trace(&self, mut tracer: vtable::VRefMut<crate::gc::GcTracerVTable>) {
        self.val.iter().for_each(|(_name, val)| {
            val.trace(tracer.borrow_mut());
        });
        self.proto.trace(tracer)
    }
}

impl Trace for Closure {
    fn trace(&self, mut tracer: vtable::VRefMut<crate::gc::GcTracerVTable>) {
        self.capture
            .iter()
            .for_each(|v| v.trace(tracer.borrow_mut()));
        self.func.trace(tracer);
    }
}

impl Trace for UpValue {
    fn trace(&self, tracer: vtable::VRefMut<crate::gc::GcTracerVTable>) {
        match self {
            UpValue::Local(frame, _) => frame.trace(tracer),
            UpValue::Detached(val) => val.trace(tracer),
        }
    }
}

impl Trace for Function {
    fn trace(&self, mut tracer: vtable::VRefMut<crate::gc::GcTracerVTable>) {
        for it in &self.constants {
            it.trace(tracer.borrow_mut())
        }
    }
}

mod tracer_impl {
    #![allow(non_upper_case_globals)]

    use super::*;
    use crate::*;
    use gc::{GcTracerVTable, Trace};

    TraceVTable_static!(static Val_TraceVTable for Val);
    TraceVTable_static!(static Array_TraceVTable for Array);
    TraceVTable_static!(static Object_TraceVTable for Object);
    TraceVTable_static!(static Closure_TraceVTable for Closure);
    TraceVTable_static!(static UpValue_TraceVTable for UpValue);
    TraceVTable_static!(static Function_TraceVTable for Function);
}
