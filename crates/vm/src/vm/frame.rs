use std::borrow::Borrow;

use bytes::Buf;
use inkstone_bytecode::inst::InstContainer;

use crate::gc::{Gc, GcTracerVTable, Trace};
use crate::value::{Function, Val};
use crate::*;

pub(crate) struct Frame {
    /// The caller of this frame. If the caller is `None`, this frame should signal the completion
    /// of the task it's in when returning.
    pub caller: Option<Gc<Frame>>,
    /// The function this frame is tied to.
    pub func: Gc<Function>,
    pub stack: Vec<Val>,
    pub ip: usize,
}

impl Frame {
    pub fn new(func: Gc<Function>, caller: Option<Gc<Frame>>) -> Self {
        Self {
            caller,
            func,
            stack: vec![],
            ip: 0,
        }
    }

    pub fn pop(&mut self) -> Val {
        self.stack.pop().expect("Popping empty stack")
    }

    pub fn push(&mut self, val: Val) {
        self.stack.push(val);
    }

    pub fn pop2(&mut self) -> (Val, Val) {
        let rhs = self.pop();
        let lhs = self.pop();
        (lhs, rhs)
    }

    /// Get a mutable reference to the frame's stack.
    pub fn stack_mut(&mut self) -> &mut Vec<Val> {
        &mut self.stack
    }

    /// Get a reference to the frame's func.
    pub fn func(&self) -> &Function {
        self.func.borrow()
    }

    /// Get a reference to the frame's func.
    pub fn func_pte(&self) -> &Gc<Function> {
        &self.func
    }
}

impl Buf for Frame {
    fn remaining(&self) -> usize {
        self.func().inst.len() - self.ip
    }

    fn chunk(&self) -> &[u8] {
        &self.func().inst[self.ip..]
    }

    fn advance(&mut self, cnt: usize) {
        self.ip += cnt
    }
}

impl InstContainer for Frame {
    fn seek(&mut self, position: usize) {
        self.ip = position
    }
}

impl Trace for Frame {
    fn trace(&self, tracer: vtable::VRefMut<crate::gc::GcTracerVTable>) {
        if let Some(caller) = &self.caller {
            caller.trace(tracer)
        }
    }
}

TraceVTable_static! {
#[allow(non_upper_case_globals)]
static Frame_TraceVTable for Frame
}
