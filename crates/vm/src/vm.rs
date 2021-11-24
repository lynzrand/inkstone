use std::sync::Arc;

use bytes::Buf;
use inkstone_bytecode::inst::{Inst, InstContainer};

use crate::value::{Function, Val};

struct Frame {
    func: Arc<Function>,
    stack: Vec<Val>,
    ip: usize,
}

impl Frame {
    pub fn new(func: Arc<Function>) -> Self {
        Self {
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
        self.func.as_ref()
    }

    /// Get a reference to the frame's func.
    pub fn func_arc(&self) -> &Arc<Function> {
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

pub struct InkstoneVm {}

impl InkstoneVm {}

/// Result after executing an instruction
#[derive(Debug)]
enum InstResult {
    Continue,
    Return,
    Yield,
    Wake,
    Panic,
}

fn exec_inst(inst: Inst, frame: &mut Frame) -> InstResult {
    use InstResult::*;
    match inst {
        Inst::Pop => {
            let _ = frame.pop();
            Continue
        }
        Inst::Dup => {
            let s = frame.stack_mut();
            let another = s.last().expect("Popping empty stack").dup();
            s.push(another);
            Continue
        }
        Inst::PushI32 => {
            let i_param = frame.read_param::<i32>();
            frame.push(Val::Int(i_param as i64));
            Continue
        }
        Inst::PushConst => {
            let idx = frame.read_param::<u32>() as usize;
            let c = frame.func().constants[idx].dup();
            frame.push(c);
            Continue
        }
        Inst::PushTrue => {
            frame.push(Val::Bool(true));
            Continue
        }
        Inst::PushFalse => {
            frame.push(Val::Bool(false));
            Continue
        }
        Inst::PushNil => {
            frame.push(Val::Nil);
            Continue
        }
        Inst::Add => todo!(),
        Inst::Sub => todo!(),
        Inst::Mul => todo!(),
        Inst::Div => todo!(),
        Inst::Rem => todo!(),
        Inst::Pow => todo!(),
        Inst::BitAnd => todo!(),
        Inst::BitOr => todo!(),
        Inst::BitXor => todo!(),
        Inst::Shl => todo!(),
        Inst::Shr => todo!(),
        Inst::ShrL => todo!(),
        Inst::And => todo!(),
        Inst::Or => todo!(),
        Inst::Not => todo!(),
        Inst::Lt => todo!(),
        Inst::Gt => todo!(),
        Inst::Le => todo!(),
        Inst::Ge => todo!(),
        Inst::Eq => todo!(),
        Inst::Ne => todo!(),
        Inst::TupleNew => todo!(),
        Inst::ArrayNew => todo!(),
        Inst::ClosureNew => todo!(),
        Inst::MapNew => todo!(),
        Inst::SetPrototype => todo!(),
        Inst::GetPrototype => todo!(),
        Inst::LoadField => todo!(),
        Inst::StoreField => todo!(),
        Inst::LoadFieldDyn => todo!(),
        Inst::StoreFieldDyn => todo!(),
        Inst::LoadFieldChain => todo!(),
        Inst::LoadLocal => todo!(),
        Inst::StoreLocal => todo!(),
        Inst::UpValueScopeNew => todo!(),
        Inst::WithUpvalue => todo!(),
        Inst::WithUpvalueCopy => todo!(),
        Inst::LoadUpvalue => todo!(),
        Inst::StoreUpvalue => todo!(),
        Inst::UpValueDetach => todo!(),
        Inst::PushGlobalObject => todo!(),
        Inst::PushModuleObject => todo!(),
        Inst::Br => todo!(),
        Inst::BrIfTrue => todo!(),
        Inst::BrIfFalse => todo!(),
        Inst::Call => todo!(),
        Inst::CallMethod => todo!(),
        Inst::TailCall => todo!(),
        Inst::TailCallMethod => todo!(),
        Inst::Return => todo!(),
        Inst::Yield => todo!(),
        Inst::NewTask => todo!(),
        Inst::PollTask => todo!(),
        Inst::DetachTask => todo!(),
        Inst::Panic => todo!(),
        Inst::Nop => todo!(),
        Inst::Invalid => todo!(),
    }
}
