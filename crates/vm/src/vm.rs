use std::ptr::NonNull;
use std::sync::Arc;

use bytes::Buf;
use inkstone_bytecode::inst::{Inst, InstContainer};

use crate::value::{Function, Val};

struct Frame {
    /// The caller of this frame. If the caller is `None`, this frame should signal the completion
    /// of the task it's in when returning.
    caller: Option<NonNull<Frame>>,
    /// The function this frame is tied to.
    func: Arc<Function>,
    stack: Vec<Val>,
    ip: usize,
}

impl Frame {
    pub fn new(func: Arc<Function>, caller: Option<NonNull<Frame>>) -> Self {
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

pub struct InkstoneVm {
    /// The current active frame.
    active_frame: *mut Frame,
}

impl InkstoneVm {}

/// Result after executing an instruction
#[derive(Debug)]
enum InstResult {
    /// Continue on executing the next instruction. Note that `frame.ip` might change when executing
    /// the current instruction.
    Continue,

    /// Return the control flow to its caller. The stack top value is used as the return value.
    Return,

    /// Tail call another function. `frame.ip` should point at the constant parameter indicating
    /// how many parameters should be popped.
    TailCall,

    /// Yield the current task. The stack top value is used as the return value, and the
    /// resume value should be pushed back to the stack top.
    Yield,

    /// Panic! Abort the current task, destroying all frames, and reply with an error if other tasks
    /// ask.
    Panic,
}

/// Handle a single instruction.
///
/// This function is inlined into the handler loop of the parent function.
///
/// ---
///
/// ## TODO: Add a contex parameter.
///
/// The context should at least be able to:
///
/// - Allocate/deallocate memory
/// - Add/wake a task inside event loop
/// - Call builtin functions
///
/// The context should not be able to:
///
/// - Modify the current frame (otherwise it will violate Rust's aliasing rules)
#[inline(always)]
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
        Inst::Add => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                frame.push(Val::Int(l.wrapping_add(r)));
                Continue
            } else if let (Some(l), Some(r)) = (lhs.as_float(), rhs.as_float()) {
                frame.push(Val::Float(l + r));
                Continue
            } else {
                add_slow(frame, lhs, rhs)
            }
        }
        Inst::Sub => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                frame.push(Val::Int(l.wrapping_sub(r)));
                Continue
            } else if let (Some(l), Some(r)) = (lhs.as_float(), rhs.as_float()) {
                frame.push(Val::Float(l - r));
                Continue
            } else {
                sub_slow(frame, lhs, rhs)
            }
        }
        Inst::Mul => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                frame.push(Val::Int(l.wrapping_mul(r)));
                Continue
            } else if let (Some(l), Some(r)) = (lhs.as_float(), rhs.as_float()) {
                frame.push(Val::Float(l * r));
                Continue
            } else {
                mul_slow(frame, lhs, rhs)
            }
        }
        Inst::Div => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                frame.push(Val::Int(l.wrapping_div(r)));
                Continue
            } else if let (Some(l), Some(r)) = (lhs.as_float(), rhs.as_float()) {
                frame.push(Val::Float(l / r));
                Continue
            } else {
                div_slow(frame, lhs, rhs)
            }
        }
        Inst::Rem => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                frame.push(Val::Int(l.wrapping_rem(r)));
                Continue
            } else if let (Some(l), Some(r)) = (lhs.as_float(), rhs.as_float()) {
                frame.push(Val::Float(l % r));
                Continue
            } else {
                todo!("rem slow")
            }
        }
        Inst::Pow => todo!(),
        Inst::BitAnd => todo!(),
        Inst::BitOr => todo!(),
        Inst::BitXor => todo!(),
        Inst::Shl => todo!(),
        Inst::Shr => todo!(),
        Inst::ShrL => todo!(),
        Inst::And => {
            let (lhs, rhs) = frame.pop2();
            frame.push(Val::Bool(lhs.to_bool() && rhs.to_bool()));
            Continue
        }
        Inst::Or => {
            let (lhs, rhs) = frame.pop2();
            frame.push(Val::Bool(lhs.to_bool() || rhs.to_bool()));
            Continue
        }
        Inst::Not => {
            let lhs = frame.pop();
            if lhs.is_bool() {
                frame.push(Val::Bool(!lhs.to_bool()));
                Continue
            } else if let Some(i) = lhs.as_int() {
                // ~i
                frame.push(Val::Int(!i));
                Continue
            } else {
                todo!("not slow")
            }
        }
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
        Inst::Yield => {
            //
            Yield
        }
        Inst::NewTask => {
            todo!("create a new task");
            Continue
        }
        Inst::PollTask => todo!(),
        Inst::DetachTask => todo!(),
        Inst::Panic => todo!(),
        Inst::Nop => todo!(),
        Inst::Invalid => todo!(),
    }
}

fn add_slow(frame: &mut Frame, lhs: Val, rhs: Val) -> InstResult {
    todo!()
}

fn sub_slow(frame: &mut Frame, lhs: Val, rhs: Val) -> InstResult {
    todo!()
}

fn mul_slow(frame: &mut Frame, lhs: Val, rhs: Val) -> InstResult {
    todo!()
}

fn div_slow(frame: &mut Frame, lhs: Val, rhs: Val) -> InstResult {
    todo!()
}
