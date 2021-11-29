use std::alloc::Layout;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::ptr::NonNull;
use std::sync::Arc;

use bytes::Buf;
use inkstone_bytecode::inst::{Inst, InstContainer};

use crate::gc::alloc::GcAllocator;
use crate::gc::{Gc, RawGcPtr};
use crate::task::Task;
use crate::value::{Closure, Function, TupleHeader, Val};

pub(crate) struct Frame {
    /// The caller of this frame. If the caller is `None`, this frame should signal the completion
    /// of the task it's in when returning.
    caller: Option<NonNull<Frame>>,
    /// The function this frame is tied to.
    func: Gc<Function>,
    stack: Vec<Val>,
    ip: usize,
}

impl Frame {
    pub fn new(func: Gc<Function>, caller: Option<NonNull<Frame>>) -> Self {
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

pub struct InkstoneVm {
    /// The current active frame.
    active_task: *mut Task,
    /// The garbage collector and allocator
    allocator: GcAllocator,
    task_list: TaskList,
}

struct TaskList {
    sleeping_tasks: HashSet<NonNull<Task>>,
    event_queue_start: *mut Task,
    event_queue_end: *mut Task,
}

impl TaskList {
    fn is_queue_empty(&self) -> bool {
        debug_assert_eq!(
            self.event_queue_start.is_null(),
            self.event_queue_end.is_null()
        );
        self.event_queue_start.is_null()
    }

    fn pop_task(&mut self) -> Option<NonNull<Task>> {
        let task = NonNull::new(self.event_queue_start)?;
        let next_task = unsafe { task.as_ref() }.next;
        self.event_queue_start = next_task;
        if next_task.is_null() {
            self.event_queue_end = std::ptr::null_mut();
        }
        Some(task)
    }

    fn push_task(&mut self, mut task: NonNull<Task>) {
        if self.event_queue_start.is_null() {
            debug_assert!(
                self.event_queue_end.is_null(),
                "The event queue should be either empty or occupied"
            );
            self.event_queue_start = task.as_ptr();
            self.event_queue_end = task.as_ptr();
            unsafe {
                task.as_mut().next = std::ptr::null_mut();
            }
        } else {
            let last_end = self.event_queue_end;
            self.event_queue_end = task.as_ptr();
            unsafe {
                debug_assert!(
                    (*last_end).next.is_null(),
                    "The chain end should not have a next value"
                );
                (*last_end).next = task.as_ptr();
                task.as_mut().next = std::ptr::null_mut();
            }
        }
    }

    fn add_sleeping(&mut self, task: NonNull<Task>) {
        self.sleeping_tasks.insert(task);
    }

    fn wake_sleeping(&mut self, task: NonNull<Task>) {
        self.sleeping_tasks.remove(&task);
        self.push_task(task);
    }
}

impl InkstoneVm {
    pub fn event_loop(&mut self) {
        if self.active_task.is_null() && self.task_list.is_queue_empty() {
            // There's nothing we can do if we don't have an active task and no tasks in the loop
            return;
        }

        if self.active_task.is_null() {
            let new_task = self
                .task_list
                .pop_task()
                .expect("Task list should not be empty, as stated before");
            self.active_task = new_task.as_ptr();
        }

        loop {
            self.interpret_loop();
            if let Some(task) = self.task_list.pop_task() {
                self.active_task = task.as_ptr();
            } else {
                break;
            }
        }
    }

    /// The main interpreting loop.
    ///
    /// Unlike the similar structure in QuickJS and other interpreters, this loop is **not**
    /// recursive. Non-builtin function calls break out of this loop and executes as a separate
    /// action, instead of being called as a subroutine.
    fn interpret_loop(&mut self) {
        let action = loop {
            // We have a unique mutable reference on the frame. This reference will last until the
            // end of loop.
            let frame = unsafe { (*self.active_task).stack_top.as_mut() };
            let inst = frame.read_inst();
            let cx = InstExecCtx {
                alloc: &mut self.allocator,
            };
            let action = exec_inst(inst, frame, cx);
            if action != InstAction::Continue {
                break action;
            }
        };
        match action {
            InstAction::Continue => unreachable!("Continue action will not break the loop"),
            InstAction::Return => todo!(),
            InstAction::Call => todo!(),
            InstAction::TailCall => todo!(),
            InstAction::CallMethod => todo!(),
            InstAction::TailCallMethod => todo!(),
            InstAction::Yield => todo!(),
            InstAction::Panic => todo!(),
        }
    }
}

/// Result after executing an instruction
#[derive(Debug, PartialEq, Eq)]
enum InstAction {
    /// Continue on executing the next instruction. Note that `frame.ip` might change when executing
    /// the current instruction.
    Continue,

    /// Return the control flow to its caller. The stack top value is used as the return value.
    Return,

    /// Call another function. `frame.ip` should point at the constant parameter indicating
    /// how many parameters should be popped.
    Call,

    /// Tail call another function. `frame.ip` should point at the constant parameter indicating
    /// how many parameters should be popped.
    TailCall,

    /// Call another function with a receiver. `frame.ip` should point at the constant parameter
    /// indicating how many parameters should be popped.
    CallMethod,

    /// Tail call another function with a receiver. `frame.ip` should point at the constant
    /// parameter indicating how many parameters should be popped.
    TailCallMethod,

    /// Yield the current task. The stack top value is used as the return value, and the
    /// resume value should be pushed back to the stack top.
    Yield,

    /// Panic! Abort the current task, destroying all frames, and reply with an error if other tasks
    /// ask.
    Panic,
}

struct InstExecCtx<'r> {
    alloc: &'r mut GcAllocator,
}

impl<'r> InstExecCtx<'r> {
    fn create_task(&mut self, closure: Gc<Closure>, params: &[Gc<Val>]) -> Gc<Task> {
        todo!("create task")
    }

    fn wake_task(&mut self, task: Gc<Task>) {
        todo!("wake task")
    }

    fn detach_task(&mut self, task: Gc<Task>) {
        todo!("detach task")
    }

    fn call_builtin(&mut self, f: &str) {
        todo!("call builtin function")
    }

    /// Get the allocator
    fn alloc_mut(&mut self) -> &mut GcAllocator {
        self.alloc
    }
}

impl GcAllocator {
    pub fn alloc_tuple(&mut self, arity: usize) -> Option<RawGcPtr> {
        let vtable = todo!();
        let (layout, _) = Layout::new::<TupleHeader>()
            .pad_to_align()
            .extend(Layout::array::<Val>(arity).expect("Failed to compute tuple layout"))
            .expect("Failed to compute tuple layout");
        unsafe { self.alloc_raw(layout, vtable) }
    }
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
fn exec_inst(inst: Inst, frame: &mut Frame, cx: InstExecCtx<'_>) -> InstAction {
    use InstAction::*;
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
        Inst::Pow => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                if !(0..u32::MAX as i64).contains(&r) {
                    Panic
                } else {
                    frame.push(Val::Int(l.pow(r as u32)));
                    Continue
                }
            } else if let (Some(l), Some(r)) = (lhs.as_float(), rhs.as_float()) {
                frame.push(Val::Float(l % r));
                Continue
            } else {
                todo!("rem slow")
            }
        }
        Inst::BitAnd => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_bool(), rhs.as_bool()) {
                frame.push(Val::Bool(l & r));
                Continue
            } else if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                frame.push(Val::Int(l & r));
                Continue
            } else {
                Panic
            }
        }
        Inst::BitOr => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_bool(), rhs.as_bool()) {
                frame.push(Val::Bool(l | r));
                Continue
            } else if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                frame.push(Val::Int(l & r));
                Continue
            } else {
                Panic
            }
        }
        Inst::BitXor => {
            let (lhs, rhs) = frame.pop2();
            if let (Some(l), Some(r)) = (lhs.as_bool(), rhs.as_bool()) {
                frame.push(Val::Bool(l ^ r));
                Continue
            } else if let (Some(l), Some(r)) = (lhs.as_int(), rhs.as_int()) {
                frame.push(Val::Int(l ^ r));
                Continue
            } else {
                Panic
            }
        }
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
            frame.push(Val::Bool(lhs.to_bool()));
            Continue
        }
        Inst::Lt => {
            // TODO: implement
            Continue
        }
        Inst::Gt => {
            // TODO: implement
            Continue
        }
        Inst::Le => {
            // TODO: implement
            Continue
        }
        Inst::Ge => {
            // TODO: implement
            Continue
        }
        Inst::Eq => {
            // TODO: implement
            Continue
        }
        Inst::Ne => {
            // TODO: implement
            Continue
        }
        Inst::TupleNew => {
            let cnt = frame.read_param::<u32>();
            let mut stack = frame.stack_mut();
            let len = stack.len();
            todo!();
            Continue
        }
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
        Inst::Call => Call,
        Inst::CallMethod => CallMethod,
        Inst::TailCall => TailCall,
        Inst::TailCallMethod => TailCallMethod,
        Inst::Return => Return,
        Inst::Yield => Yield,
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

fn add_slow(frame: &mut Frame, lhs: Val, rhs: Val) -> InstAction {
    todo!()
}

fn sub_slow(frame: &mut Frame, lhs: Val, rhs: Val) -> InstAction {
    todo!()
}

fn mul_slow(frame: &mut Frame, lhs: Val, rhs: Val) -> InstAction {
    todo!()
}

fn div_slow(frame: &mut Frame, lhs: Val, rhs: Val) -> InstAction {
    todo!()
}
