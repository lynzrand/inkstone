pub(crate) mod frame;
pub(crate) mod task;

use std::alloc::Layout;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::hint::unreachable_unchecked;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::Arc;

use bytes::Buf;
use inkstone_bytecode::inst::{Inst, InstContainer};
use inkstone_util::by_ptr::{ByPtr, ByPtrRef};
use inkstone_util::string::ArcStr;

use crate::gc::alloc::GcAllocator;
use crate::gc::{Gc, RawGcPtr};
use crate::value::{Closure, Function, TupleHeader, Val};

use frame::Frame;
use task::Task;

pub struct InkstoneVm {
    /// The current active frame.
    active_task: Option<Gc<Task>>,
    /// The garbage collector and allocator
    allocator: GcAllocator,
    task_list: TaskList,
}

struct TaskList {
    sleeping_tasks: HashSet<ByPtr<Gc<Task>>>,
    event_queue_start: Option<Gc<Task>>,
    event_queue_end: Option<Gc<Task>>,
}

impl TaskList {
    fn is_queue_empty(&self) -> bool {
        debug_assert_eq!(
            self.event_queue_start.is_none(),
            self.event_queue_end.is_none()
        );
        self.event_queue_start.is_none()
    }

    fn pop_task(&mut self) -> Option<Gc<Task>> {
        let task = self.event_queue_start.take()?;
        let next_task = unsafe { task.get_mut() }.next.take();
        if next_task.is_none() {
            self.event_queue_end = None;
        }
        self.event_queue_start = next_task;
        Some(task)
    }

    fn push_task(&mut self, task: Gc<Task>) {
        if self.event_queue_start.is_none() {
            debug_assert!(
                self.event_queue_end.is_none(),
                "The event queue should be either empty or occupied"
            );
            unsafe {
                task.get_mut().next = None;
            }
            self.event_queue_start = Some(task.clone());
            self.event_queue_end = Some(task);
        } else {
            let last_end = self.event_queue_end.clone().unwrap();
            unsafe {
                debug_assert!(
                    (*last_end).next.is_none(),
                    "The chain end should not have a next value"
                );
                task.get_mut().next = None;
                last_end.get_mut().next = Some(task.clone());
            }
            self.event_queue_end = Some(task);
        }
    }

    fn add_sleeping(&mut self, task: Gc<Task>) {
        self.sleeping_tasks.insert(task.into());
    }

    fn wake_sleeping(&mut self, task: Gc<Task>) {
        let task_by_ptr = ByPtr::new(task);
        self.sleeping_tasks.remove(&task_by_ptr);
        self.push_task(task_by_ptr.unwrap());
    }
}

impl InkstoneVm {
    pub fn invoke(&mut self, f: Gc<Closure>, params: &[Val]) -> Gc<Task> {
        let mut cx = InstExecCtx {
            alloc: &mut self.allocator,
            task_list: &mut self.task_list,
        };
        cx.create_task(Some("<main>".into()), f, params)
    }

    /// The loop executing different tasks.
    ///
    /// This loop is responsible for running every task
    pub fn event_loop(&mut self) {
        if self.active_task.is_none() && self.task_list.is_queue_empty() {
            // There's nothing we can do if we don't have an active task and no tasks in the loop
            return;
        }

        if self.active_task.is_none() {
            let new_task = self
                .task_list
                .pop_task()
                .expect("Task list should not be empty, as stated before");
            self.active_task = Some(new_task);
        }

        loop {
            self.task_loop();
            if let Some(task) = self.task_list.pop_task() {
                self.active_task = Some(task);
            } else {
                break;
            }
        }
    }

    /// The loop executing a single task. It drives the task until it completes, panics or yields.
    fn task_loop(&mut self) {
        loop {
            let action = self.interpret_loop();
            match action {
                InstAction::Continue => unreachable!("Continue action will not break the loop"),
                InstAction::Return => unsafe {
                    let active_task = self.active_task.as_ref().unwrap().get_mut();
                    let task_finished = active_task
                        .stack_top
                        .take()
                        .expect("The stack top must be available")
                        .with(|f| {
                            let return_val = f.pop();
                            let next_frame = f.caller.take();
                            if let Some(next_frame) = next_frame {
                                next_frame.with(|f| f.push(return_val));
                                active_task.stack_top = Some(next_frame);
                                false
                            } else {
                                // the task finishes
                                active_task.result = Some(return_val);
                                active_task.stack_top = None;
                                true
                            }
                        });
                    if task_finished {
                        break;
                    }
                },
                InstAction::Call
                | InstAction::CallMethod
                | InstAction::TailCall
                | InstAction::TailCallMethod => {
                    let task = unsafe { self.active_task.as_ref().unwrap().get_mut() };
                    let active_frame = task.stack_top.take().expect("The frame must be available");

                    let frame = unsafe { active_frame.get_mut() };
                    let param_cnt = frame.read_param::<u32>();

                    let closure_param_offset = match action {
                        InstAction::Call | InstAction::TailCall => {
                            frame.stack.len() - param_cnt as usize - 1
                        }
                        InstAction::CallMethod | InstAction::TailCallMethod => {
                            frame.stack.len() - param_cnt as usize - 2
                        }
                        _ => unsafe { unreachable_unchecked() },
                    };

                    let func_closure =
                        match frame.stack[closure_param_offset].clone().try_into_closure() {
                            Ok(closure) => closure,
                            Err(_) => {
                                todo!("Panic this task because value is not a closure")
                            }
                        };

                    let self_param = if func_closure.func.binds_self
                        && matches!(action, InstAction::CallMethod | InstAction::TailCallMethod)
                    {
                        Some(frame.stack[frame.stack.len() - param_cnt as usize - 1].clone())
                    } else {
                        None
                    };

                    let iter = self_param.into_iter().chain(
                        frame
                            .stack
                            .drain((frame.stack.len() - param_cnt as usize)..),
                    );

                    let caller = match action {
                        InstAction::Call | InstAction::CallMethod => Some(active_frame.clone()),
                        InstAction::TailCall | InstAction::TailCallMethod => {
                            active_frame.caller.clone()
                        }
                        _ => unsafe { unreachable_unchecked() },
                    };

                    let new_frame = Frame::new(
                        func_closure.func.clone(),
                        caller,
                        iter,
                        &func_closure.capture,
                    );

                    let new_frame = Gc::new(new_frame, &mut self.allocator).expect("Out of memory");
                    task.stack_top = Some(new_frame);
                }

                InstAction::Yield => todo!(),
                InstAction::Panic => todo!(),
            }
        }
    }

    /// The main interpreting loop.
    ///
    /// Unlike the similar structure in QuickJS and other interpreters, this loop is **not**
    /// recursive. Non-builtin function calls break out of this loop and executes as a separate
    /// action, instead of being called as a subroutine.
    fn interpret_loop(&mut self) -> InstAction {
        let frame = unsafe {
            self.active_task
                .as_ref()
                .unwrap()
                .get_mut()
                .stack_top
                .as_ref()
                .expect("Stack top must be available")
                .get_mut()
        };
        loop {
            // We have a unique mutable reference on the frame. This reference will last until the
            // end of loop.
            let inst = frame.read_inst();
            let cx = InstExecCtx {
                alloc: &mut self.allocator,
                task_list: &mut self.task_list,
            };
            let action = exec_inst(inst, frame, cx);
            if action != InstAction::Continue {
                break action;
            }
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
    task_list: &'r mut TaskList,
}

impl<'r> InstExecCtx<'r> {
    fn create_task(
        &mut self,
        name: Option<ArcStr>,
        closure: Gc<Closure>,
        params: &[Val],
    ) -> Gc<Task> {
        let frame = Frame::new(
            closure.func.clone(),
            None,
            params.iter().cloned(),
            &closure.capture,
        );
        let frame = Gc::new(frame, self.alloc).expect("OOM");
        let task = Task {
            name,
            is_detached: false,
            stack_top: Some(frame),
            next: None,
            result: None,
        };
        Gc::new(task, self.alloc).expect("OOM")
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
