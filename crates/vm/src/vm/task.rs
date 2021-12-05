use inkstone_util::string::ArcStr;

use crate::gc::Gc;
use crate::value::Val;
use crate::vm::frame::Frame;

pub struct Task {
    /// The name of this task
    pub(crate) name: Option<ArcStr>,

    /// Whether this task is detached
    pub(crate) is_detached: bool,

    /// The top of the call stack
    pub(crate) stack_top: Option<Gc<Frame>>,

    /// Next task in the list
    pub(crate) next: Option<Gc<Task>>,

    pub(crate) result: Option<Val>,
}

mod trace_impl {
    use crate::gc::{GcTracerVTable, Trace};
    use crate::*;

    use super::Task;

    impl Trace for Task {
        fn trace(&self, mut tracer: vtable::VRefMut<GcTracerVTable>) {
            if let Some(frame) = &self.stack_top {
                frame.trace(tracer.borrow_mut())
            }
            if let Some(next) = &self.next {
                next.trace(tracer.borrow_mut())
            }
            if let Some(result) = &self.result {
                result.trace(tracer)
            }
        }
    }

    TraceVTable_static! {#[allow(non_upper_case_globals)] static Task_TraceVTable for Task }
}
