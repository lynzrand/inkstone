use std::ptr::NonNull;

use inkstone_util::string::ArcStr;

use crate::value::Val;
use crate::vm::Frame;

pub struct Task {
    /// The name of this task
    pub(crate) name: Option<ArcStr>,

    /// Whether this task is detached
    pub(crate) is_detached: bool,

    /// The top of the call stack
    pub(crate) stack_top: Option<NonNull<Frame>>,

    /// Next task in the list
    pub(crate) next: *mut Task,

    pub(crate) result: Option<Val>,
}
