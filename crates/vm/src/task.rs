use std::ptr::NonNull;

use inkstone_util::string::ArcStr;

use crate::vm::Frame;

pub struct Task {
    /// The name of this task
    pub(crate) name: Option<ArcStr>,

    /// Whether this task is detached
    pub(crate) is_detached: bool,

    /// The top of the call stack
    pub(crate) stack_top: NonNull<Frame>,

    /// Next task in the list
    pub(crate) next: *mut Task,
}
