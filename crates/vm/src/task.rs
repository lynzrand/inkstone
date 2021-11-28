use std::ptr::NonNull;

use inkstone_util::string::ArcStr;

use crate::vm::Frame;

pub struct Task {
    pub(crate) name: Option<ArcStr>,
    pub(crate) stack_top: NonNull<Frame>,
}
