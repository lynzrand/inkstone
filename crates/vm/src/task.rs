use std::ptr::NonNull;

use inkstone_util::string::ArcStr;

use crate::vm::Frame;

pub struct Task {
    name: Option<ArcStr>,
    stack_top: NonNull<Frame>,
}
