use std::sync::Arc;

use inkstone_bytecode::inst::Inst;

use crate::value::{Function, Val};

struct Frame {
    func: Arc<Function>,
    stack: Vec<Val>,
}

pub struct InkstoneVm {}

impl InkstoneVm {
    fn exec_inst(&mut self) {
        let inst: Inst = todo!();

        match inst {
            Inst::Pop => todo!(),
            Inst::Dup => todo!(),
            Inst::PushI32 => todo!(),
            Inst::PushConst => todo!(),
            Inst::PushTrue => todo!(),
            Inst::PushFalse => todo!(),
            Inst::PushNil => todo!(),
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
}
