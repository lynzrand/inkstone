use bytes::BufMut;

use crate::inst::Inst;

use super::{param, IParamType};

pub trait InstContainer {
    fn seek(&mut self, offset: u32);
    fn read_u8(&mut self) -> u8;
    fn read_param<T: param::IParamType>(&mut self) -> T;
}

pub trait InstContainerMut {
    fn write_u8(&mut self, v: u8);
    fn write_param(&mut self, v: impl IParamType);

    fn emit(&mut self, i: Inst) -> &mut Self {
        self.write_u8(i.ordinal());
        self
    }

    fn emit_p(&mut self, i: Inst, v: impl IParamType) -> &mut Self {
        self.write_u8(i.ordinal());
        self.write_param(v);
        self
    }
}

impl<T> InstContainerMut for T
where
    T: BufMut,
{
    fn write_u8(&mut self, v: u8) {
        self.put_u8(v);
    }

    fn write_param(&mut self, v: impl IParamType) {
        v.write(self);
    }
}
