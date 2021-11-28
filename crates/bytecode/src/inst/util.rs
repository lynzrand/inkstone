use std::fmt::{Debug, DebugList, Display};

use bytes::{Buf, BufMut};

use crate::inst::Inst;

use super::{param, IParamType};

pub trait InstContainer: Buf {
    /// Move to the specified **absolute** position
    fn seek(&mut self, position: usize);

    fn read_inst(&mut self) -> Inst {
        Inst::from_ordinal(self.get_u8()).unwrap_or(Inst::Invalid)
    }

    fn read_param<P: IParamType>(&mut self) -> P {
        P::parse(self)
    }

    fn validate_param<P: IParamType>(&mut self) -> bool {
        P::validate(self)
    }
}

/// A simple instruction reader
pub struct InstReader<T> {
    buf: T,
    offset: usize,
}

impl<T: AsRef<[u8]>> InstReader<T> {
    pub fn new(buf: T) -> Self {
        InstReader { buf, offset: 0 }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn unwrap(self) -> T {
        self.buf
    }
}

impl<T: AsRef<[u8]>> Buf for InstReader<T> {
    fn remaining(&self) -> usize {
        self.buf.as_ref().len() - self.offset
    }

    fn chunk(&self) -> &[u8] {
        &self.buf.as_ref()[self.offset..]
    }

    fn advance(&mut self, cnt: usize) {
        self.offset += cnt;
    }
}

impl<T: AsRef<[u8]>> InstContainer for InstReader<T> {
    fn seek(&mut self, position: usize) {
        self.offset = position
    }
}

pub struct InstContainerFormatter<'a>(pub &'a [u8]);

impl<'a> InstContainerFormatter<'a> {
    fn fmt_inner(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut container = InstReader::new(self.0);
        while container.remaining() > 0 {
            let inst = container.read_inst();
            f.write_str("    ")?;
            inst.fmt_with_param(&mut container, f)?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl<'a> Display for InstContainerFormatter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "[")?;
        self.fmt_inner(f)?;
        write!(f, "]")
    }
}

impl<'a> Debug for InstContainerFormatter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
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
