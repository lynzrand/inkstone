use std::fmt::Display;

pub(crate) trait IParamType: Sized {
    const PARAM_ENUM_TY: ParamType;

    const FROM_U64: bool = false;
    const FROM_U32: bool = true;
    const FROM_U16: bool = true;
    const FROM_U8: bool = true;
    const FROM_U7: bool = true;

    /// Read this param from a `u64` value
    fn from_u64(_i: u64) -> Self {
        panic!("Parsing from u64 is not supported!")
    }

    /// Read this param from a `u32` value
    fn from_u32(i: u32) -> Self;

    /// Read this param from a `u16` value. Defaults to unsigned extension.
    fn from_u16(i: u16) -> Self {
        Self::from_u32(i as u32)
    }

    /// Read this param from a `u8` value. Defaults to unsigned extension.
    fn from_u8(i: u8) -> Self {
        Self::from_u32(i as u32)
    }

    /// Read this param from a `u7` value. Defaults to unsigned extension.
    fn from_u7(i: u8) -> Self {
        Self::from_u32(i as u32)
    }

    /// Parse this param from buffer. Returns the parsed value and bytes to advance
    /// if succeeds.
    ///
    /// Defaults to parsing a 8, 16 or 32-bit little endian value:
    ///
    /// - `0x00 -- 0x7f` are identified as regular `u7` values
    /// - `0x81` indicates the next 1 bytes to be parsed as a `u8`
    /// - `0x82` indicates the next 2 bytes to be parsed as a `u16`
    /// - `0x83` indicates the next 4 bytes to be parsed as a `u32`
    /// - `0x84` indicates the next 8 bytes to be parsed as a `u64`
    fn parse(buf: &[u8]) -> Option<(Self, usize)> {
        if buf.is_empty() {
            return None;
        }

        let n = buf[0];
        if n & 0x80 == 0 && Self::FROM_U7 {
            // parse from a `u7`
            Some((Self::from_u7(n), 1))
        } else if n == 0x81 && Self::FROM_U8 {
            if buf.len() < 2 {
                return None;
            }
            let n = buf[1];
            Some((Self::from_u8(n), 2))
        } else if n == 0x82 && Self::FROM_U16 {
            if buf.len() < 3 {
                return None;
            }
            let mut b = [0u8; 2];
            b.copy_from_slice(&buf[1..3]);
            Some((Self::from_u16(u16::from_le_bytes(b)), 3))
        } else if n == 0x83 && Self::FROM_U32 {
            if buf.len() < 5 {
                return None;
            }
            let mut b = [0u8; 4];
            b.copy_from_slice(&buf[1..5]);
            Some((Self::from_u32(u32::from_le_bytes(b)), 5))
        } else if n == 0x84 && Self::FROM_U64 {
            if buf.len() < 9 {
                return None;
            }
            let mut b = [0u8; 8];
            b.copy_from_slice(&buf[1..9]);
            Some((Self::from_u64(u64::from_le_bytes(b)), 9))
        } else {
            None
        }
    }

    /// Bytes to reserve when writing this value
    fn write_length(&self) -> usize;

    /// Write this value into the given buffer.
    /// The caller must ensure that `buf.len() >= self.write_length()` or
    /// else this method may panic.
    fn write_buf(&self, buf: &mut [u8]);
}

pub(crate) fn u32_write_length(v: u32) -> usize {
    match v {
        0..=0x7f => 1,
        0x80..=0xff => 2,
        0x100..=0xffff => 3,
        0x10000..=0xffffffff => 5,
    }
}

pub(crate) fn u64_write_length(v: u64) -> usize {
    match v {
        0x1_0000_0000.. => 9,
        _ => u32_write_length(v as u32),
    }
}

pub(crate) fn write_u32(v: u32, buf: &mut [u8]) {
    match v {
        0..=0x7f => {
            buf[0] = v as u8;
        }
        0x80..=0xff => {
            buf[0] = 0x81;
            buf[1] = v as u8;
        }
        0x100..=0xffff => {
            buf[0] = 0x82;
            let bytes = (v as u16).to_le_bytes();
            buf[1..3].copy_from_slice(&bytes);
        }
        0x10000..=0xffffffff => {
            buf[0] = 0x83;
            let bytes = v.to_le_bytes();
            buf[1..5].copy_from_slice(&bytes);
        }
    }
}

pub(crate) fn write_u64(v: u64, buf: &mut [u8]) {
    match v {
        0x1_0000_0000.. => {
            buf[0] = 0x84;
            let bytes = v.to_le_bytes();
            buf[1..9].copy_from_slice(&bytes);
        }
        _ => write_u32(v as u32, buf),
    }
}

macro_rules! param_types {
    ($ty_name:ident,
        $($(#[$variant_meta:meta])* $name:ident, $ty:ty),*
    ) => {
        pub enum $ty_name {$(
            $(#[$variant_meta])*
            $name
        ),*}
    };
}

/// Represents a local register
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Reg(u32);

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl IParamType for Reg {
    const PARAM_ENUM_TY: ParamType = ParamType::Reg;

    fn from_u32(i: u32) -> Self {
        Reg(i)
    }

    fn write_length(&self) -> usize {
        u32_write_length(self.0)
    }

    fn write_buf(&self, buf: &mut [u8]) {
        write_u32(self.0, buf)
    }
}

/// Represents an index of constant table
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Idx(u32);

impl Display for Idx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl IParamType for Idx {
    const PARAM_ENUM_TY: ParamType = ParamType::Idx;

    fn from_u32(i: u32) -> Self {
        Idx(i)
    }

    fn write_length(&self) -> usize {
        u32_write_length(self.0)
    }

    fn write_buf(&self, buf: &mut [u8]) {
        write_u32(self.0, buf)
    }
}

impl IParamType for u32 {
    const PARAM_ENUM_TY: ParamType = ParamType::Cnt;

    fn from_u32(i: u32) -> Self {
        i
    }

    fn write_length(&self) -> usize {
        u32_write_length(*self)
    }

    fn write_buf(&self, buf: &mut [u8]) {
        write_u32(*self, buf)
    }
}

impl IParamType for i32 {
    const PARAM_ENUM_TY: ParamType = ParamType::Int;

    fn from_u32(i: u32) -> Self {
        i as i32
    }

    /// Signed extend from u16
    fn from_u16(i: u16) -> Self {
        i as i16 as i32
    }

    /// Signed extend from u8
    fn from_u8(i: u8) -> Self {
        i as i8 as i32
    }

    /// Signed extend from u7
    fn from_u7(i: u8) -> Self {
        let signed_extend_i = (i << 1) as i8 >> 1;
        signed_extend_i as i32
    }

    fn write_length(&self) -> usize {
        u32_write_length(*self as u32)
    }

    fn write_buf(&self, buf: &mut [u8]) {
        write_u32(*self as u32, buf)
    }
}

param_types! {
    ParamType,
    /// A register in scope
    Reg, Reg,
    /// An index in constant table
    Idx, Idx,
    /// An integer constant
    Int, i32,
    /// Count or length of another value
    Cnt, u32
}
