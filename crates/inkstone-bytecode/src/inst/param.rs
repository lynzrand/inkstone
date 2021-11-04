use bytes::{Buf, BufMut};
use std::fmt::Display;

pub trait IParamType: Sized {
    const PARAM_ENUM_TY: ParamType;

    const MAX_RESERVE_LEN: usize;

    /// Validate that the buffer's head pointer contains valid param
    fn validate(r: impl Buf) -> bool;

    /// Parse the param from the buffer's head pointer. **Panics if it doesn't succeed.**
    fn parse(r: impl Buf) -> Self;

    /// Write the param into the given buffer.
    fn write(&self, w: impl BufMut);
}

#[allow(unused)]
pub(crate) fn u32_write_length(v: u32) -> usize {
    match v {
        0..=0x7f => 1,
        0x80..=0xff => 2,
        0x100..=0xffff => 3,
        0x10000..=0xffffffff => 5,
    }
}

#[allow(unused)]
pub(crate) fn u64_write_length(v: u64) -> usize {
    match v {
        0x1_0000_0000.. => 9,
        _ => u32_write_length(v as u32),
    }
}

pub(crate) fn write_u32(v: u32, mut buf: impl BufMut) {
    match v {
        0..=0x7f => {
            buf.put_u8(v as u8);
        }
        0x80..=0xff => {
            buf.put_u8(0x81);
            buf.put_u8(v as u8);
        }
        0x100..=0xffff => {
            buf.put_u8(0x82);
            buf.put_u16_le(v as u16);
        }
        0x10000..=0xffffffff => {
            buf.put_u8(0x83);
            buf.put_u32_le(v);
        }
    }
}

pub(crate) fn write_i32(v: i32, mut buf: impl BufMut) {
    match v {
        -0x40..=0x3f => {
            buf.put_u8(i8_to_u7(v as i8));
        }
        0x40..=0x7f | -0x80..=-0x41 => {
            buf.put_u8(0x81);
            buf.put_i8(v as i8);
        }
        0x80..=0x7fff | -0x8000..=-0x81 => {
            buf.put_u8(0x82);
            buf.put_i16_le(v as i16);
        }
        _ => {
            buf.put_u8(0x83);
            buf.put_i32_le(v);
        }
    }
}

/// Convert a compressed 7-bit unsigned integer into 8-bit integer
fn u7_to_i8(u7: u8) -> i8 {
    ((u7 << 1) as i8) >> 1
}

/// Is n representable as a `u7` value?
fn representable_in_u7(n: i32) -> bool {
    n >= -64 && n <= 63
}

/// Convert an 8-bit integer into compressed 7-bit unsigned integer
fn i8_to_u7(i8: i8) -> u8 {
    (i8 as u8) & 0x7f
}

fn validate_u32(mut buf: impl Buf) -> bool {
    fn check_and_advance(mut buf: impl Buf, n: usize) -> bool {
        if buf.remaining() >= n {
            buf.advance(n);
            true
        } else {
            false
        }
    }
    let n = buf.get_u8();
    if n & 0x80 == 0 {
        true
    } else if n == 0x81 {
        check_and_advance(buf, 1)
    } else if n == 0x82 {
        check_and_advance(buf, 2)
    } else if n == 0x83 {
        check_and_advance(buf, 4)
    } else {
        false
    }
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
fn read_u32(mut buf: impl Buf) -> u32 {
    let n = buf.get_u8();
    if n & 0x80 == 0 {
        n as u32
    } else if n == 0x81 {
        buf.get_u8() as u32
    } else if n == 0x82 {
        buf.get_u16_le() as u32
    } else if n == 0x83 {
        buf.get_u32_le()
    } else {
        panic!("Invalid data")
    }
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
fn read_i32(mut buf: impl Buf) -> i32 {
    let n = buf.get_u8();
    if n & 0x80 == 0 {
        u7_to_i8(n) as i32
    } else if n == 0x81 {
        buf.get_i8() as i32
    } else if n == 0x82 {
        buf.get_i16_le() as i32
    } else if n == 0x83 {
        buf.get_i32_le()
    } else {
        panic!("invalid data")
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

        impl $ty_name {
            pub fn validate(self, buf: impl Buf) -> bool {
                match self {$(
                    Self::$name => <$ty>::validate(buf),
                )*}
            }
        }
    };
}

const U32_MAX_RESERVE_LEN: usize = 5;

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

    const MAX_RESERVE_LEN: usize = 5;

    fn validate(r: impl Buf) -> bool {
        validate_u32(r)
    }

    fn parse(r: impl Buf) -> Self {
        Reg(read_u32(r))
    }

    fn write(&self, w: impl BufMut) {
        write_u32(self.0, w)
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
    const MAX_RESERVE_LEN: usize = U32_MAX_RESERVE_LEN;

    fn validate(r: impl Buf) -> bool {
        validate_u32(r)
    }

    fn parse(r: impl Buf) -> Self {
        Idx(read_u32(r))
    }

    fn write(&self, w: impl BufMut) {
        write_u32(self.0, w)
    }
}

impl IParamType for u32 {
    const PARAM_ENUM_TY: ParamType = ParamType::Cnt;
    const MAX_RESERVE_LEN: usize = U32_MAX_RESERVE_LEN;

    fn validate(r: impl Buf) -> bool {
        validate_u32(r)
    }

    fn parse(r: impl Buf) -> Self {
        read_u32(r)
    }

    fn write(&self, w: impl BufMut) {
        write_u32(*self, w)
    }
}

impl IParamType for i32 {
    const PARAM_ENUM_TY: ParamType = ParamType::Int;
    const MAX_RESERVE_LEN: usize = U32_MAX_RESERVE_LEN;

    fn validate(r: impl Buf) -> bool {
        validate_u32(r)
    }

    fn parse(r: impl Buf) -> Self {
        read_i32(r)
    }

    fn write(&self, w: impl BufMut) {
        write_i32(*self, w)
    }
}

impl IParamType for () {
    const PARAM_ENUM_TY: ParamType = ParamType::None;

    const MAX_RESERVE_LEN: usize = 0;

    fn validate(_: impl Buf) -> bool {
        true
    }

    fn parse(_r: impl Buf) -> Self {}

    fn write(&self, _w: impl BufMut) {}
}

param_types! {
    ParamType,
    None,(),
    /// A register in scope
    Reg, Reg,
    /// An index in constant table
    Idx, Idx,
    /// An integer constant
    Int, i32,
    /// Count or length of another value
    Cnt, u32
}
