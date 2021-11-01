use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use std::fmt::Display;
use std::io::{Read, Write};

pub(crate) trait IParamType: Sized {
    const PARAM_ENUM_TY: ParamType;

    const MAX_RESERVE_LEN: usize;

    fn parse(r: impl Read) -> std::io::Result<Self>;
    fn write(&self, w: impl Write) -> std::io::Result<()>;
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

pub(crate) fn write_u32(v: u32, mut buf: impl Write) -> std::io::Result<()> {
    match v {
        0..=0x7f => {
            buf.write_u8(v as u8)?;
        }
        0x80..=0xff => {
            buf.write_u8(0x81)?;
            buf.write_u8(v as u8)?;
        }
        0x100..=0xffff => {
            buf.write_u8(0x82)?;
            buf.write_u16::<LE>(v as u16)?;
        }
        0x10000..=0xffffffff => {
            buf.write_u8(0x83)?;
            buf.write_u32::<LE>(v)?;
        }
    }
    Ok(())
}

pub(crate) fn write_i32(v: i32, mut buf: impl Write) -> std::io::Result<()> {
    match v {
        -0x40..=0x3f => {
            buf.write_u8(i8_to_u7(v as i8))?;
        }
        0x40..=0x7f | -0x80..=-0x41 => {
            buf.write_u8(0x81)?;
            buf.write_i8(v as i8)?;
        }
        0x80..=0x7fff | -0x8000..=-0x81 => {
            buf.write_u8(0x82)?;
            buf.write_i16::<LE>(v as i16)?;
        }
        _ => {
            buf.write_u8(0x83)?;
            buf.write_i32::<LE>(v)?;
        }
    }
    Ok(())
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

/// Parse this param from buffer. Returns the parsed value and bytes to advance
/// if succeeds.
///
/// Defaults to parsing a 8, 16 or 32-bit little endian value:
///
/// - `0x00 -- 0x7f` are identified as regular `u7` values
/// - `0x81` indicates the next 1 bytes to be parsed as a `u8`
/// - `0x82` indicates the next 2 bytes to be parsed as a `u16`
/// - `0x83` indicates the next 4 bytes to be parsed as a `u32`
fn read_u32(mut buf: impl Read) -> std::io::Result<u32> {
    let n = buf.read_u8()?;
    if n & 0x80 == 0 {
        Ok(n as u32)
    } else if n == 0x81 {
        Ok(buf.read_u8()? as u32)
    } else if n == 0x82 {
        Ok(buf.read_u16::<LE>()? as u32)
    } else if n == 0x83 {
        Ok(buf.read_u32::<LE>()?)
    } else {
        Err(std::io::ErrorKind::InvalidData.into())
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
fn read_i32(mut buf: impl Read) -> std::io::Result<i32> {
    let n = buf.read_u8()?;
    if n & 0x80 == 0 {
        Ok(u7_to_i8(n) as i32)
    } else if n == 0x81 {
        Ok(buf.read_i8()? as i32)
    } else if n == 0x82 {
        Ok(buf.read_i16::<LE>()? as i32)
    } else if n == 0x83 {
        Ok(buf.read_i32::<LE>()?)
    } else {
        Err(std::io::ErrorKind::InvalidData.into())
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

    fn parse(r: impl Read) -> std::io::Result<Self> {
        read_u32(r).map(Reg)
    }

    fn write(&self, w: impl Write) -> std::io::Result<()> {
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

    fn parse(r: impl Read) -> std::io::Result<Self> {
        read_u32(r).map(Idx)
    }

    fn write(&self, w: impl Write) -> std::io::Result<()> {
        write_u32(self.0, w)
    }
}

impl IParamType for u32 {
    const PARAM_ENUM_TY: ParamType = ParamType::Cnt;
    const MAX_RESERVE_LEN: usize = U32_MAX_RESERVE_LEN;

    fn parse(r: impl Read) -> std::io::Result<Self> {
        read_u32(r)
    }

    fn write(&self, w: impl Write) -> std::io::Result<()> {
        write_u32(*self, w)
    }
}

impl IParamType for i32 {
    const PARAM_ENUM_TY: ParamType = ParamType::Int;
    const MAX_RESERVE_LEN: usize = U32_MAX_RESERVE_LEN;

    fn parse(r: impl Read) -> std::io::Result<Self> {
        read_i32(r)
    }

    fn write(&self, w: impl Write) -> std::io::Result<()> {
        write_i32(*self, w)
    }
}

impl IParamType for () {
    const PARAM_ENUM_TY: ParamType = ParamType::None;

    const MAX_RESERVE_LEN: usize = 0;

    fn parse(_r: impl Read) -> std::io::Result<Self> {
        Ok(())
    }

    fn write(&self, _w: impl Write) -> std::io::Result<()> {
        Ok(())
    }
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
