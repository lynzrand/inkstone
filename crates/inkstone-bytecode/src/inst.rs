use integer_encoding::VarInt;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::convert::TryInto;
use std::fmt::Display;

macro_rules! define_inst {
    (
        $(#[$meta:meta])*
        // type names
        $ty_vis:vis $type:ident,
        // instruction variant definition
        $(
            // metadata for this variant. Will be put inside the final enum
            $(#[$variant_meta:meta])*
            // variant name
            $name:ident
            // params
            $(($param_name:ident : $param:ident))?
            // number
            $(= $n:literal)?

            // Pop and push specs
            $(                   >> $push_cnt:literal)?
            $(                            << $pop_cnt:literal)?
        ),*
    ) => {
        $(#[$meta])*
        #[derive(Clone, Debug, PartialEq, Copy, IntoPrimitive, TryFromPrimitive)]
        #[repr(u8)]
        $ty_vis enum $type {$(
            $(#[$variant_meta])*
            $name $(= $n)?
        ),*}

        impl $type {
            pub fn parse_param<P: VarInt>(self, buf: &[u8]) -> Option<(P, usize)> {
                P::decode_var(buf)
            }

            /// Returns the count of parameters of this instruction
            pub fn param_type(inst: u8) -> Option<ParamType> {
                #[allow(path_statements)]
                match $type::try_from_primitive(inst) {$(
                    Ok($type::$name) => {
                        None::<ParamType>
                        $(; Some(ParamType::$param))?
                    }
                ),*
                    Err(_) => None
                }
            }

            pub fn pop_count(self) -> usize {
                match self {$(
                    Self::$name => {
                        0
                        $(; $pop_cnt)?
                    }
                ),*}
            }

            pub fn push_count(self) -> usize {
                match self {$(
                    Self::$name => {
                        0
                        $(; $push_cnt)?
                    }
                ),*}
            }
        }

        impl ::std::fmt::Display for $type {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {$(
                    Self::$name => {
                        write!(f, stringify!($name))?;
                    }
                )*}
                Ok(())
            }
        }
    };
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

pub trait IParamType: Sized {
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

    /// Bytes to reserve when parsing/seralizing this value
    fn reserve_bytes() -> usize {
        9
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
}

impl IParamType for u32 {
    const PARAM_ENUM_TY: ParamType = ParamType::Cnt;

    fn from_u32(i: u32) -> Self {
        i
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

// instruction definition.
//
// Each line is an instruction. `>>` and `<<` refers to the number of arguments
// popped from and pushed into the stack.
//
// Each instruction may have at most 1 argument. It must be equal or less than
// 32 bits.
define_inst! {
    /// The list of instructions. Each instruction either has 0 or exactly 1 parameter.
    /// The parameter must be an integer, and will be encoded in VarInt encoding.
    pub Inst,

    // stack manipulation
    /// Pop a value from stack
    Pop                          >> 1,
    /// Duplicate the stack top value
    Dup                          >> 1     << 2,

    // constants
    /// Push a 32-bit integer constant
    PushI32(num: Int)                     << 1,
    /// Push a value inside the constant table
    PushConst(idx: Idx)                   << 1,
    /// Push boolean true
    PushTrue                              << 1,
    /// Push boolean false
    PushFalse                             << 1,
    /// Push nil
    PushNil                               << 1,

    // arithmetic
    /// Add stack top values, push result
    Add                          >> 2     << 1,
    /// Subtract stack top values, push result
    Sub                          >> 2     << 1,
    ///
    Mul                          >> 2     << 1,
    Div                          >> 2     << 1,
    Pow                          >> 2     << 1,

    BitAnd                       >> 2     << 1,
    BitOr                        >> 2     << 1,
    BitXor                       >> 2     << 1,
    Shl                          >> 2     << 1,
    Shr                          >> 2     << 1,
    ShrL                         >> 2     << 1,

    And                          >> 2     << 1,
    Or                           >> 2     << 1,

    // compound types
    /// Create a new tuple. Pops additional `len` items to populate the tuple.
    TupleNew(len: Cnt)                    << 1,
    /// Creates a new array. Pops additional `len` items into the array.
    ArrayNew(len: Cnt)                    << 1,
    /// Creates a new map.
    MapNew                                << 1,
    /// Set the prototype of the object at stack top
    SetPrototype                 >> 2     << 1,
    /// Get the prototype of the object at stack top
    GetPrototype                 >> 1     << 1,

    /// Load a field from an object using the `idx`th constant in table as index.
    LoadField(idx: Idx)          >> 1     << 1,
    /// Store a field into an object using the `idx`th constant in table as index.
    StoreField(idx: Idx)         >> 2,
    /// Load a field from an object using a dynamic symbol or number as index.
    LoadFieldDyn                 >> 2     << 1,
    /// Storing a field from an object using a dynamic symbol or number as index.
    StoreFieldDyn                >> 3,
    /// Load an object using a chain of indices. Pops additional `len` items to form the path.
    LoadFieldChain(len: Cnt)     >> 1,

    // load/stores
    /// Load a value from the `reg`th slot of local scope
    LoadLocal(reg: Reg)                   << 1,
    /// Store a value into the `reg`th slot of local scope
    StoreLocal(reg: Reg)         >> 1,

    // scope operations
    /// Create a new scope that has `len` slots
    ScopeNew(len: Cnt)                    << 1,
    /// Load a value from the `reg`th slot of the given scope
    ScopeLoad(idx: Reg)          >> 1     << 1,
    /// Store a value into the `reg`th slot of the given scope
    ScopeStore(idx: Reg)         >> 2,
    /// Load a value from a slot of the given scope
    ScopeLoadDyn                 >> 2     << 1,
    /// Store a value into a slot of the given scope
    ScopeStoreDyn                >> 3,
    /// Assign all values in the first scope into the second scope
    ScopeAssignAll               >> 2,

    /// Push the global scope onto stack
    PushGlobalScope                       << 1,
    /// Push the current module scope onto stack
    PushModuleScope                       << 1,
    /// Push the local function scope onto stack
    PushLocalScope                        << 1,

    // function
    /// Calls the function using the given arguments
    Call(n_args: Cnt)            >> 1     << 1,
    /// Calls the function using the given arguments, with `self` bound to the
    /// 1st argument if possible.
    CallMethod(n_args: Cnt)      >> 1     << 1,
    /// Return this function call with the result of the function using the given arguments
    TailCall(n_args: Cnt)        >> 1,
    /// Return this function call with the result of the function using the given arguments
    /// with `self` bound to the 1st argument if possible
    TailCallMethod(n_args: Cnt)  >> 1,

    /// Return the current function
    Return                       >> 1,

    // tasks
    /// Yield the current task with the given value. Returns the param
    /// the waker provides, or `nil` if no value was provided.
    Yield                        >> 1     << 1,
    /// Create a new task using the given closure.
    NewTask                      >> 1     << 1,
    /// Wake and poll the given task with the given parameter. Returns `(:pending, value)`
    /// if the task yields, or `(:completed, return_value)` if the task completes.
    PollTask                     >> 2     << 1,
    /// Detach the given task, allowing it to be polled to complete by the runtime.
    /// The task will be polled with no value every time it's waken.
    DetachTask                   >> 1,
    /// Wake the given task.
    WakeTask                     >> 1,

    /// Do nothing.
    Nop
}
