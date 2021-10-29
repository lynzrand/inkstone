use integer_encoding::VarInt;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::fmt::Display;

macro_rules! define_inst {
    (
        $(#[$meta:meta])*
        // type names
        $ty_vis:vis $type:ident,
        // item definition
        $($name:ident
            // params
            $(($param_name:ident : $param:ty))?
            // number
            $(= $n:literal)?),*
    ) => {
        $(#[$meta])*
        #[derive(Clone, Debug, PartialEq, Copy, IntoPrimitive, TryFromPrimitive)]
        #[repr(u8)]
        $ty_vis enum $type {$(
            $name $(= $n)?
        ),*}

        impl $type {
            pub fn parse_param<P: VarInt>(self, buf: &[u8]) -> Option<(P, usize)> {
                P::decode_var(buf)
            }

            /// Returns the count of parameters of this instruction
            pub fn has_param(inst: u8) -> bool {
                match $type::try_from_primitive(inst) {$(
                    Ok($type::$name) => {
                        let _res = false;
                        $(let _res = true; let $param_name = ();)?
                        _res
                    }
                ),*
                    Err(_) => false
                }
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
    ($($name:ident, $ty:ty),*) => {};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Reg(u32);

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

define_inst! {
    /// The list of instructions. Each instruction either has 0 or exactly 1 parameter.
    /// The parameter must be an integer, and will be encoded in VarInt encoding.
    pub Inst,

    // stack manipulation
    Pop,
    Dup,

    // constants
    PushI64(num: i64),
    PushF64(idx: u32),
    PushConst(idx: u32),
    PushTrue,
    PushFalse,
    PushNil,

    // arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    ShrA,

    And,
    Or,

    // compound types
    TupleNew(len: u32),
    ArrayNew,
    MapNew,
    SetPrototype,
    GetPrototype,

    LoadField(idx: u32),
    StoreField(idx: u32),
    LoadFieldDyn,
    StoreFieldDyn,
    LoadFieldChain(len: u32),

    // load/stores
    LoadLocal(reg: Reg),
    StoreLocal(reg: Reg),

    // scope operations
    ScopeNew,
    ScopeLoad(idx: u32),
    ScopeStore(idx: u32),
    ScopeLoadDyn,
    ScopeStoreDyn,
    ScopeAssignAll,

    PushGlobalScope,
    PushModuleScope,

    // function
    Call(n_args: u32),
    TailCall(n_args: u32),

    Return,

    // tasks
    Yield,
    NewTask,
    JoinTask,

    Nop
}
