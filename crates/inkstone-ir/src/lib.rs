use std::fmt::Display;

pub struct Function {
    pub name: Option<String>,
    pub inst: Vec<Inst>,
    pub param_cnt: u32,
    pub binds_self: bool,
    pub has_rest_param: bool,
    pub constants: Vec<()>,
}

macro_rules! define_inst {
    (
        // type names
        $type:ident, $ident_ty:ident,
        // item definition
        $($name:ident
            // params
            $(($($param_name:ident : $param:ty),+))?
            // number
            $(= $n:literal)?),*
    ) => {
        #[derive(Clone, Debug, PartialEq, Copy)]
        #[repr(C)]
        pub enum $type {$(
            $name $(($($param),+))? $(= $n)?
        ),*}

        #[derive(Clone, Debug, PartialEq, Copy)]
        #[repr(u16)]
        pub enum $ident_ty {$(
            $name $(= $n)?
        ),*}

        impl $type {
            pub fn to_id(&self) -> $ident_ty {
                match self {$(
                    Self::$name $(($($param_name),+))? => {
                        $($(let _ = $param_name;)+)?
                        $ident_ty::$name
                    }
                ),*}
            }
        }

        impl ::std::fmt::Display for $type {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {$(
                    Self::$name $(($($param_name),+))? => {
                        write!(f, stringify!($name))?;
                        $($(
                            write!(f, " {}", $param_name)?;
                        )+)?
                    }
                )*}
                Ok(())
            }
        }

        impl ::std::fmt::Display for $ident_ty {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Reg(u32);

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

define_inst! {
    Inst,
    InstId,

    // stack manipulation
    Pop,

    // constants
    PushI64(num: i64),
    PushF64(num: u64),
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
    NewTask(reg: Reg),
    JoinTask
}
