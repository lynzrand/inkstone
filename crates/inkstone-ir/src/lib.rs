pub struct Function {}

/// A virtual register slot in function
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
#[repr(transparent)]
pub struct Reg(u32);

/// An SSA instruction.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Inst {
    /// Instruction Kind
    kind: InstKind,
    /// Instruction Flags
    flags: InstFlags,
    /// Destination
    dest: Reg,
    /// The 1st argument
    p1: Reg,
    /// The 2nd argument
    p2: Reg,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
#[repr(u16)]
pub enum InstKind {
    Mov = 0,

    // int
    IAdd = 16,
    ISub,
    IMul,
    IDiv,
    IAnd,
    IOr,
    IXor,
    IEq,
    ILt,
    ILe,

    // float
    FAdd = 32,
    FSub,
    FMul,
    FDiv,
    FEq,
    FLt,
    FLe,

    I2F,
    F2I,

    // bool
    BAnd,
    BOr,
    BXor,
    BNot,

    // constants
    IConst = 48,
    FConst,
    BTrue,
    BFalse,
    Nil,

    // scope
    ScopeNew = 64,
    ScopeLoad,
    ScopeStore,
    ScopeCurrent,
    // tuple
    /// `dest <- TupleNew(p1=length, _)`
    TupleNew,
    // object
    /// `dest <- ObjectNew(p1=initial_len)`
    ObjectNew,
    // array
    /// `dest <- ArrayNew(p1=initial_len)`
    ArrayNew,
    // closure
    /// `dest <- ClosureNew(p1=function, p2=scope)`
    ClosureNew,

    // object operations
    /// `dest[p1] <- p2`
    SubscriptSet,
    /// `dest <- p1[p2]`
    SubscriptLoad,

    // flow control
    /// `tail jump to (dest=function, p1=scope, p2=args)`
    Tail = 96,
    /// `tail jump to (dest=function, p1=scope, p2=cond)`
    TailIf,

    Call,
}

bitflags::bitflags! {
    pub struct InstFlags: u16 {

    }
}
