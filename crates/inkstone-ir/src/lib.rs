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
    Nop = 0,
    // int
    IAdd = 16,
    ISub,
    IMul,
    IDiv,
    IAnd,
    IOr,
    IXor,
    IEq,
    INeq,
    IGt,
    ILt,
    IGe,
    ILe,
    // float
    FAdd = 32,
    FSub,
    FMul,
    FDiv,
    FEq,
    FNeq,
    FGt,
    FLt,
    FGe,
    FLe,
    // bool
    BAnd,
    BOr,
    BXor,
    BNot,
    // scope
    ScopeNew = 48,
    ScopeLoad,
    ScopeStore,
    // tuple
    /// `dest <- TupleNew(p1=length, _)`
    TupleNew,
    // object
    /// `dest[p1] <- p2`
    SubscriptSet,
    /// `dest <- p1[p2]`
    SubscriptLoad,
    // closure
    /// `dest <- ClosureNew(p1=function, p2=scope)`
    ClosureNew,
    // flow control
    Tail = 96,
    TailIf,
    Call,
}

bitflags::bitflags! {
    pub struct InstFlags: u16 {

    }
}
