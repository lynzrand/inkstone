mod param;

use bytes::BufMut;
use enum_ordinalize::Ordinalize;

pub use self::param::IParamType;
pub use param::ParamType;

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
            $(>> $push_cnt:literal)?
            $(<< $pop_cnt:literal)?
        ),*
    ) => {
        $(#[$meta])*
        #[derive(Clone, Debug, PartialEq, Copy, Ordinalize)]
        #[repr(u8)]
        $ty_vis enum $type {$(
            $(#[$variant_meta])*
            $name $(= $n)?
        ),*}

        impl $type {
            /// Returns the count of parameters of this instruction
            pub fn param_type(self) -> Option<ParamType> {
                #[allow(path_statements)]
                match self {$(
                    $type::$name => {
                        None::<ParamType>
                        $(; Some(ParamType::$param))?
                    }
                ),*
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

pub fn write_inst(mut w: impl BufMut, inst: Inst, param: impl IParamType) {
    w.put_u8(inst as u8);
    param.write(w)
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
    Rem                          >> 2     << 1,
    Pow                          >> 2     << 1,

    BitAnd                       >> 2     << 1,
    BitOr                        >> 2     << 1,
    BitXor                       >> 2     << 1,
    Shl                          >> 2     << 1,
    Shr                          >> 2     << 1,
    ShrL                         >> 2     << 1,

    And                          >> 2     << 1,
    Or                           >> 2     << 1,
    Not                          >> 1     << 1,

    Lt                           >> 2     << 1,
    Gt                           >> 2     << 1,
    Le                           >> 2     << 1,
    Ge                           >> 2     << 1,
    Eq                           >> 2     << 1,
    Ne                           >> 2     << 1,

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
    /// Yield the current task to its poller with the given value. Returns the param
    /// the waker provides, or `nil` if no value was provided.
    ///
    /// A yielded task must be waken from an external source to be polled again,
    /// either via `PollTask`, or via an exposed API of the VM.
    Yield                        >> 1     << 1,
    /// Create a new task using the given closure.
    NewTask                      >> 1     << 1,
    /// Suspend the current task and wakes the given task with the given parameter.
    ///
    /// - Returns `(:pending, yield_value)` if the task yields
    /// - Returns `(:completed, return_value)` if the task completes
    /// - Returns `(:panicked, error)` if the task panicked
    ///
    /// Polling on a detached task does not suspend the current task, but only
    /// schedules it to run after the current task. The instruction immediately
    /// returns with `nil` yield values or the corresponding result if the task
    /// has already completed.
    PollTask                     >> 2     << 1,
    /// Detach the given task, allowing it to be polled to complete by the
    /// scheduler. The task will be polled with `nil` every time it's waken.
    DetachTask                   >> 1,

    /// Panic! Wipe out the current task and create a backtrace.
    ///
    /// Panics cannot be caught. In fact, **no failing error** can be caught in
    /// Inkstone, for simplicity's sake.
    Panic                        >> 1,

    /// Do nothing.
    Nop
}
