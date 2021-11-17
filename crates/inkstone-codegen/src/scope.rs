use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};

use fnv::FnvBuildHasher;
use inkstone_syn::ast::{BlockScope, Expr, FuncDef};
use smol_str::SmolStr;
use vec1::Vec1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeType {
    /// The top-level scope inside a module
    Module,
    /// The top-level scope inside a function
    Function,
}

/// Additional data of a lexical scope that should own a `Scope` to hold local
/// variables in itself and its children scopes.
#[derive(Debug)]
pub struct Scope<'a> {
    /// A unique scope ID inside this compilation unit
    id: u32,
    /// Type of this scope
    ty: ScopeType,
    super_scope: Option<&'a Scope<'a>>,
    locals: Vec<ScopeEntry>,
    /// List of lexical scopes tied to this scope. The top-level lexical scope will be used
    /// to determine the relation between exported item names and their offsets after the
    /// compilation completes.
    scope_stack: Vec1<LexicalScope>,
    /// Upvalue in this scope and upvalues in super scopes
    upvalue_capture: RefCell<UpValueCapture>,
}

#[derive(Debug, Default)]
pub struct UpValueCapture {
    /// Mapping from **super scope slots** to local scope slot and upvalue kind.
    captures: BTreeMap<ScopeVariable, u32>,
    upvalue_cnt: u32,
}

impl UpValueCapture {
    pub fn get(&self, super_slot: &ScopeVariable) -> Option<u32> {
        self.captures.get(super_slot).copied()
    }

    pub fn insert(&mut self, super_slot: ScopeVariable) -> u32 {
        let slot = self.upvalue_cnt;
        self.upvalue_cnt += 1;
        self.captures.insert(super_slot, slot);
        slot
    }
}

/// The kind of upvalue capture
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UpValueKind {
    /// This value should be captured into an upvalue, and detached when going out of scope.
    Capture,
    /// This value is already an upvalue, and should be directly copied.
    Copy,
}

impl<'a> Scope<'a> {
    pub fn new(id: u32, scope_type: ScopeType, super_scope: Option<&'a Scope<'a>>) -> Self {
        Scope {
            id,
            ty: scope_type,
            super_scope,
            locals: Vec::new(),
            scope_stack: Vec1::new(LexicalScope::default()),
            upvalue_capture: RefCell::new(UpValueCapture::default()),
        }
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn new_scope(&mut self) {
        self.scope_stack.push(LexicalScope::default());
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack
            .pop()
            .expect("This ScopeMap pops its last scope. what happened?");
    }

    pub fn insert(&mut self, name: SmolStr, entry: ScopeEntry) -> u32 {
        let id = self.locals.len();
        assert!(
            id <= u32::MAX as usize,
            "Cannot hold more than 2^32 local variables"
        );

        let id = id as u32;
        self.locals.push(entry);

        let last = self.scope_stack.last_mut();
        // NOTE: we allow rebinding of names here.
        last.mapping.insert(name, id);

        id
    }

    pub fn get_slot(&self, slot: u32) -> Option<&ScopeEntry> {
        self.locals.get(slot as usize)
    }

    /// Get the definition of a name, in local scope or super scope.
    ///
    /// Returns `Some((scope_id, offset, entry))` if the name is defined, `None` otherwise.
    pub fn get(&self, name: &str) -> Option<(u32, u32)> {
        self.get_local(name)
            .map(|offset| (self.id, offset))
            .or_else(|| self.get_super(name))
    }

    /// Get the definition of a name in local scope.
    pub fn get_local(&self, name: &str) -> Option<u32> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(idx) = scope.mapping.get(name) {
                return Some(*idx);
            }
        }
        None
    }

    fn get_super(&self, name: &str) -> Option<(u32, u32)> {
        self.super_scope.and_then(|scope| scope.get(name))
    }

    /// Search for the given name in local scope. If found, return `Some((slot, false))`.
    /// If not, search for the given name in super scopes and recursively add it into the upvalue
    /// stack.
    pub fn get_local_or_add_upvalue(&self, name: &str) -> Option<ScopeVariable> {
        if let Some(slot) = self.get_local(name) {
            // This is a simple local value.
            return Some(ScopeVariable::Local(slot));
        }

        if let Some(slot) = self
            .super_scope
            .and_then(|scope| scope.get_local_or_add_upvalue(name))
        {
            if self
                .super_scope
                .map(|s| s.super_scope.is_none())
                .unwrap_or_default()
            {
                // super scope is module scope
                return Some(ScopeVariable::Module);
            }

            let mut capture = self.upvalue_capture.borrow_mut();
            if let Some(slot) = capture.get(&slot) {
                return Some(ScopeVariable::Upvalue(slot));
            }
            let local_upvalue_slot = capture.insert(slot);
            Some(ScopeVariable::Upvalue(local_upvalue_slot))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScopeVariable {
    /// This value is a local variable at the given slot
    Local(u32),
    /// This value is an upvalue at the given upvalue slot
    Upvalue(u32),
    /// This value is the specified named object in module scope
    Module,
}

/// A lexical scope that holds local variables. variables are stored in the
/// closest [`ScopeMap`] above current scope.
#[derive(Debug, Default)]
pub struct LexicalScope {
    mapping: HashMap<SmolStr, u32, fnv::FnvBuildHasher>,
}

#[derive(Debug)]
pub enum ScopeEntryKind {
    Variable,
    Function,
    Module,
}

#[derive(Debug)]
pub struct ScopeEntry {
    pub kind: ScopeEntryKind,
    pub is_public: bool,
}
