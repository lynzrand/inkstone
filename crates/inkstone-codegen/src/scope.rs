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
}

impl<'a> Scope<'a> {
    pub fn new(id: u32, scope_type: ScopeType, super_scope: Option<&'a Scope<'a>>) -> Self {
        Scope {
            id,
            ty: scope_type,
            super_scope,
            locals: Vec::new(),
            scope_stack: Vec1::new(LexicalScope::default()),
        }
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

    /// Get the definition of a name, in local scope or super scope.
    ///
    /// Returns `Some((scope_id, offset, entry))` if the name is defined, `None` otherwise.
    pub fn get(&self, name: &str) -> Option<(u32, u32, &ScopeEntry)> {
        self.get_local(name)
            .map(|(offset, entry)| (self.id, offset, entry))
            .or_else(|| self.get_super(name))
    }

    /// Get the definition of a name in local scope.
    pub fn get_local(&self, name: &str) -> Option<(u32, &ScopeEntry)> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(idx) = scope.mapping.get(name) {
                return self.locals.get(*idx as usize).map(|entry| (*idx, entry));
            }
        }
        None
    }

    fn get_super(&self, name: &str) -> Option<(u32, u32, &ScopeEntry)> {
        self.super_scope.and_then(|scope| scope.get(name))
    }
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
