use std::collections::{BTreeMap, HashMap};

use inkstone_syn::ast::BlockScope;
use smol_str::SmolStr;

pub struct IrGen {}

pub struct ChunkContext {
    pub module: Vec<String>,
}

/// Compile the given chunk
pub fn compile_chunk(chunk: BlockScope, ctx: ChunkContext) {
    let chunk_as_function = FunctionCompileCtx {
        ..Default::default()
    };
    compile_function_body(chunk, &chunk_as_function);
}

/// The context used when building a function
#[derive(Debug, Default)]
struct FunctionCompileCtx<'a> {
    constants: ConstantTableBuilder,
    scope: ScopeMap<'a>,
}

/// Type used to build a constant table
#[derive(Debug, Default)]
struct ConstantTableBuilder {}

/// Additional data of a lexical scope that should own a `Scope` to hold local
/// variables in itself and its children scopes.
#[derive(Debug, Default)]
struct ScopeMap<'a> {
    super_scope: Option<&'a ScopeMap<'a>>,
    mapping: Vec<ScopeEntry>,
}

/// A lexical scope that holds local variables. variables are stored in the
/// closest [`ScopeMap`] above current scope.
#[derive(Debug)]
struct LexicalScope<'a> {
    map: &'a mut ScopeMap<'a>,
    super_scope: Option<&'a LexicalScope<'a>>,
    mapping: HashMap<SmolStr, usize>,
}

#[derive(Debug)]
struct ScopeEntry {}

impl<'a> FunctionCompileCtx<'a> {
    pub fn new(super_scope: Option<&'a ScopeMap<'a>>) -> Self {
        FunctionCompileCtx {
            constants: ConstantTableBuilder::default(),
            scope: ScopeMap {
                super_scope,
                mapping: Default::default(),
            },
        }
    }
}
