pub mod error;
pub mod func;
mod scope;

use std::cell::RefCell;
use std::rc::Rc;

use fnv::FnvHashMap;
use func::{FunctionCompileCtx, FunctionCompileMetadata};
use inkstone_bytecode::Function;
use inkstone_syn::ast::{AstNode, BlockScope};
use smol_str::SmolStr;

pub struct ChunkContext {
    pub current_module: Vec<SmolStr>,
}

/// Compile the given chunk
pub fn compile_chunk(chunk: BlockScope, ctx: ChunkContext) -> (Function, FunctionCompileMetadata) {
    let builder = Rc::new(RefCell::new(SymbolListBuilder::new()));
    let scope =
        scope::ScopeBuilder::new(chunk.span().start().into(), scope::ScopeType::Module, None);
    let mut ctx = FunctionCompileCtx::new(scope, builder.clone());
    ctx.compile_block_scope(chunk);
    ctx.finish()

    // TODO: create module and stuff
}

/// Type used to build the symbol list
#[derive(Debug, Default)]
pub struct SymbolListBuilder {
    list: Vec<SmolStr>,
    index: FnvHashMap<SmolStr, usize>,
}

impl SymbolListBuilder {
    pub fn new() -> Self {
        SymbolListBuilder {
            list: vec![],
            index: FnvHashMap::default(),
        }
    }

    pub fn intern(&mut self, s: &SmolStr) -> usize {
        if let Some(&idx) = self.index.get(s) {
            idx
        } else {
            let idx = self.list.len();
            self.list.push(s.clone());
            self.index.insert(s.clone(), idx);
            idx
        }
    }
}
