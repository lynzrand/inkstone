pub mod func;
mod scope;

use func::FunctionCompileCtx;
use inkstone_syn::ast::BlockScope;

pub struct ChunkContext {
    pub module: Vec<String>,
}

/// Compile the given chunk
pub fn compile_chunk(chunk: BlockScope, ctx: ChunkContext) {
    let ctx = FunctionCompileCtx::in_module_scope(&chunk);
}
