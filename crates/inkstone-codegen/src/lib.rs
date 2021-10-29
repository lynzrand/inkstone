use inkstone_syn::ast::BlockScope;

pub struct IrGen {}

pub struct ChunkContext {
    pub module: Vec<String>,
}

/// Compile the given chunk
pub fn compile_chunk(chunk: BlockScope, ctx: ChunkContext) {
    let chunk_as_function = FunctionCompileCtx {};
    compile_function_body(chunk, chunk_as_function);
}

struct FunctionCompileCtx {}

fn compile_function_body(body: BlockScope, ctx: FunctionCompileCtx) {}
