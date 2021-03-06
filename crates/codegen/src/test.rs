use inkstone_syn::ast::{AstNode, Root};
use test_env_log::test;

use crate::ChunkContext;
use expect_test::expect_file;

#[test]
fn test_compile() {
    let mut parser = inkstone_syn::parse::Parser::new(
        r"
let i = 1 + 1
let bar = 1 * i + 4
def foo x y z = x + y + z + i
let baz = \x y -> \z -> x + y + z
i
    ",
    );
    parser.parse();
    let (chunk, errors) = parser.finish();
    assert!(errors.is_empty(), "Parse error: {:?}", errors);
    let chunk = Root::cast(chunk).expect("Failed to cast blockscope");
    let ctx = ChunkContext {
        current_module: vec!["<main>".into()],
    };

    let (f, meta) = crate::compile_chunk(chunk, ctx);

    expect_file!["test_data/test_compile.txt"].assert_debug_eq(&f);
    expect_file!["test_data/test_compile.meta.txt"].assert_debug_eq(&meta);
}
