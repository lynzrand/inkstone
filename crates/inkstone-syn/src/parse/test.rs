use std::borrow::Borrow;

use expect_test::{expect_file, ExpectFile};
use rowan::SyntaxNode;

use crate::node::{InkstoneLang, SynTag};
use crate::parse::tag_util::FormatTree;
use crate::Lexer;

use super::Parser;

fn assert_tree_matches<R: Borrow<SyntaxNode<InkstoneLang>>>(r: R, tree: ExpectFile) {
    tree.assert_debug_eq(r.borrow());
}

#[test]
fn test_parse_empty() {
    let input = " ";

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, _) = parser.finish();

    assert_tree_matches(result, expect_file!["./test_data/parse_empty.txt"]);
}

#[test]
fn test_parse_blocks() {
    let input = r"
begin
    begin
        begin
        end
    end
end
    "
    .trim();

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, _) = parser.finish();

    assert_tree_matches(result, expect_file!["./test_data/parse_blocks.txt"]);
}

#[test]
fn test_parse_expr() {
    let input = "a.c (1+1) 2 (3 + 4 * 5 ** 6) (foo::bar event)";

    let mut parser = Parser::new(input);
    parser.parse_expr(false);

    let (result, _) = parser.finish();

    assert_tree_matches(result, expect_file!["./test_data/parse_expr.txt"]);
}

#[test]
fn test_parse_lambda_expr() {
    let input = r#"env :hello "hello world!" \-> print env"#;

    let mut parser = Parser::new(input);
    parser.parse_expr(false);

    let (result, _) = parser.finish();

    assert_tree_matches(result, expect_file!["./test_data/parse_lambda_expr.txt"]);
}

#[test]
fn test_parse_let_stmts() {
    let input = r#"
let x = 1 + 2;
let y = 3 + 4;

def add a b = a + b
def mul a b = begin
    a * b
end
def id x = \x -> x
    "#;

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, _) = parser.finish();

    assert_tree_matches(result, expect_file!["./test_data/parse_let_stmts.txt"]);
}

#[test]
fn test_parse_objects() {
    let input = r#"
let x = [1, 2+3, 5, 7, foo bar, baz]
let y = {
    foo: \x->x.foo,
    "bar": baz bug,
}
    "#;

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, _) = parser.finish();

    assert_tree_matches(result, expect_file!["./test_data/parse_objects.txt"]);
}
