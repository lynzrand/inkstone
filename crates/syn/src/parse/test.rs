use std::borrow::Borrow;

use expect_test::{expect_file, ExpectFile};
use rowan::SyntaxNode;
use test_env_log::test;

use crate::node::InkstoneLang;

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
    let input = "a.c (1+1) 2 (3 + 4 * 5 ** 6) (foo.bar event)";

    let mut parser = Parser::new(input);
    parser.parse_expr(false).expect("should succeed");

    let (result, err) = parser.finish();
    assert!(err.is_empty());

    assert_tree_matches(result, expect_file!["./test_data/parse_expr.txt"]);
}

#[test]
fn test_parse_function_calls() {
    let input = r"
foo bar + baz qux
foo 1 + 2 * 3
foo (1 + 2)
foo (bar baz) * 3 + 2
    ";

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, err) = parser.finish();
    assert!(err.is_empty());

    assert_tree_matches(result, expect_file!["./test_data/parse_function calls.txt"]);
}

#[test]
fn test_parse_lambda_expr() {
    let input = r#"env :hello "hello world!" \-> print env"#;

    let mut parser = Parser::new(input);
    parser.parse_expr(false).expect("Should succeed");

    let (result, err) = parser.finish();
    assert!(err.is_empty());

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

    let (result, err) = parser.finish();
    assert!(err.is_empty());

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

    let (result, err) = parser.finish();
    assert!(err.is_empty());

    assert_tree_matches(result, expect_file!["./test_data/parse_objects.txt"]);
}

#[test]
fn test_parse_if_while_for() {
    let input = r#"
let x = if foo; do something; else give up; end

if (Date.today ()) == (Date 2021 9 30)
    wake_up me
else if 2 + 2 == 5
    print "It's 1984!"
else
    
end

let y = while true
    for z in w
        if x > 0
            break 12345
        end
    end
    if w == 5
        break
    end
end
"#;

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, e) = parser.finish();
    assert!(e.is_empty());

    assert_tree_matches(result, expect_file!["./test_data/parse_if_while_for.txt"]);
}

#[test]
fn test_parse_weird_exprs() {
    let input = r#"
let foo = \x -> x**2 3 4 5
def pow x y = begin
    
end
    "#;

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, e) = parser.finish();
    assert!(e.is_empty());

    assert_tree_matches(result, expect_file!["./test_data/parse_weird_exprs.txt"]);
}

#[test]
fn test_parse_public_declarations() {
    let input = r#"
pub def new tag val = std.make (tag, val) Result
pub def ok val = new :ok val

pub let foo_constant = 1.2345

pub mod bar
    pub let bar_constant = 114514
end
    "#;

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, e) = parser.finish();
    assert!(e.is_empty(), "Errors: {:?}", e);

    assert_tree_matches(
        result,
        expect_file!["./test_data/parse_public_declarations.txt"],
    );
}
