use std::borrow::Borrow;

use rowan::SyntaxNode;

use crate::node::{InkstoneLang, SynTag};
use crate::parse::tag_util::FormatTree;
use crate::Lexer;

use super::Parser;

fn assert_tree_matches<R: Borrow<SyntaxNode<InkstoneLang>>>(r: R, tree: &str) {
    let result_formatted = format!("{}", FormatTree(r.borrow()));
    let actual = result_formatted.trim();
    let expected = tree.trim();
    if actual != expected {
        let diff = colored_diff::PrettyDifference { expected, actual };
        panic!("tree mismatch (< expected / > actual):\n{}", diff);
    }
}

#[test]
fn test_parse_empty() {
    let input = " ";

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, _) = parser.finish();

    assert_tree_matches(
        result,
        r#"
Root
    WS " "
    BlockScope
        "#,
    );
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

    {
        use SynTag::*;
        let lexer = Lexer::new(input);
        let res = lexer.collect::<Vec<_>>();
        pretty_assertions::assert_eq!(
            res,
            vec![
                BeginKw, LF, WS, BeginKw, LF, WS, BeginKw, LF, WS, EndKw, LF, WS, EndKw, LF, EndKw
            ]
        );
    }

    let mut parser = Parser::new(input);
    parser.parse_root();

    let (result, _) = parser.finish();

    assert_tree_matches(
        result,
        r#"
Root
    BlockScope
        Block
            BeginKw "begin"
            LF "\n"
            WS "    "
            BlockScope
                Block
                    BeginKw "begin"
                    LF "\n"
                    WS "        "
                    BlockScope
                        Block
                            BeginKw "begin"
                            LF "\n"
                            WS "        "
                            BlockScope
                            EndKw "end"
                        LF "\n"
                        WS "    "
                    EndKw "end"
                LF "\n"
            EndKw "end"
        "#,
    );
}

#[test]
fn test_parse_expr() {
    let input = "a.c (1+1) 2 (3 + 4 * 5 ** 6) (foo::bar event)";

    let mut parser = Parser::new(input);
    parser.parse_expr(false);

    let (result, _) = parser.finish();

    eprintln!("{}", FormatTree(&result))
}

#[test]
fn test_parse_lambda_expr() {
    let input = r#"env :hello "hello world!" \-> print env"#;

    let mut parser = Parser::new(input);
    parser.parse_expr(false);

    let (result, _) = parser.finish();

    eprintln!("{}", FormatTree(&result))
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

    eprintln!("{}", FormatTree(&result))
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

    eprintln!("{}", FormatTree(&result))
}
