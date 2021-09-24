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
        let diff = colored_diff::PrettyDifference { actual, expected };
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
