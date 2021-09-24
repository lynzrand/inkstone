use std::io::stderr;

use rowan::{GreenNodeBuilder, SyntaxNode};

use crate::node::InkstoneLang;
use crate::parse::tag_util::FormatTree;

use super::Parser;

#[test]
fn test_parse_body() {
    let input = " ";

    let mut parser = Parser::new(input);
    parser.parse_root();

    let result = parser.b.finish();
    let result = SyntaxNode::<InkstoneLang>::new_root(result);

    eprintln!("{}", FormatTree(&result));
}
