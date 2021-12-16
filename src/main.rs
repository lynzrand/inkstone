use clap::Parser as ClapParser;
use inkstone_syn::ast::AstNode;
use std::io::BufRead;

fn main() {
    let opt = Opt::parse();

    let input = if let Some(e) = opt.eval {
        e
    } else if let Some(f) = opt.file {
        std::fs::read_to_string(f).expect("Failed to read file")
    } else {
        let mut s = String::new();
        let stdin = std::io::stdin();
        let mut stdin = stdin.lock();
        while stdin.read_line(&mut s).unwrap() != 0 {}
        s
    };

    let mut parser = inkstone_syn::parse::Parser::new(&input);
    parser.parse();
    let (chunk, errors) = parser.finish();
    assert!(errors.is_empty(), "Parse error: {:?}", errors);
    if opt.parse_only {
        println!("{:#?}", chunk);
        return;
    }
    let chunk = inkstone_syn::ast::Root::cast(chunk).expect("Failed to cast blockscope");

    let ctx = inkstone_codegen::ChunkContext {
        current_module: vec!["<main>".into()],
    };

    let (f, meta) = inkstone_codegen::compile_chunk(chunk, ctx);

    if opt.compile_only {
        println!("{:#?}", f);
        return;
    }

    panic!("VM is not yet implemented. Please specify `--parse-only` or `--compile-only` when running this compiler.")
}

#[derive(clap::Parser)]
#[clap(about, version, author)]
struct Opt {
    /// Only parse the input file. Outputs the abstract syntax tree.
    #[clap(long)]
    parse_only: bool,

    /// Only parse and compile the input file. Outputs function bytecode.
    #[clap(long)]
    compile_only: bool,

    /// Evaluate the given string as an Inkstone program.
    #[clap(short, long)]
    eval: Option<String>,

    /// Evaluate the given file as an Inkstone program.
    #[clap(short, long)]
    file: Option<String>,
}
