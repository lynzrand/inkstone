use clap::Parser as ClapParser;
use inkstone_syn::ast::AstNode;
use inkstone_syn::parse::error_report::ParseError;
use std::io::BufRead;
use std::process::exit;
use text_lines::TextLines;
use unicode_width::UnicodeWidthStr;

fn main() {
    let opt = Opt::parse();

    tracing_subscriber::FmtSubscriber::builder()
        .without_time()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

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
    if !errors.is_empty() {
        let index = TextLines::new(&input);
        for err in errors {
            show_error(&input, &index, &err);
        }
        exit(1);
    }
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

fn show_error(text: &str, index: &TextLines, error: &ParseError) {
    const CONTEXT: usize = 2;

    let span = &error.span;
    let start_ln = index.line_and_column_index(span.start().into());
    let end_ln = index.line_and_column_index(span.end().into());

    let context_begin = start_ln.line_index.saturating_sub(2);
    let context_end = std::cmp::min(end_ln.line_index.saturating_add(2), index.lines_count() - 1);
    let display_lines = (context_begin..context_end).map(|ln| {
        let line_range = index.line_range(ln);
        let line_range = line_range.0..line_range.1;
        let display_range = if ln < start_ln.line_index || ln > end_ln.line_index {
            None
        } else {
            let mut display_range = line_range.clone();
            if ln == start_ln.line_index {
                display_range.start = span.start().into();
            }
            if ln == end_ln.line_index {
                display_range.end = span.end().into();
            }
            Some(display_range)
        };
        (ln, line_range, display_range)
    });

    eprintln!("{:?}: {:?}", error.level, error.kind);

    for (ln, line_range, display_range) in display_lines {
        eprintln!("{:>4} | {}", ln, &text[line_range.clone()]);
        if let Some(range) = display_range {
            let start_diff = line_range.start..range.start;
            let space_width = text[start_diff].width();
            let tilde_width = text[range].width();
            eprintln!("     | {0:1$}{2:~<3$}", " ", space_width, "^", tilde_width);
        }
    }
    eprintln!();
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
