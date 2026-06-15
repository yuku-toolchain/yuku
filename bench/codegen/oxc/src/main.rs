//! Standalone codegen benchmark for `oxc_codegen`.
//!
//! Mirrors ../yuku.zig exactly: parse a file once, then run codegen in a
//! self-calibrating loop, timing the codegen step only (parse excluded).
//! Output: one JSON object on stdout, same schema as the yuku harness.
//!
//! Usage: oxc-codegen-bench <file> [min_seconds]

use std::path::Path;
use std::time::Instant;

use oxc_allocator::Allocator;
use oxc_codegen::{Codegen, CodegenOptions, CommentOptions, IndentChar};
use oxc_parser::Parser;
use oxc_span::SourceType;

fn main() {
    let raw: Vec<String> = std::env::args().skip(1).collect();
    let mut path: Option<String> = None;
    let mut min_seconds: f64 = 3.0;
    let mut dump_path: Option<String> = None;
    let mut it = raw.into_iter();
    while let Some(arg) = it.next() {
        if arg == "--dump" {
            dump_path = it.next();
        } else if path.is_none() {
            path = Some(arg);
        } else {
            min_seconds = arg.parse().expect("min_seconds must be a number");
        }
    }
    let path = path.unwrap_or_else(|| {
        eprintln!("usage: oxc-codegen-bench <file> [min_seconds] [--dump <out>]");
        std::process::exit(2);
    });
    let path = &path;

    let source = std::fs::read_to_string(path).expect("failed to read file");
    let source_type = SourceType::from_path(path).expect("unknown source type");

    // Parse once, outside the measured loop. Keep the program alive in the
    // arena for the whole run; codegen borrows it.
    let allocator = Allocator::default();
    let ret = Parser::new(&allocator, &source, source_type).parse();
    let program = ret.program;

    // Match yuku's `{ format: .pretty, indent: 2, comments: .none }`:
    // non-minified, 2-space indent, comments disabled. (oxc's own default
    // is tabs + comments-on, which would emit different output.)
    let options = CodegenOptions {
        comments: CommentOptions::disabled(),
        indent_char: IndentChar::Space,
        indent_width: 2,
        ..CodegenOptions::default()
    };
    let codegen = || Codegen::new().with_options(options.clone()).build(&program).code;

    // Warm up and capture output size.
    let warm = codegen();
    let output_bytes = warm.len();
    // Optional: dump generated code for output-parity verification.
    if let Some(dp) = &dump_path {
        std::fs::write(dp, &warm).expect("failed to write dump");
    }
    drop(warm);

    let min = std::time::Duration::from_secs_f64(min_seconds);
    let mut best_ns: u128 = u128::MAX;
    let mut iters: u64 = 0;
    let loop_start = Instant::now();
    while loop_start.elapsed() < min {
        let t0 = Instant::now();
        let code = codegen();
        let elapsed = t0.elapsed().as_nanos();
        std::hint::black_box(&code);
        drop(code);
        if elapsed < best_ns {
            best_ns = elapsed;
        }
        iters += 1;
    }
    let total_ns = loop_start.elapsed().as_nanos();
    let mean_ns = total_ns / iters as u128;

    let bytes_f = source.len() as f64;
    let mean_mb_s = bytes_f / (mean_ns as f64 / 1e9) / (1024.0 * 1024.0);
    let best_mb_s = bytes_f / (best_ns as f64 / 1e9) / (1024.0 * 1024.0);

    let file = Path::new(path)
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or(path);

    println!(
        "{{\"tool\":\"oxc\",\"file\":\"{}\",\"input_bytes\":{},\"output_bytes\":{},\"iters\":{},\"mean_ns\":{},\"best_ns\":{},\"mean_mb_s\":{:.2},\"best_mb_s\":{:.2}}}",
        file, source.len(), output_bytes, iters, mean_ns, best_ns, mean_mb_s, best_mb_s
    );
}
