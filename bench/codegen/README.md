# Codegen benchmark: yuku vs `oxc_codegen`

Compares the throughput of yuku's `src/parser/codegen/` against
[`oxc_codegen`](https://crates.io/crates/oxc_codegen) on the shared
parser-benchmark files.

## Running

```sh
bun bench/codegen/run.ts            # all files, 3s/case
bun bench/codegen/run.ts 5          # all files, 5s/case
bun bench/codegen/run.ts react.js   # one file
```

The driver builds both harnesses (`zig build` → ReleaseFast, `cargo build
--release`) and prints a comparison table. The input files live in
`profiler/files/` — run `bun load-files` first if they're missing.

You can also run a single harness directly (emits one JSON line):

```sh
zig build codegen-bench -- profiler/files/react.js 3
./bench/codegen/oxc/target/release/oxc-codegen-bench profiler/files/react.js 3
```

## Methodology

Both harnesses are identical in shape (`yuku.zig` / `oxc/src/main.rs`):

1. Read the file and **parse it once**, outside the timed region.
2. Run codegen in a **self-calibrating loop** for `min_seconds`, timing only
   the codegen call. The reported `best` is the fastest single pass (min),
   `mean` is total/iters.
3. Report ns/pass and MB/s throughput (input bytes / time).

The parse cost is excluded so the numbers reflect **code generation only**.

### Apples-to-apples configuration

Both are configured to emit the **same output**: non-minified (pretty),
2-space indentation, comments disabled.

- yuku: `{ format: .pretty, indent: 2, comments: .none }`
- oxc: `CodegenOptions { comments: CommentOptions::disabled(), indent_char:
  Space, indent_width: 2, ..default }`

This matters because oxc's bare `Codegen::new()` default is **tabs +
comments-on**, which produces substantially different (larger) output and is
not comparable. With the config above, output sizes match within ~3% across
all files (see below). The residual difference is numeric-literal
normalization — yuku preserves the source form (`0xeac7`), oxc expands to
decimal (`60103`) — a legitimate codegen choice, not a correctness gap.

Verify output parity yourself with `--dump`:

```sh
zig build codegen-bench -- profiler/files/react.js 0.1 --dump /tmp/y.js
./bench/codegen/oxc/target/release/oxc-codegen-bench profiler/files/react.js 0.1 --dump /tmp/o.js
diff /tmp/y.js /tmp/o.js
```

### Fairness notes

- The oxc harness uses oxc's **own production release profile** (`opt-level=3`,
  `lto="fat"`, `codegen-units=1`, `panic="abort"`), giving oxc_codegen its
  best-case performance — i.e. a conservative baseline for yuku.
- Both allocate output through system malloc (`std.heap.c_allocator` /
  Rust's global allocator), so the result isolates code generation rather than
  allocator choice.
- Both parse with their own parser into their own AST; only codegen is timed,
  so parser differences don't affect the result.
- `best` (min time) is the headline metric: least affected by OS scheduling
  noise, closest to pure compute.

## Sample results

Apple Silicon (darwin/arm64), oxc_codegen 0.135.0, 3s/case:

| file          | size  | yuku best | oxc best | yuku MB/s | oxc MB/s | speedup   |
| ------------- | ----- | --------- | -------- | --------- | -------- | --------- |
| react.js      | 0.08M | 68.5 µs   | 51.2 µs  | 1144      | 1533     | oxc 1.34x |
| calcom.tsx    | 1.01M | 1.42 ms   | 0.96 ms  | 712       | 1052     | oxc 1.48x |
| typescript.js | 8.32M | 11.67 ms  | 10.60 ms | 713       | 785      | oxc 1.10x |

Numbers are machine-dependent; re-run locally for your hardware.

Both tools keep source-map and comment handling as runtime checks (oxc's
default build enables the `sourcemap` feature and checks per call), so this is
a like-for-like comparison — the codegen is a single compiled variant, not
specialized per option.
