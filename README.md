<!-- markdownlint-disable first-line-h1 -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/docs/public/logo.svg" alt="Logo" width="280" />

  <br><br>

  [![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
  [![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

  A JavaScript/TypeScript parser and toolchain written in Zig.<br>
  Spec-compliant. Zero dependencies. Fast by design.

</div>

<br>

![Benchmark: Parsing TypeScript source to JavaScript](https://raw.githubusercontent.com/yuku-toolchain/parser-benchmark/refs/heads/main/charts/typescript.png)

<div align="center"><sub>Parsing the bundled TypeScript compiler source (7.8 MB) · macOS (ARM) · Apple M4 Pro · <a href="https://github.com/yuku-toolchain/parser-benchmark">source</a></sub></div>

<br>

```zig
const std = @import("std");
const parser = @import("parser");

const traverser = parser.traverser.scoped;

pub fn main() !void {
    var tree = try parser.parse(std.heap.page_allocator, source, .{ .lang = .tsx });
    defer tree.deinit();

    // walk the AST
    var visitor = MyVisitor{};
    try traverser.traverse(MyVisitor, &tree, &visitor);
}
```

## Documentation

Full docs, guides, and API reference at **[yuku.fyi](https://yuku.fyi)**.

## Contributing

Yuku is pure Zig with no external dependencies.

```bash
git clone https://github.com/yuku-toolchain/yuku.git
cd yuku
zig build
```

Run the full test suite (45,000+ Test262 files with AST matching):

```bash
bun run test
```

## Roadmap

- [x] JavaScript Parser
- [x] WASM
- [x] JSX Support
- [x] Visitor/Traverser
- [ ] TypeScript Support _(in progress)_
- [ ] Module Resolver
