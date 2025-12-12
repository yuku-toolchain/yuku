<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/assets/logo.svg" alt="Logo" width="300" />

  [![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
  [![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

A very fast JavaScript/TypeScript parser written in Zig to enable JavaScript tooling in Zig.

</div>

<br/>

<br/>

- Full ECMAScript spec compliance
- Very fast, built with performance in mind from the start, competitive with established parsers like Oxc.
- Robust error recovery, partial ASTs on recoverable errors.
- Rich features, including JSX and TypeScript support.
- Can output 100% ESTree + TypeScript-ESTree compliant AST when needed.

<br/>

We are passing 60.47% of [Test262](https://github.com/tc39/test262) conformance tests so far. We'll get there.

<br/>

```zig
const std = @import("std");
const yuku = @import("yuku");

pub fn main() !void {
    // The allocator passed to the parse function is used as a backing allocator for the internal arena allocator
    const allocator = std.heap.page_allocator;
    const source = "const x = 1 + 2";

    // Parse source code into an AST
    // Options:
    //   .source_type - .module (default) or .script
    //   .lang        - .js (default), .ts, .jsx, .tsx, or .dts
    //   .is_strict   - true (default) or false
    const tree = try yuku.parse(allocator, source, .{});
    defer tree.deinit();

    // ParseTree contains:
    //   .program       - Root node index (always a Program node)
    //   .source        - Original source code
    //   .nodes         - All AST nodes
    //   .extra         - Storage for variadic children (e.g., array elements)
    //   .diagnostics   - Parse errors, warnings, hints encountered

    // Check for parse issues
    if (tree.hasDiagnostics()) {
        for (tree.diagnostics.items) |diagnostic| {
            std.debug.print("{s}: {s} at {d}:{d}\n", .{
                diagnostic.severity.toString(), diagnostic.message, diagnostic.span.start, diagnostic.span.end
            });
            if (diagnostic.help) |help| std.debug.print("  Help: {s}\n", .{help});
        }
    }
}
```
<!-- markdownlint-restore -->
