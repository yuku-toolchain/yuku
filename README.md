<!-- markdownlint-disable first-line-h1 -->

<!-- markdownlint-start-capture -->
<!-- markdownlint-disable-file no-inline-html -->
<div align="center">

  <!-- markdownlint-disable-next-line no-alt-text -->
  <img src="/assets/logo.svg" alt="Logo" width="300" />

  [![NPM Version](https://img.shields.io/npm/v/yuku?logo=npm&logoColor=212121&label=version&labelColor=ffc44e&color=212121)](https://npmjs.com/package/yuku)
  [![sponsor](https://img.shields.io/badge/sponsor-EA4AAA?logo=githubsponsors&labelColor=FAFAFA)](https://github.com/sponsors/arshad-yaseen)

Very fast parser for the web, written in Zig.

<br/>

<br/>

- [x] Javascript Lexer
- [ ] Javascript Parser
- [ ] TypeScript Support
- [ ] JSX Support

</div>

```zig
const std = @import("std");
const yuku = @import("yuku");

pub fn main() !void {
    // Backing allocator for the internal Arena allocator
    const allocator = std.heap.page_allocator;
    const source = "const x = 1 + 2";

    // Initialize the parser
    // Options:
    //   .source_type - .Module (default) or .Script
    //   .lang        - .Js (default), .Ts, .Jsx, .Tsx, or .Dts
    //   .is_strict   - true (default) or false
    var parser = yuku.Parser.init(allocator, source, .{
        .source_type = .Module,
        .lang = .Js,
        .is_strict = true,
    });

    // Parse the source code and get the ParseTree
    // The parser is consumed after this call
    const tree = try parser.parse();
    defer tree.deinit(); // Free all arena-allocated memory

    // ParseTree contains:
    //   .program  - Root node index (always a Program node)
    //   .source   - Original source code
    //   .nodes    - All AST nodes
    //   .extra    - Storage for variadic children (e.g., array elements)
    //   .errors   - Parse errors encountered

    // Check for parse errors
    if (tree.hasErrors()) {
        for (tree.errors.items) |err| {
            std.debug.print("Error: {s} at {d}:{d}\n", .{
                err.message, err.span.start, err.span.end
            });
            if (err.help) |help| std.debug.print("  Help: {s}\n", .{help});
        }
        return;
    }

    // Coming Soon Built-in AST traverser, visitor pattern, and
    // more utilities to work with the AST efficiently.
}
```
<!-- markdownlint-restore -->
