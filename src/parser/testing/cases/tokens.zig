//! Token stream collection. Verifies the opt-in `Options.tokens` output:
//! espree classification per tag, source order without overlap, rescan
//! correctness (regex, template continuations, split generics, JSX
//! text), zero cost when disabled, and the transfer format round-trip.

const std = @import("std");
const parser = @import("parser");
const transfer = @import("transfer");

const ast = parser.ast;
const TokenType = parser.TokenType;
const testing = std.testing;

const Expected = struct {
    type: TokenType,
    text: []const u8,
};

/// parses `source` with token collection on and asserts the recorded
/// stream matches `expected` exactly, in both classification and the
/// text each span slices back to. also asserts global stream invariants:
/// source order, no overlap, no zero-length tokens.
fn expectTokens(source: []const u8, options: parser.Options, expected: []const Expected) !void {
    var opts = options;
    opts.tokens = true;
    var tree = try parser.parse(testing.allocator, source, opts);
    defer tree.deinit();

    try testing.expect(!tree.hasErrors());
    try verifyStream(&tree, source);

    const types = tree.tokens.items(.type);
    const starts = tree.tokens.items(.start);
    const ends = tree.tokens.items(.end);
    try testing.expectEqual(expected.len, tree.tokens.len);
    for (expected, 0..) |e, i| {
        try testing.expectEqual(e.type, types[i]);
        try testing.expectEqualStrings(e.text, source[starts[i]..ends[i]]);
    }
}

fn verifyStream(tree: *const ast.Tree, source: []const u8) !void {
    try testing.expect(tree.collected_tokens);
    const starts = tree.tokens.items(.start);
    const ends = tree.tokens.items(.end);
    var prev_end: u32 = 0;
    for (starts, ends) |start, end| {
        try testing.expect(start < end);
        try testing.expect(start >= prev_end);
        try testing.expect(end <= source.len);
        prev_end = end;
    }
}

test "tokens are off by default and cost nothing" {
    var tree = try parser.parse(testing.allocator, "const x = 1;", .{});
    defer tree.deinit();
    try testing.expect(!tree.collected_tokens);
    try testing.expectEqual(0, tree.tokens.len);
}

test "empty streams still mark collection" {
    var tree = try parser.parse(testing.allocator, "// just a comment", .{ .tokens = true });
    defer tree.deinit();
    try testing.expect(tree.collected_tokens);
    try testing.expectEqual(0, tree.tokens.len);
    try testing.expectEqual(1, tree.comments.len);
}

test "espree classification over the basic categories" {
    try expectTokens(
        "const x = obj.true1 ?? 0xFF; class C { #p = null; m() { return true; } }",
        .{},
        &.{
            .{ .type = .keyword, .text = "const" },
            .{ .type = .identifier, .text = "x" },
            .{ .type = .punctuator, .text = "=" },
            .{ .type = .identifier, .text = "obj" },
            .{ .type = .punctuator, .text = "." },
            .{ .type = .identifier, .text = "true1" },
            .{ .type = .punctuator, .text = "??" },
            .{ .type = .numeric, .text = "0xFF" },
            .{ .type = .punctuator, .text = ";" },
            .{ .type = .keyword, .text = "class" },
            .{ .type = .identifier, .text = "C" },
            .{ .type = .punctuator, .text = "{" },
            .{ .type = .private_identifier, .text = "#p" },
            .{ .type = .punctuator, .text = "=" },
            .{ .type = .null, .text = "null" },
            .{ .type = .punctuator, .text = ";" },
            .{ .type = .identifier, .text = "m" },
            .{ .type = .punctuator, .text = "(" },
            .{ .type = .punctuator, .text = ")" },
            .{ .type = .punctuator, .text = "{" },
            .{ .type = .keyword, .text = "return" },
            .{ .type = .boolean, .text = "true" },
            .{ .type = .punctuator, .text = ";" },
            .{ .type = .punctuator, .text = "}" },
            .{ .type = .punctuator, .text = "}" },
        },
    );
}

test "regex rescan replaces the speculative slash token" {
    try expectTokens("a / b; c = /d/g;", .{}, &.{
        .{ .type = .identifier, .text = "a" },
        .{ .type = .punctuator, .text = "/" },
        .{ .type = .identifier, .text = "b" },
        .{ .type = .punctuator, .text = ";" },
        .{ .type = .identifier, .text = "c" },
        .{ .type = .punctuator, .text = "=" },
        .{ .type = .regular_expression, .text = "/d/g" },
        .{ .type = .punctuator, .text = ";" },
    });
}

test "template continuations replace the closing brace" {
    try expectTokens("`a${b}c${d}e`;", .{}, &.{
        .{ .type = .template, .text = "`a${" },
        .{ .type = .identifier, .text = "b" },
        .{ .type = .template, .text = "}c${" },
        .{ .type = .identifier, .text = "d" },
        .{ .type = .template, .text = "}e`" },
        .{ .type = .punctuator, .text = ";" },
    });
}

test "await is a keyword in modules and an identifier in scripts" {
    try expectTokens("await 0;", .{ .source_type = .module }, &.{
        .{ .type = .keyword, .text = "await" },
        .{ .type = .numeric, .text = "0" },
        .{ .type = .punctuator, .text = ";" },
    });
    try expectTokens("var await;", .{ .source_type = .script }, &.{
        .{ .type = .keyword, .text = "var" },
        .{ .type = .identifier, .text = "await" },
        .{ .type = .punctuator, .text = ";" },
    });
}

test "contextual keywords classify as identifiers" {
    try expectTokens("type A = string; declare const of: any;", .{ .lang = .ts }, &.{
        .{ .type = .identifier, .text = "type" },
        .{ .type = .identifier, .text = "A" },
        .{ .type = .punctuator, .text = "=" },
        .{ .type = .identifier, .text = "string" },
        .{ .type = .punctuator, .text = ";" },
        .{ .type = .identifier, .text = "declare" },
        .{ .type = .keyword, .text = "const" },
        .{ .type = .identifier, .text = "of" },
        .{ .type = .punctuator, .text = ":" },
        .{ .type = .identifier, .text = "any" },
        .{ .type = .punctuator, .text = ";" },
    });
}

test "nested generics split the fused shift token" {
    try expectTokens("let m: Map<string, Map<string, number>> = new Map();", .{ .lang = .ts }, &.{
        .{ .type = .keyword, .text = "let" },
        .{ .type = .identifier, .text = "m" },
        .{ .type = .punctuator, .text = ":" },
        .{ .type = .identifier, .text = "Map" },
        .{ .type = .punctuator, .text = "<" },
        .{ .type = .identifier, .text = "string" },
        .{ .type = .punctuator, .text = "," },
        .{ .type = .identifier, .text = "Map" },
        .{ .type = .punctuator, .text = "<" },
        .{ .type = .identifier, .text = "string" },
        .{ .type = .punctuator, .text = "," },
        .{ .type = .identifier, .text = "number" },
        .{ .type = .punctuator, .text = ">" },
        .{ .type = .punctuator, .text = ">" },
        .{ .type = .punctuator, .text = "=" },
        .{ .type = .keyword, .text = "new" },
        .{ .type = .identifier, .text = "Map" },
        .{ .type = .punctuator, .text = "(" },
        .{ .type = .punctuator, .text = ")" },
        .{ .type = .punctuator, .text = ";" },
    });
}

test "jsx text and identifiers" {
    try expectTokens("let e = <a-b href={x}>hi</a-b>;", .{ .lang = .jsx }, &.{
        .{ .type = .keyword, .text = "let" },
        .{ .type = .identifier, .text = "e" },
        .{ .type = .punctuator, .text = "=" },
        .{ .type = .punctuator, .text = "<" },
        .{ .type = .jsx_identifier, .text = "a-b" },
        .{ .type = .jsx_identifier, .text = "href" },
        .{ .type = .punctuator, .text = "=" },
        .{ .type = .punctuator, .text = "{" },
        .{ .type = .identifier, .text = "x" },
        .{ .type = .punctuator, .text = "}" },
        .{ .type = .punctuator, .text = ">" },
        .{ .type = .jsx_text, .text = "hi" },
        .{ .type = .punctuator, .text = "<" },
        .{ .type = .punctuator, .text = "/" },
        .{ .type = .jsx_identifier, .text = "a-b" },
        .{ .type = .punctuator, .text = ">" },
        .{ .type = .punctuator, .text = ";" },
    });
}

test "jsx fragments record both angle brackets" {
    try expectTokens("let f = <>x{y}</>;", .{ .lang = .jsx }, &.{
        .{ .type = .keyword, .text = "let" },
        .{ .type = .identifier, .text = "f" },
        .{ .type = .punctuator, .text = "=" },
        .{ .type = .punctuator, .text = "<" },
        .{ .type = .punctuator, .text = ">" },
        .{ .type = .jsx_text, .text = "x" },
        .{ .type = .punctuator, .text = "{" },
        .{ .type = .identifier, .text = "y" },
        .{ .type = .punctuator, .text = "}" },
        .{ .type = .punctuator, .text = "<" },
        .{ .type = .punctuator, .text = "/" },
        .{ .type = .punctuator, .text = ">" },
        .{ .type = .punctuator, .text = ";" },
    });
}

test "speculative arrow parses leave no duplicate tokens" {
    // `(a, b)` is first parsed as a parenthesized expression or arrow
    // head speculatively, forcing checkpoint/rewind. the recorded stream
    // must contain each token exactly once.
    try expectTokens("f = (a, b) => a + b;", .{}, &.{
        .{ .type = .identifier, .text = "f" },
        .{ .type = .punctuator, .text = "=" },
        .{ .type = .punctuator, .text = "(" },
        .{ .type = .identifier, .text = "a" },
        .{ .type = .punctuator, .text = "," },
        .{ .type = .identifier, .text = "b" },
        .{ .type = .punctuator, .text = ")" },
        .{ .type = .punctuator, .text = "=>" },
        .{ .type = .identifier, .text = "a" },
        .{ .type = .punctuator, .text = "+" },
        .{ .type = .identifier, .text = "b" },
        .{ .type = .punctuator, .text = ";" },
    });
}

test "streams survive sources with syntax errors" {
    // recovery skips tokens without recording, but everything recorded
    // must still satisfy the stream invariants.
    const source = "const = 1; const y = 2;";
    var tree = try parser.parse(testing.allocator, source, .{ .tokens = true });
    defer tree.deinit();
    try testing.expect(tree.hasErrors());
    try verifyStream(&tree, source);
    try testing.expect(tree.tokens.len > 0);
}

test "transfer round-trip preserves the token stream" {
    const source = "const x = `a${y}b`;";
    var tree = try parser.parse(testing.allocator, source, .{ .tokens = true });
    defer tree.deinit();
    try testing.expect(tree.tokens.len > 0);

    const buf = try testing.allocator.alignedAlloc(u8, .of(u32), transfer.bufferSize(&tree));
    defer testing.allocator.free(buf);
    _ = transfer.serializeInto(&tree, buf);

    var back = try transfer.deserializeFromBuf(testing.allocator, buf, source);
    defer back.deinit();

    try testing.expect(back.collected_tokens);
    try testing.expectEqual(tree.tokens.len, back.tokens.len);
    try testing.expectEqualSlices(TokenType, tree.tokens.items(.type), back.tokens.items(.type));
    try testing.expectEqualSlices(u32, tree.tokens.items(.start), back.tokens.items(.start));
    try testing.expectEqualSlices(u32, tree.tokens.items(.end), back.tokens.items(.end));
}

test "transfer round-trip keeps the empty stream flag" {
    var tree = try parser.parse(testing.allocator, "// c", .{ .tokens = true });
    defer tree.deinit();
    try testing.expectEqual(0, tree.tokens.len);

    const buf = try testing.allocator.alignedAlloc(u8, .of(u32), transfer.bufferSize(&tree));
    defer testing.allocator.free(buf);
    _ = transfer.serializeInto(&tree, buf);

    var back = try transfer.deserializeFromBuf(testing.allocator, buf, "// c");
    defer back.deinit();
    try testing.expect(back.collected_tokens);
    try testing.expectEqual(0, back.tokens.len);
}
