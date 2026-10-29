const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const testing = std.testing;

const source = "\"use client\";\n/a/g;\n<a b=\"&amp;\" c=\"&amp;\">&copy;&#10;<i>old</i></a>;";

fn parse() !ast.Tree {
    var tree = try parser.parse(testing.allocator, source, .{ .lang = .jsx });
    errdefer tree.deinit();
    try testing.expect(!tree.hasErrors());
    return tree;
}

fn expectPrinted(tree: *const ast.Tree, expected: []const u8) !void {
    const printed = try parser.codegen.generate(testing.allocator, tree, .{});
    defer printed.deinit(testing.allocator);
    try testing.expectEqualStrings(expected, printed.code);
}

test "an edited value wins over its stale raw lexeme" {
    var tree = try parse();
    defer tree.deinit();
    const body = tree.extra(tree.data(tree.root).program.body);
    const server = try tree.addString("use server");
    const directive = tree.data(body[0]).directive;
    var literal = tree.data(directive.expression).string_literal;
    literal.value = server;
    tree.setData(directive.expression, .{ .string_literal = literal });
    tree.setData(body[0], .{ .directive = .{
        .expression = directive.expression,
        .value = server,
    } });

    const element = tree.data(tree.data(body[2]).expression_statement.expression).jsx_element;
    const opening = tree.data(element.opening_element).jsx_opening_element;
    const attribute = tree.data(tree.extra(opening.attributes)[0]).jsx_attribute;
    var string = tree.data(attribute.value).string_literal;
    string.value = try tree.addString("x \"y\"");
    tree.setData(attribute.value, .{ .string_literal = string });
    const inner = tree.data(tree.extra(element.children)[1]).jsx_element;
    const text_index = tree.extra(inner.children)[0];
    var text = tree.data(text_index).jsx_text;
    text.value = try tree.addString("a < b { c } & d");
    tree.setData(text_index, .{ .jsx_text = text });

    try expectPrinted(&tree, "\"use server\";\n/a/g;\n" ++
        "<a b=\"x &#34;y&#34;\" c=\"&amp;\">&copy;&#10;" ++
        "<i>a &#60; b &#123; c &#125; &#38; d</i></a>;");
}

test "a node without a raw lexeme prints from its value" {
    var tree = try parse();
    defer tree.deinit();
    for (tree.nodes.items(.data)) |*data| switch (data.*) {
        .string_literal => |*literal| literal.raw = .empty,
        .jsx_text => |*text| text.raw = .empty,
        else => {},
    };

    try expectPrinted(&tree, "\"use client\";\n/a/g;\n" ++
        "<a b=\"&#38;\" c=\"&#38;\">©\n<i>old</i></a>;");
}

test "an empty regular expression pattern prints as an empty group" {
    var tree = try parse();
    defer tree.deinit();
    const body = tree.extra(tree.data(tree.root).program.body);
    const regexp = tree.data(body[1]).expression_statement.expression;
    var literal = tree.data(regexp).regexp_literal;
    literal.pattern = .empty;
    tree.setData(regexp, .{ .regexp_literal = literal });

    try expectPrinted(&tree, "\"use client\";\n/(?:)/g;\n" ++
        "<a b=\"&amp;\" c=\"&amp;\">&copy;&#10;<i>old</i></a>;");
}
