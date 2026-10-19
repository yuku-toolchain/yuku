const std = @import("std");
const parser = @import("parser");

const ast = parser.ast;
const dialect = parser.dialect;
const Claim = dialect.Claim;
const Error = dialect.Error;
const NodeIndex = ast.NodeIndex;
const Parser = dialect.Parser;

pub const files: []const dialect.File = &.{.{ .extension = ".jsx", .lang = .jsx }};

pub const Node = enum {
    each,
    assertion,
    topic,
    pipeline,
    expression_body,
    interpolation,
    marked,
    shorthand,
    rescue,

    pub fn shape(node: Node) dialect.Shape {
        return switch (node) {
            .each => .{
                .type = "EachStatement",
                .fields = &.{ "item", "list", "body[]" },
                .block_scope = true,
                .lexical_bindings = true,
                .iteration = true,
            },
            .assertion => .{ .type = "AssertStatement", .fields = &.{"argument"} },
            .topic => .{ .type = "TopicReference" },
            .pipeline => .{
                .type = "PipelineExpression",
                .fields = &.{ "left", "right" },
                .properties = "{\"operator\":\"|>\"}",
            },
            .expression_body => .{ .type = "ExpressionBody", .fields = &.{"expression"} },
            .interpolation => .{ .type = "JSXInterpolation", .fields = &.{"expression"} },
            .marked => .{ .type = "JSXMarkedElement", .fields = &.{"element"} },
            .shorthand => .{ .type = "JSXShorthandAttribute", .fields = &.{"value"} },
            .rescue => .{
                .type = "RescueStatement",
                .fields = &.{ "param", "body[]" },
                .block_scope = true,
                .catch_bindings = true,
            },
        };
    }
};

pub fn statement(p: *Parser) Error!Claim {
    const token = p.token();
    if (!token.tag.isIdentifierLike()) return .decline;
    const name = token.text(p.source());
    const next = p.tokenAt(token.span.end);
    if (std.mem.eql(u8, name, "each") and next.tag == .left_paren) return parseEach(p);
    if (std.mem.eql(u8, name, "rescue") and next.tag == .left_paren) return parseRescue(p);
    if (std.mem.eql(u8, name, "assert") and next.tag.isIdentifierLike()) return parseAssertion(p);
    return .decline;
}

pub fn expression(p: *Parser) Error!Claim {
    const token = p.token();
    if (token.tag == .at and std.mem.startsWith(u8, p.source()[token.span.end..], "<")) {
        return parseMarked(p);
    }
    if (token.tag != .percent) return .decline;
    if (!try p.advance()) return .failed;
    return claimed(try p.addNode(.topic, &.{}, token.span));
}

pub fn operator(p: *Parser, left: NodeIndex, precedence: u8) Error!Claim {
    const token = p.token();
    // a `(` that starts a line starts a statement
    if (token.tag == .left_paren and token.hasLineTerminatorBefore()) return .end;
    if (!std.mem.startsWith(u8, p.source()[token.span.start..], "|>")) return .decline;
    if (precedence > ast.Precedence.Assignment) return .end;

    if (!try p.advance() or !try p.advance()) return .failed;
    const right = try p.parseExpression(ast.Precedence.Assignment + 1) orelse return .failed;
    const span: ast.Span = .{ .start = p.tree().span(left).start, .end = p.tree().span(right).end };
    return claimed(try p.addNode(.pipeline, &.{ left, right }, span));
}

pub fn function_body(p: *Parser) Error!Claim {
    const start = p.token().span.start;
    if (p.token().tag != .assign) return .decline;

    if (!try p.advance()) return .failed;
    const value = try p.parseExpression(ast.Precedence.Assignment) orelse return .failed;
    const span: ast.Span = .{ .start = start, .end = p.tree().span(value).end };
    return claimed(try p.addNode(.expression_body, &.{value}, span));
}

pub fn jsx_child(p: *Parser, offset: u32, parent: NodeIndex) Error!Claim {
    const source = p.source();
    if (offset == p.tree().span(parent).end and isRaw(p.tree(), parent)) {
        const end: u32 = @intCast(std.mem.findPos(u8, source, offset, "</raw>") orelse source.len);
        if (end == offset) return .decline;
        const text = p.tree().sourceSlice(offset, end);
        const raw: ast.NodeData = .{ .jsx_text = .{ .raw = text } };
        return claimed(try p.tree().addNode(raw, .{ .start = offset, .end = end }));
    }

    if (std.mem.startsWith(u8, source[offset..], "@(")) {
        if (!try p.seek(offset + 1) or !try p.advance()) return .failed;
        const value = try p.parseExpression(ast.Precedence.Lowest) orelse return .failed;
        if (p.token().tag != .right_paren) {
            try p.report(p.token().span, "Expected ')' to close '@('");
            return .failed;
        }
        const span: ast.Span = .{ .start = offset, .end = p.token().span.end };
        return claimed(try p.addNode(.interpolation, &.{value}, span));
    }

    if (std.mem.startsWith(u8, source[offset..], "@<")) {
        if (!try p.seek(offset + 1)) return .failed;
        const element = try p.parseJsxElement() orelse return .failed;
        const span: ast.Span = .{ .start = offset, .end = p.tree().span(element).end };
        return claimed(try p.addNode(.marked, &.{element}, span));
    }

    if (std.mem.startsWith(u8, source[offset..], "/*")) {
        const close = std.mem.findPos(u8, source, offset, "*/") orelse return .decline;
        const span: ast.Span = .{ .start = offset, .end = @intCast(close + 2) };
        try p.addComment(.block, span);
        const empty = try p.tree().addNode(.{ .jsx_empty_expression = .{} }, span);
        return claimed(try p.tree().addNode(.{ .jsx_expression_container = .{
            .expression = empty,
        } }, span));
    }

    var end = offset;
    while (end < source.len) : (end += 1) switch (source[end]) {
        '<', '{', '>', '}' => return .decline,
        '@', '/' => if (startsChild(source[end..])) break,
        else => {},
    };
    if (end == offset or end == source.len) return .decline;
    return claimed(try p.addJsxText(.{ .start = offset, .end = end }));
}

pub fn jsx_attribute(p: *Parser) Error!Claim {
    const start = p.checkpoint();
    if (try p.parseJsxExpressionContainer()) |container| {
        const value = p.tree().data(container).jsx_expression_container.expression;
        if (p.tree().data(value) == .identifier_reference) {
            return claimed(try p.addNode(.shorthand, &.{container}, p.tree().span(container)));
        }
    }
    p.rewind(start);
    return .decline;
}

pub fn jsx_element_name(p: *Parser) Error!Claim {
    if (p.token().tag != .left_brace) return .decline;
    return claimed(try p.parseJsxExpressionContainer() orelse return .failed);
}

pub fn module_specifier(p: *Parser) Error!Claim {
    const token = p.token();
    if (token.tag != .identifier) return .decline;

    const name = p.tree().sourceSlice(token.span.start, token.span.end);
    const node = try p.tree().addNode(.{ .identifier_reference = .{ .name = name } }, token.span);
    if (!try p.advance()) return .failed;
    return claimed(node);
}

pub fn print(w: *dialect.Printer, kind: Node, children: []const NodeIndex) Error!void {
    switch (kind) {
        .each => {
            try w.write("each (");
            try w.node(children[0]);
            try w.write(" of ");
            try w.node(children[1]);
            try w.write(")");
            try w.space();
            try w.block(children[2..]);
        },
        .assertion => {
            try w.write("assert ");
            try w.node(children[0]);
            try w.write(";");
        },
        .topic => try w.write("%"),
        .pipeline => {
            try w.node(children[0]);
            try w.write(" |> ");
            try w.node(children[1]);
        },
        .expression_body => {
            try w.write("= ");
            try w.node(children[0]);
        },
        .interpolation => {
            try w.write("@(");
            try w.node(children[0]);
            try w.write(")");
        },
        .marked => {
            try w.write("@");
            try w.node(children[0]);
        },
        .shorthand => try w.node(children[0]),
        .rescue => {
            try w.write("rescue (");
            try w.node(children[0]);
            try w.write(")");
            try w.space();
            try w.block(children[1..]);
        },
    }
}

fn parseEach(p: *Parser) Error!Claim {
    const start = p.token().span.start;
    if (!try p.advance() or !try p.expect(.left_paren, "Expected '(' after 'each'")) return .failed;
    const item = try p.parseBindingPattern() orelse return .failed;
    if (!try p.expect(.of, "Expected 'of' after the item of 'each'")) return .failed;
    const list = try p.parseExpression(ast.Precedence.Assignment) orelse return .failed;
    if (!try p.expect(.right_paren, "Expected ')' to close 'each'")) return .failed;

    var children = p.list();
    defer children.reset();
    try children.append(item);
    try children.append(list);
    const end = try parseBody(p, &children, "each") orelse return .failed;
    return claimed(try p.addNode(.each, children.items(), .{ .start = start, .end = end }));
}

fn parseRescue(p: *Parser) Error!Claim {
    const start = p.token().span.start;
    if (!try p.advance() or !try p.expect(.left_paren, "Expected '(' after 'rescue'")) {
        return .failed;
    }
    const param = try p.parseBindingPattern() orelse return .failed;
    if (!try p.expect(.right_paren, "Expected ')' to close 'rescue'")) return .failed;

    var children = p.list();
    defer children.reset();
    try children.append(param);
    const end = try parseBody(p, &children, "rescue") orelse return .failed;
    return claimed(try p.addNode(.rescue, children.items(), .{ .start = start, .end = end }));
}

// the statements of a `{ }` body, returning where it ends
fn parseBody(p: *Parser, children: *const dialect.List, comptime name: []const u8) Error!?u32 {
    if (!try p.expect(.left_brace, "Expected '{' to open the body of '" ++ name ++ "'")) {
        return null;
    }
    while (p.token().tag != .right_brace and p.token().tag != .eof) {
        try children.append(try p.parseStatement() orelse return null);
    }
    const end = p.token().span.end;
    if (!try p.expect(.right_brace, "Expected '}' to close '" ++ name ++ "'")) return null;
    return end;
}

// `@<tag>…</tag>` as a value
fn parseMarked(p: *Parser) Error!Claim {
    const start = p.token().span.start;
    if (!try p.seek(p.token().span.end)) return .failed;
    const element = try p.parseJsxElement() orelse return .failed;
    const span: ast.Span = .{ .start = start, .end = p.tree().span(element).end };
    if (!try p.advance()) return .failed;
    return claimed(try p.addNode(.marked, &.{element}, span));
}

fn parseAssertion(p: *Parser) Error!Claim {
    const start = p.token().span.start;
    if (!try p.advance()) return .failed;
    const condition = try p.parseExpression(ast.Precedence.Lowest) orelse return .failed;
    const end = try p.semicolon(p.tree().span(condition).end) orelse return .failed;
    return claimed(try p.addNode(.assertion, &.{condition}, .{ .start = start, .end = end }));
}

fn claimed(node: NodeIndex) Claim {
    return .{ .node = node };
}

fn startsChild(text: []const u8) bool {
    for ([_][]const u8{ "@(", "@<", "/*" }) |start| {
        if (std.mem.startsWith(u8, text, start)) return true;
    }
    return false;
}

fn isRaw(tree: *const ast.Tree, opening: NodeIndex) bool {
    if (tree.data(opening) != .jsx_opening_element) return false;
    const name = tree.data(opening).jsx_opening_element.name;
    if (tree.data(name) != .jsx_identifier) return false;
    return std.mem.eql(u8, tree.string(tree.data(name).jsx_identifier.name), "raw");
}

const testing = std.testing;

const every_position =
    \\import { range } from std;
    \\function double(x) = x * 2
    \\each (item of range(3)) {
    \\  assert item >= 0;
    \\  const value = item |> double(%);
    \\}
    \\rescue (error) {
    \\  assert error;
    \\}
    \\const view = <{tag} {value}>a @(value) b @<i>c</i><raw>{not code}</raw></{tag}>;
;

fn parseSource(source: []const u8, enabled: bool) !ast.Tree {
    return parser.parse(testing.allocator, source, .{ .lang = .jsx, .dialect = enabled });
}

fn expectClean(tree: *const ast.Tree) !void {
    for (tree.diagnostics.items) |diagnostic| std.debug.print("{s}\n", .{diagnostic.message});
    try testing.expect(!tree.hasDiagnostics());
}

fn firstOf(tree: *const ast.Tree, kind: Node) ?NodeIndex {
    for (tree.nodes.items(.data), 0..) |data, i| {
        if (data == .dialect and data.dialect.kind == kind) {
            return @fromBackingInt(@intCast(i));
        }
    }
    return null;
}

fn initOf(tree: *const ast.Tree, statement_index: usize) NodeIndex {
    const declaration = tree.extra(tree.data(tree.root).program.body)[statement_index];
    const declarator = tree.extra(tree.data(declaration).variable_declaration.declarators)[0];
    return tree.data(declarator).variable_declarator.init;
}

fn exportedOf(tree: *const ast.Tree) NodeIndex {
    const first = tree.extra(tree.data(tree.root).program.body)[0];
    return tree.data(first).export_default_declaration.declaration;
}

test "every position claims its syntax" {
    var tree = try parseSource(every_position, true);
    defer tree.deinit();
    try expectClean(&tree);

    for (std.enums.values(Node)) |kind| try testing.expect(firstOf(&tree, kind) != null);
    const import_declaration = tree.extra(tree.data(tree.root).program.body)[0];
    const specifier = tree.data(import_declaration).import_declaration.source;
    try testing.expect(tree.data(specifier) == .identifier_reference);
}

test "a source without the dialect is standard syntax" {
    var standard = try parseSource("const a = <raw>{b}</raw>;", false);
    defer standard.deinit();
    try expectClean(&standard);
    try testing.expect(firstOf(&standard, .topic) == null);
    const element = standard.data(initOf(&standard, 0)).jsx_element;
    const child = standard.extra(element.children)[0];
    try testing.expect(standard.data(child) == .jsx_expression_container);

    var rejected = try parseSource("const a = %;", false);
    defer rejected.deinit();
    try testing.expect(rejected.hasErrors());
}

test "the operator position binds at the precedence it receives" {
    const source = "const a = 1 + 2 |> f(%) |> g(%);\nconst b = c\n(d)";
    var tree = try parseSource(source, true);
    defer tree.deinit();
    try expectClean(&tree);

    const outer = tree.extra(tree.data(initOf(&tree, 0)).dialect.children);
    const inner = tree.extra(tree.data(outer[0]).dialect.children);
    try testing.expect(tree.data(inner[0]) == .binary_expression);
    try testing.expectEqual(3, tree.extra(tree.data(tree.root).program.body).len);
}

test "the dialect's files and node shapes reach the packages as JSON" {
    try testing.expectEqualStrings(
        \\{"files":[{"extension":".jsx","lang":"jsx"}],"shapes":
    ++
        \\[{"type":"EachStatement","fields":["item","list","body"],"list":2,"properties":{}},
    ++
        \\{"type":"AssertStatement","fields":["argument"],"list":-1,"properties":{}},
    ++
        \\{"type":"TopicReference","fields":[],"list":-1,"properties":{}},
    ++
        \\{"type":"PipelineExpression","fields":["left","right"],"list":-1,
    ++
        \\"properties":{"operator":"|>"}},
    ++
        \\{"type":"ExpressionBody","fields":["expression"],"list":-1,"properties":{}},
    ++
        \\{"type":"JSXInterpolation","fields":["expression"],"list":-1,"properties":{}},
    ++
        \\{"type":"JSXMarkedElement","fields":["element"],"list":-1,"properties":{}},
    ++
        \\{"type":"JSXShorthandAttribute","fields":["value"],"list":-1,"properties":{}},
    ++
        \\{"type":"RescueStatement","fields":["param","body"],"list":1,"properties":{}}]}
    , dialect.description);
}

test "a comment the dialect reads joins the tree's comments and prints" {
    const source = "const a = <p>x /* note */ y</p>;";
    var tree = try parser.parse(testing.allocator, source, .{
        .lang = .jsx,
        .dialect = true,
        .comments = .attach,
    });
    defer tree.deinit();
    try expectClean(&tree);

    try testing.expectEqual(1, tree.comments.len);
    try testing.expectEqualStrings(" note ", tree.string(tree.comments[0].value));
    const printed = try parser.codegen.generate(testing.allocator, &tree, .{ .comments = .all });
    defer printed.deinit(testing.allocator);
    try testing.expectEqualStrings("const a = <p>x {/* note */} y</p>;", printed.code);
}

test "an attribute the dialect declines rewinds to standard syntax" {
    var tree = try parseSource("const a = <a {...rest} {name} />;", true);
    defer tree.deinit();
    try expectClean(&tree);

    const opening = tree.data(tree.data(initOf(&tree, 0)).jsx_element.opening_element);
    const attributes = tree.extra(opening.jsx_opening_element.attributes);
    try testing.expect(tree.data(attributes[0]) == .jsx_spread_attribute);
    try testing.expect(tree.data(attributes[1]) == .dialect);
}

test "a failed claim reports its diagnostic" {
    var tree = try parseSource("const a = <p>@(1 2)</p>;", true);
    defer tree.deinit();
    try testing.expectEqualStrings("Expected ')' to close '@('", tree.diagnostics.items[0].message);
}

test "a dialect binding lands in its node's scope" {
    var tree = try parseSource("each (item of items) { use(item); }", true);
    defer tree.deinit();
    const semantic = try parser.semantic.analyze(&tree, .{ .check = true });
    try expectClean(&tree);

    const each = firstOf(&tree, .each).?;
    for (tree.nodes.items(.data), 0..) |data, i| {
        if (data != .identifier_reference) continue;
        if (!std.mem.eql(u8, tree.string(data.identifier_reference.name), "item")) continue;
        const symbol = semantic.symbol(semantic.symbolOf(@fromBackingInt(@intCast(i))).?);
        try testing.expect(symbol.flags.block_scoped_var);
        try testing.expectEqual(each, semantic.scope(symbol.scope).node);
    }
}

test "codegen prints dialect nodes back to the source" {
    var tree = try parseSource(every_position, true);
    defer tree.deinit();
    const printed = try parser.codegen.generate(testing.allocator, &tree, .{});
    defer printed.deinit(testing.allocator);

    try testing.expectEqualStrings(every_position, printed.code);
}

test "a dialect loop is the target of break and continue" {
    var tree = try parseSource("each (item of items) { if (item) continue; break; }", true);
    defer tree.deinit();
    _ = try parser.semantic.analyze(&tree, .{ .check = true });
    try expectClean(&tree);

    var crossing = try parseSource("each (item of items) { () => { continue; }; }", true);
    defer crossing.deinit();
    _ = try parser.semantic.analyze(&crossing, .{ .check = true });
    try testing.expect(crossing.hasErrors());
}

test "a var redeclares a dialect catch binding while the binding is an identifier" {
    var simple = try parseSource("rescue (error) { var error = 1; }", true);
    defer simple.deinit();
    _ = try parser.semantic.analyze(&simple, .{ .check = true });
    try expectClean(&simple);

    var pattern = try parseSource("rescue ({ message }) { var message = 1; }", true);
    defer pattern.deinit();
    _ = try parser.semantic.analyze(&pattern, .{ .check = true });
    try testing.expect(pattern.hasErrors());
}

test "after export default, an @ the dialect reads is its expression, and else a decorator" {
    var marked = try parseSource("export default @<i>c</i>;", true);
    defer marked.deinit();
    try expectClean(&marked);
    try testing.expect(marked.data(exportedOf(&marked)) == .dialect);

    var decorated = try parseSource("export default @dec class {}", true);
    defer decorated.deinit();
    try expectClean(&decorated);
    try testing.expect(decorated.data(exportedOf(&decorated)) == .class);
}

test "a closing tag matches the dialect's tag by its expression, without comments" {
    var tree = try parseSource("const a = <{tag /* opens */}>x</{ tag }>;", true);
    defer tree.deinit();
    try expectClean(&tree);

    var mismatched = try parseSource("const a = <{a}>x</{b}>;", true);
    defer mismatched.deinit();
    try testing.expect(mismatched.hasErrors());
}
