const std = @import("std");
const util = @import("util");
const spec = @import("spec");
const Simd = util.Simd;
const ast = @import("../ast.zig");
const output = @import("output.zig");
const sourcemap = @import("sourcemap.zig");
const utils = @import("utils.zig");
const lexical = @import("../lexical.zig");

const source_maps = @import("codegen_options").source_maps;

const Allocator = std.mem.Allocator;
const Output = output.Output;
const Tree = ast.Tree;
const NodeIndex = ast.NodeIndex;
const NodeData = ast.NodeData;
const NodeTag = ast.NodeTag;
const IndexRange = ast.IndexRange;
const Precedence = ast.Precedence;
const Diagnostic = ast.Diagnostic;

pub const SourceMap = sourcemap.SourceMap;
pub const SourceMapOptions = sourcemap.Options;

/// Whitespace mode. `compact` emits only the whitespace the grammar requires.
pub const Format = enum { pretty, compact };

/// Quote style for string literals. `shortest` picks the quote with fewer escapes, double on a tie.
pub const Quotes = enum { preserve, double, single, shortest };

/// Comment passthrough filter. `some` keeps legal headers, JSDoc, and annotations.
pub const Comments = enum { none, all, some, line, block };

pub const Options = struct {
    /// Apply size-reducing syntax rewrites.
    minify: bool = false,
    format: Format = .pretty,
    /// Spaces per level in pretty format.
    indent: u8 = 2,
    quotes: Quotes = .preserve,
    source_map: ?SourceMapOptions = null,
    comments: Comments = .some,
    max_depth: u32 = 2048,
};

pub const Result = struct {
    code: []const u8,
    diagnostics: []const Diagnostic,
    map: ?SourceMap = null,

    pub fn deinit(self: Result, allocator: Allocator) void {
        allocator.free(self.code);
        allocator.free(self.diagnostics);
        if (self.map) |m| m.deinit(allocator);
    }
};

pub fn generate(allocator: Allocator, tree: *const Tree, options: Options) Allocator.Error!Result {
    std.debug.assert(tree.root != .null);
    var p: Printer = .{
        .allocator = allocator,
        .tree = tree,
        .node_data = tree.nodes.items(.data),
        .options = options,
        .comments_on = options.comments != .none and tree.comment_index.owners.items.len > 0,
        .out = try .init(
            allocator,
            options.format == .pretty,
            tree.source,
            options.source_map,
            tree.nodes.len * 8 + 64,
        ),
    };
    defer p.deinit();
    p.emit(tree.root);
    if (p.out.failed) return error.OutOfMemory;
    std.debug.assert(p.owed == .null);
    std.debug.assert(p.defer_trailing_of == .null);
    std.debug.assert(p.links.items.len == 0);
    std.debug.assert(p.depth == 0);
    std.debug.assert(p.restricted == null);
    std.debug.assert(p.indent_depth == 0);

    const code = try p.out.code.toOwnedSlice(allocator);
    errdefer allocator.free(code);
    const diagnostics = try p.diagnostics.toOwnedSlice(allocator);
    errdefer allocator.free(diagnostics);
    const map = if (comptime source_maps)
        (if (p.out.map) |*map| try map.build(allocator) else null)
    else
        null;
    return .{ .code = code, .diagnostics = diagnostics, .map = map };
}

const Ctx = packed struct(u32) {
    prec: u8 = Precedence.Lowest,
    no_in: bool = false,
    no_call: bool = false,
    no_chain: bool = false,
    // the next token would change how TypeScript reads a trailing `f<T>`
    no_instantiation: bool = false,
    no_jsx_tag: bool = false,
    tagged: bool = false,
    defer_trailing: bool = false,
    no_decorators: bool = false,
    postfix_operand: bool = false,
    bare: bool = false,
    definite: bool = false,
    _: u13 = 0,

    fn assignment(ctx: Ctx) Ctx {
        return .{ .prec = Precedence.Assignment, .no_in = ctx.no_in };
    }

    fn unary(ctx: Ctx) Ctx {
        return .{ .prec = Precedence.Unary, .no_instantiation = ctx.no_instantiation };
    }
};

const TPrec = struct {
    const trailing: u8 = 1;
    const @"union": u8 = 2;
    const intersection: u8 = 3;
    const operator: u8 = 4;
    const primary: u8 = 5;
};

const Mark = enum { sp, semi, flush };
const sp: Mark = .sp;
const semi: Mark = .semi;
const flush: Mark = .flush;

const Operand = struct { idx: NodeIndex, ctx: Ctx };

const Restricted = struct { idx: NodeIndex, ctx: Ctx };

const Inside = struct { idx: NodeIndex };

inline fn at(idx: NodeIndex, ctx: Ctx) Operand {
    return .{ .idx = idx, .ctx = ctx };
}

inline fn value(idx: NodeIndex) Operand {
    return at(idx, .{ .prec = Precedence.Assignment });
}

inline fn restricted(idx: NodeIndex, ctx: Ctx) Restricted {
    return .{ .idx = idx, .ctx = ctx };
}

inline fn inside(idx: NodeIndex) Inside {
    return .{ .idx = idx };
}

const Printer = struct {
    allocator: Allocator,
    tree: *const Tree,
    node_data: []const NodeData,
    options: Options,
    out: Output,
    diagnostics: std.ArrayList(Diagnostic) = .empty,

    comments_on: bool,
    indent_depth: u32 = 0,
    pending_semi: bool = false,
    skip_leading_of: NodeIndex = .null,
    defer_trailing_of: NodeIndex = .null,
    in_assign_target: bool = false,
    in_prologue: bool = false,
    restricted: ?*bool = null,
    restricted_len: usize = 0,
    // the node whose trailing comments follow its parent's next token
    owed: NodeIndex = .null,

    depth: u32 = 0,
    links: std.ArrayList(Link) = .empty,
    decoded: std.ArrayList(u8) = .empty,

    const DecoratorLayout = enum { own_line, same_line };

    const Body = enum { statements, prologue, members, enumerators };

    const Item = enum { value, target, parameter };

    const Link = struct {
        idx: NodeIndex,
        inner: Ctx,
        wrap: bool,
        defer_trailing: bool,
        comments: ?ast.CommentIterator,
    };

    const Self = @This();

    fn deinit(self: *Self) void {
        self.out.deinit();
        self.diagnostics.deinit(self.allocator);
        self.links.deinit(self.allocator);
        self.decoded.deinit(self.allocator);
    }

    inline fn nodeData(self: *const Self, idx: NodeIndex) NodeData {
        return self.node_data[@backingInt(idx)];
    }

    inline fn tagOf(self: *const Self, idx: NodeIndex) NodeTag {
        return std.meta.activeTag(self.node_data[@backingInt(idx)]);
    }

    inline fn writeString(self: *Self, id: ast.String) void {
        self.out.writeToken(self.tree.string(id));
    }

    inline fn print(self: *Self, parts: anytype) void {
        inline for (parts) |part| {
            const info = @typeInfo(@TypeOf(part));
            if (info == .@"struct" and info.@"struct".is_tuple) {
                if (isSet(part)) inline for (part) |inner| self.printPart(inner);
            } else if (@TypeOf(part) == bool) {
                @compileError("a flag prints nothing, it opens optional syntax");
            } else self.printPart(part);
        }
    }

    inline fn printPart(self: *Self, part: anytype) void {
        switch (@TypeOf(part)) {
            Mark => switch (part) {
                .sp => self.out.space(),
                .semi => self.softSemi(),
                .flush => self.flushSemi(),
            },
            NodeIndex => self.emit(part),
            Operand => self.emitExpr(part.idx, part.ctx),
            Restricted => self.emitRestrictedArg(part.idx, part.ctx),
            IndexRange => self.emitItems(self.tree.extra(part), .null, .{}, .value),
            Inside => if (self.hasComments(part.idx)) self.emitInsideComments(part.idx, false),
            ast.String => self.writeString(part),
            []const u8, [:0]const u8 => self.out.writeStr(part),
            bool => {},
            else => |Text| if (@typeInfo(@typeInfo(Text).pointer.child).array.len == 1)
                self.out.writeByte(part[0])
            else
                self.out.writeStr(part),
        }
    }

    inline fn isSet(group: anytype) bool {
        inline for (group) |part| switch (@TypeOf(part)) {
            bool => return part,
            NodeIndex => return part != .null,
            Operand => return part.idx != .null,
            IndexRange => return part.len > 0,
            else => {},
        };
        @compileError("optional syntax names no node, range, or flag");
    }

    fn emitItems(
        self: *Self,
        items: []const NodeIndex,
        rest: NodeIndex,
        ctx: Ctx,
        comptime item: Item,
    ) void {
        var item_ctx = ctx;
        item_ctx.defer_trailing = true;
        const depth = self.indent_depth;
        for (items, 0..) |x, i| {
            switch (item) {
                .value => self.emitExpr(x, item_ctx),
                .target => self.emitAssignTarget(x, item_ctx),
                .parameter => switch (self.nodeData(x)) {
                    .formal_parameter => |p| self.emitExpr(p.pattern, item_ctx),
                    else => self.emitExpr(x, item_ctx),
                },
            }
            self.closeItem(i + 1 < items.len or rest != .null, depth);
        }
        if (rest != .null) {
            self.emitExpr(rest, item_ctx);
            self.closeItem(false, depth);
        }
        self.closeList(depth);
    }

    // an item's separator precedes its trailing comments, so a line comment cannot swallow it
    inline fn closeItem(self: *Self, separated: bool, hang_depth: ?u32) void {
        if (separated) self.out.writeByte(',');
        if (self.owed != .null) return self.closeItemOwed(separated, hang_depth);
        if (separated) self.out.space();
    }

    fn closeItemOwed(self: *Self, separated: bool, hang_depth: ?u32) void {
        self.emitOwedComments();
        if (!separated) return;
        if (hang_depth) |depth| if (self.indent_depth == depth and self.out.atLineStart()) {
            self.indent_depth = depth + 1;
            self.breakLine();
        };
        if (!self.out.atLineStart()) self.out.space();
    }

    inline fn closeList(self: *Self, depth: u32) void {
        std.debug.assert(self.owed == .null);
        if (self.indent_depth == depth) return;
        std.debug.assert(self.indent_depth == depth + 1);
        self.indent_depth = depth;
        if (self.out.atLineStart()) self.breakLine();
    }

    fn printBraced(self: *Self, items: []const NodeIndex, rest: NodeIndex) void {
        if (items.len > 0 or rest != .null) {
            self.out.space();
            self.emitItems(items, rest, .{}, .value);
            if (!self.out.atLineStart()) self.out.space();
        }
        self.out.writeByte('}');
    }

    fn newline(self: *Self) void {
        if (self.out.pretty) self.breakLine();
    }

    fn hasPrintedComments(self: *const Self, idx: NodeIndex) bool {
        if (!self.comments_on) return false;
        var comments = self.tree.commentsOf(idx);
        while (comments.next()) |c| {
            if (c.position != .inside and self.allowComment(c)) return true;
        }
        return false;
    }

    inline fn emit(self: *Self, idx: NodeIndex) void {
        return self.emitExpr(idx, .{});
    }

    inline fn emitExpr(self: *Self, idx: NodeIndex, ctx: Ctx) void {
        if (idx == .null) return;
        return handlers[@backingInt(self.tagOf(idx))](self, idx, ctx);
    }

    fn Handler(comptime tag: NodeTag) type {
        return struct {
            fn run(self: *Self, idx: NodeIndex, ctx: Ctx) void {
                @setEvalBranchQuota(10_000);
                const node = &@field(self.node_data[@backingInt(idx)], @tagName(tag));
                const nests = (comptime !isLeaf(tag)) and !ctx.bare;
                if (nests) {
                    if (self.depth == self.options.max_depth) {
                        return self.diagnose(idx, "Code is nested too deeply to print");
                    }
                    self.depth += 1;
                }
                defer if (nests) {
                    self.depth -= 1;
                };
                if (!ctx.bare) {
                    const wrap = self.needsParens(tag, node, ctx);
                    if (wrap or self.hasComments(idx)) return self.emitDressed(idx, ctx, wrap);
                }
                var inner = ctx;
                inner.defer_trailing = false;
                inner.bare = false;
                self.recordMapping(idx);

                if (comptime isChainLink(tag)) return self.emitLink(tag, node, inner);
                if (comptime fixedString(tag)) |text| return self.out.writeStr(text);
                return self.emitNode(tag, idx, node, inner);
            }
        };
    }

    noinline fn emitDressed(self: *Self, idx: NodeIndex, ctx: Ctx, wrap: bool) void {
        var inner: Ctx = if (wrap) .{} else ctx;
        inner.bare = true;
        if (wrap) self.out.writeByte('(');
        const comments = self.openComments(idx);
        handlers[@backingInt(self.tagOf(idx))](self, idx, inner);
        self.closeComments(idx, comments, ctx.defer_trailing);
        if (wrap) self.out.writeByte(')');
    }

    inline fn hasComments(self: *const Self, idx: NodeIndex) bool {
        return self.comments_on and self.tree.hasComments(idx);
    }

    inline fn openComments(self: *Self, idx: NodeIndex) ?ast.CommentIterator {
        if (!self.hasComments(idx)) return null;
        const comments = self.tree.commentsOf(idx);
        const saved_lead = self.out.lead;
        self.emitLeadingComments(idx, comments);
        self.out.lead = saved_lead;
        return comments;
    }

    inline fn closeComments(
        self: *Self,
        idx: NodeIndex,
        opened: ?ast.CommentIterator,
        defer_trailing: bool,
    ) void {
        const comments = opened orelse return;
        if (defer_trailing or idx == self.defer_trailing_of) {
            std.debug.assert(self.owed == .null);
            self.owed = idx;
        } else {
            self.emitTrailingComments(comments);
        }
    }

    fn writeNodeText(self: *Self, idx: NodeIndex, text: []const u8) void {
        const scope = self.openComments(idx);
        self.recordMapping(idx);
        self.out.writeStr(text);
        self.closeComments(idx, scope, false);
    }

    fn writeVerbatimString(self: *Self, idx: NodeIndex, text: []const u8, comptime jsx: bool) void {
        const raw = self.tree.string(self.nodeData(idx).string_literal.raw);
        const spells = raw.len >= 2 and if (jsx)
            self.jsxDecodesTo(raw[1 .. raw.len - 1], text)
        else
            std.mem.eql(u8, raw[1 .. raw.len - 1], text);
        if (spells) return self.writeNodeText(idx, raw);
        const quote: u8 = if (jsx or std.mem.findScalar(u8, text, '"') == null) '"' else '\'';
        const scope = self.openComments(idx);
        self.recordMapping(idx);
        self.out.writeByte(quote);
        self.writeCharReferences(text, if (jsx) "\"&" else "");
        self.out.writeRawByte(quote);
        self.closeComments(idx, scope, false);
    }

    fn writeCharReferences(
        self: *Self,
        text: []const u8,
        comptime references: []const u8,
    ) void {
        comptime for (references) |c| std.debug.assert(c < 0x80);
        var start: usize = 0;
        for (text, 0..) |c, i| {
            if (std.mem.findScalar(u8, references, c) == null) continue;
            var buffer: [6]u8 = undefined;
            const reference = std.fmt.bufPrint(&buffer, "&#{d};", .{c}) catch unreachable;
            self.out.writeRawStr(text[start..i]);
            self.out.writeRawStr(reference);
            start = i + 1;
        }
        self.out.writeRawStr(text[start..]);
    }

    fn jsxDecodesTo(self: *Self, raw: []const u8, text: []const u8) bool {
        if (std.mem.findScalar(u8, raw, '&') == null) return std.mem.eql(u8, raw, text);
        self.decoded.clearRetainingCapacity();
        lexical.decodeJSXEntities(self.allocator, raw, &self.decoded) catch {
            self.out.failed = true;
            return false;
        };
        std.debug.assert(self.decoded.items.len <= raw.len);
        return std.mem.eql(u8, self.decoded.items, text);
    }

    inline fn emitOwedComments(self: *Self) void {
        if (self.owed != .null) self.emitOwed();
    }

    fn emitOwed(self: *Self) void {
        const owed = self.owed;
        self.owed = .null;
        self.emitTrailingComments(self.tree.commentsOf(owed));
    }

    fn writeKeywordThenOwed(self: *Self, keyword: []const u8) void {
        self.out.writeStr(keyword);
        self.emitOwedComments();
        if (!self.out.atLineStart()) self.out.writeByte(' ');
    }

    inline fn emitLink(self: *Self, comptime tag: NodeTag, node: anytype, ctx: Ctx) void {
        const head = self.linkHead(tag, node, ctx);
        if (self.depth < self.options.max_depth / 4) {
            self.emitExpr(head.idx, head.ctx);
        } else {
            self.emitChainIteratively(head);
        }
        self.emitLinkSuffix(tag, node, ctx);
    }

    noinline fn emitChainIteratively(self: *Self, head: Operand) void {
        const base = self.links.items.len;
        var next = head;
        while (next.idx != .null and isChainLink(self.tagOf(next.idx))) {
            std.debug.assert(self.links.items.len - base < self.tree.nodes.len);
            const wrap = self.onLink(next.idx, next.ctx, needsParens);
            if (wrap) self.out.writeByte('(');
            const comments = self.openComments(next.idx);
            self.recordMapping(next.idx);
            var inner: Ctx = if (wrap) .{} else next.ctx;
            inner.defer_trailing = false;
            self.links.append(self.allocator, .{
                .idx = next.idx,
                .inner = inner,
                .wrap = wrap,
                .defer_trailing = next.ctx.defer_trailing,
                .comments = comments,
            }) catch {
                self.out.failed = true;
                return;
            };
            next = self.onLink(next.idx, inner, linkHead);
        }
        self.emitExpr(next.idx, next.ctx);
        while (self.links.items.len > base) {
            const link = self.links.pop().?;
            self.onLink(link.idx, link.inner, emitLinkSuffix);
            self.closeComments(link.idx, link.comments, link.defer_trailing);
            if (link.wrap) self.out.writeByte(')');
        }
    }

    fn onLink(
        self: *Self,
        idx: NodeIndex,
        ctx: Ctx,
        comptime f: anytype,
    ) @typeInfo(@TypeOf(f)).@"fn".return_type.? {
        switch (self.node_data[@backingInt(idx)]) {
            inline else => |*node, tag| return if (comptime isChainLink(tag))
                f(self, tag, node, ctx)
            else
                unreachable,
        }
    }

    inline fn linkHead(self: *const Self, comptime tag: NodeTag, node: anytype, ctx: Ctx) Operand {
        return switch (tag) {
            .binary_expression => at(node.left, .{
                .prec = binaryLeftPrecedence(self.tree, node.*),
                .no_in = ctx.no_in,
                .no_instantiation = !canFollowTypeArguments(node.operator),
            }),
            .logical_expression => at(node.left, .{
                .prec = self.operandPrecedence(node, false),
                .no_in = ctx.no_in,
            }),
            // TypeScript rejects `f<T>?.x`, and minify prints `?.["x"]` as `?.x`
            .member_expression, .tagged_template_expression => at(
                if (tag == .member_expression) node.object else node.tag,
                .{ .prec = Precedence.Call, .no_call = ctx.no_call, .no_instantiation = true },
            ),
            // `f<T>?.()` keeps its type arguments where `f<T>()` takes them as the call's own
            .call_expression => at(node.callee, .{
                .prec = Precedence.Call,
                .no_instantiation = !node.optional,
            }),
            .chain_expression => at(node.expression, ctx),
            .ts_non_null_expression, .ts_instantiation_expression => at(node.expression, .{
                .prec = Precedence.Postfix,
                .no_instantiation = true,
                .no_chain = tag == .ts_non_null_expression,
            }),
            .ts_as_expression, .ts_satisfies_expression => at(node.expression, .{
                .prec = Precedence.Relational,
                .no_in = ctx.no_in,
                .defer_trailing = true,
            }),
            .ts_array_type, .ts_indexed_access_type => at(
                if (tag == .ts_array_type) node.element_type else node.object_type,
                .{ .prec = TPrec.primary, .defer_trailing = true },
            ),
            .ts_jsdoc_nullable_type, .ts_jsdoc_non_nullable_type => at(
                if (node.postfix) node.type_annotation else .null,
                .{},
            ),
            .ts_qualified_name => at(node.left, .{}),
            .jsx_member_expression => at(node.object, .{}),
            else => @compileError("not a chain link: " ++ @tagName(tag)),
        };
    }

    inline fn emitLinkSuffix(self: *Self, comptime tag: NodeTag, node: anytype, ctx: Ctx) void {
        switch (tag) {
            .binary_expression => {
                const op = node.operator.toString();
                const p: u8 = node.operator.toToken().precedence();
                if (utils.isWordOp(op)) {
                    self.print(.{ " ", op, " " });
                } else {
                    self.out.space();
                    // `f<T>==x` would re-lex the type argument closer into `>=`
                    if (self.out.lastByte() == '>' and (op[0] == '>' or op[0] == '=')) {
                        self.out.writeByte(' ');
                    }
                    self.print(.{ op, sp });
                }
                self.emitExpr(node.right, .{
                    .prec = if (node.operator == .exponent) p else p + 1,
                    .no_in = ctx.no_in,
                    .no_instantiation = ctx.no_instantiation,
                });
            },
            .logical_expression => {
                const prec = self.operandPrecedence(node, true);
                self.print(.{ sp, node.operator.toString(), sp });
                self.emitExpr(node.right, .{ .prec = prec, .no_in = ctx.no_in });
            },
            .member_expression => {
                const static_key = if (self.options.minify and node.computed)
                    simpleStringKey(self.tree, node.property)
                else
                    null;

                if (node.computed and static_key == null) {
                    self.print(.{ .{ node.optional, "?." }, "[", node.property, "]" });
                } else {
                    self.out.writeStr(if (node.optional) "?." else ".");
                    if (static_key) |k| {
                        self.writeNodeText(node.property, k);
                    } else {
                        self.emit(node.property);
                    }
                }
            },
            .call_expression => {
                self.print(.{ .{ node.optional, "?." }, node.type_arguments });
                self.printArgList(node.arguments);
            },
            .tagged_template_expression => {
                self.print(.{ node.type_arguments, at(node.quasi, .{ .tagged = true }) });
            },
            .chain_expression => {},
            .ts_non_null_expression => self.out.writeByte('!'),
            .ts_instantiation_expression => self.emit(node.type_arguments),
            .ts_as_expression, .ts_satisfies_expression => {
                self.writeKeywordThenOwed(if (tag == .ts_as_expression) " as" else " satisfies");
                self.emit(node.type_annotation);
            },
            .ts_array_type, .ts_indexed_access_type => {
                self.out.writeByte('[');
                self.emitOwedComments();
                if (tag == .ts_indexed_access_type) self.emit(node.index_type);
                self.out.writeByte(']');
            },
            .ts_jsdoc_nullable_type, .ts_jsdoc_non_nullable_type => {
                self.out.writeByte(if (tag == .ts_jsdoc_nullable_type) '?' else '!');
                if (!node.postfix) self.emit(node.type_annotation);
            },
            .ts_qualified_name => self.print(.{ ".", node.right }),
            .jsx_member_expression => self.print(.{ ".", node.property }),
            else => @compileError("not a chain link: " ++ @tagName(tag)),
        }
    }

    // `??` mixed with `&&` or `||` must be parenthesized
    fn operandPrecedence(self: *const Self, e: *const ast.LogicalExpression, right: bool) u8 {
        const mixed = switch (self.nodeData(if (right) e.right else e.left)) {
            .logical_expression => |operand| (operand.operator == .nullish_coalescing) !=
                (e.operator == .nullish_coalescing),
            else => false,
        };
        if (mixed) return Precedence.Grouping;
        return @as(u8, e.operator.toToken().precedence()) + @intFromBool(right);
    }

    inline fn needsParens(self: *const Self, comptime tag: NodeTag, node: anytype, ctx: Ctx) bool {
        if (ctx.no_instantiation) std.debug.assert(ctx.prec >= Precedence.Relational);
        const lead = self.out.lead;
        const starts_statement = lead == .stmt or lead == .arrow;
        const starts_declaration = lead == .stmt or lead == .export_default;
        const required = switch (tag) {
            .object_expression => starts_statement,
            .assignment_expression => starts_statement and self.tagOf(node.left) == .object_pattern,
            .function => starts_declaration and (node.type == .function_expression or
                node.type == .ts_empty_body_function_expression),
            .class => starts_declaration and node.type == .class_expression,
            .member_expression => starts_declaration and node.computed and
                isNamed(self.tree, node.object, "let"),
            .call_expression, .import_expression => ctx.no_call,
            .chain_expression => ctx.no_call or ctx.no_chain or ctx.prec >= Precedence.Call,
            .ts_instantiation_expression => ctx.no_instantiation,
            .jsx_element, .jsx_fragment => ctx.prec >= Precedence.Call,
            .binary_expression => ctx.no_in and node.operator == .in,
            else => false,
        };
        const fixed = comptime precedenceOf(tag);
        if (fixed == Precedence.Grouping) return required;
        const precedence = switch (tag) {
            .logical_expression, .binary_expression => node.operator.toToken().precedence(),
            // minify's `!0` ranks as unary
            .boolean_literal => if (self.options.minify) Precedence.Unary else Precedence.Grouping,
            else => fixed,
        };
        return required or precedence < ctx.prec;
    }

    inline fn allowComment(self: *const Self, c: ast.Comment) bool {
        return switch (self.options.comments) {
            .none => false,
            .all => true,
            .some => c.type == .block and
                utils.isSignificantBlockComment(self.tree.string(c.value)),
            .line => c.type == .line,
            .block => c.type == .block,
        };
    }

    fn emitLeadingComments(self: *Self, idx: NodeIndex, comments: ast.CommentIterator) void {
        if (idx == self.skip_leading_of) return;
        var it = comments;
        while (it.next()) |c| {
            if (c.position == .before and self.allowComment(c)) self.writeLeading(c);
        }
    }

    // a comment between `get`/`async` and its key would split them
    fn hoistKeyComments(self: *Self, key: NodeIndex) void {
        if (!self.comments_on or key == .null) return;
        self.emitLeadingComments(key, self.tree.commentsOf(key));
        self.skip_leading_of = key;
    }

    fn breaksLineAfter(self: *const Self, idx: NodeIndex) bool {
        if (!self.comments_on) return false;
        var comments = self.tree.commentsOf(idx);
        while (comments.next()) |c| {
            if (c.position != .after or !self.allowComment(c)) continue;
            if (c.type == .line or !c.same_line) return true;
            if (hasLineTerminator(self.tree.string(c.value))) return true;
        }
        return false;
    }

    fn emitTrailingComments(self: *Self, comments: ast.CommentIterator) void {
        var it = comments;
        while (it.next()) |c| {
            if (c.position == .after and self.allowComment(c)) {
                // a deferred `;` landing after the comment would re-home it on reparse
                self.flushSemi();
                self.writeTrailing(c);
            }
        }
    }

    fn writeLeading(self: *Self, c: ast.Comment) void {
        if (c.type == .block and c.same_line) {
            // a comment spanning lines is a line terminator too
            if (self.restrictedArmed() and hasLineTerminator(self.tree.string(c.value))) {
                self.openRestrictedParen();
            }
            const last = self.out.lastByte();
            if (self.out.pretty and last != 0 and last != ' ' and last != '\n') {
                self.out.writeByte(' ');
            }
            self.writeCommentBody(c);
            self.out.space();
        } else {
            self.breakLine();
            self.writeCommentBody(c);
            self.breakLine();
        }
        // a comment is not the operand's first token
        if (self.restricted != null) self.restricted_len = self.out.len();
    }

    fn writeTrailing(self: *Self, c: ast.Comment) void {
        if (c.same_line) self.out.space() else self.breakLine();
        self.writeCommentBody(c);
        if (c.type == .line or !c.same_line) self.breakLine();
    }

    inline fn writeCommentBody(self: *Self, c: ast.Comment) void {
        const text = self.tree.string(c.value);
        self.out.writeStr(if (c.type == .line) "//" else "/*");
        if (c.type == .block) {
            self.writeBlockBody(text);
            self.out.writeComment("*/");
        } else {
            self.out.writeComment(text);
        }
    }

    fn writeBlockBody(self: *Self, body: []const u8) void {
        if (!self.out.pretty or !utils.isJsdocBody(body)) return self.out.writeComment(body);
        var it = std.mem.splitScalar(u8, body, '\n');
        self.out.writeStr(std.mem.trimEnd(u8, it.first(), "\r"));
        while (it.next()) |line| {
            self.breakLine();
            const text = std.mem.trimStart(u8, std.mem.trimEnd(u8, line, "\r"), " \t");
            // a line break alone would collapse a blank line
            if (text.len == 0 and it.peek() != null) {
                self.out.held_spaces = 0;
                self.out.writeComment("\n");
                continue;
            }
            self.out.writeByte(' ');
            self.out.writeStr(text);
        }
    }

    fn breakLine(self: *Self) void {
        if (self.restrictedArmed()) self.openRestrictedParen();
        self.out.endLine(self.indent_depth * self.options.indent);
    }

    inline fn recordMapping(self: *Self, idx: NodeIndex) void {
        if (comptime !source_maps) return;
        if (self.out.mapped) self.out.recordMapping(self.tree.span(idx));
    }

    noinline fn diagnose(self: *Self, idx: NodeIndex, message: []const u8) void {
        self.diagnostics.append(self.allocator, .{
            .severity = .@"error",
            .message = message,
            .span = self.tree.span(idx),
        }) catch {
            self.out.failed = true;
        };
    }

    inline fn emitNode(
        self: *Self,
        comptime tag: NodeTag,
        idx: NodeIndex,
        n: anytype,
        ctx: Ctx,
    ) void {
        switch (tag) {
            .formal_parameters, .formal_parameter => {
                std.debug.panic("codegen: {s} is emitted by its parent", .{@tagName(tag)});
            },

            .block_statement, .ts_module_block => self.printBlock(idx, n.body, .statements),
            .function_body => self.printBlock(idx, n.body, .prologue),
            .class_body, .ts_interface_body => self.printBlock(idx, n.body, .members),
            .ts_type_literal => self.printBlock(idx, n.members, .members),
            .ts_enum_body => self.printBlock(idx, n.members, .enumerators),
            // never deferred, `if(x);` needs the `;` to materialize the body
            .empty_statement => self.out.writeByte(';'),
            .debugger_statement => self.print(.{ "debugger", inside(idx), semi }),
            .break_statement => self.print(.{ "break", inside(idx), .{ " ", n.label }, semi }),
            .continue_statement => self.print(.{
                "continue", inside(idx), .{ " ", n.label }, semi,
            }),
            .return_statement => self.print(.{
                "return", inside(idx), restricted(n.argument, .{}), semi,
            }),
            .throw_statement => self.print(.{ "throw", restricted(n.argument, .{}), semi }),
            .labeled_statement => self.print(.{ n.label, ":", sp, n.body }),
            .if_statement => {
                self.print(.{ "if", sp, "(", n.@"test", ")", sp, n.consequent });
                self.print(.{.{ flush, sp, "else ", n.alternate }});
            },
            .do_while_statement => self.print(.{
                "do ", n.body, flush, sp, "while", sp, "(", n.@"test", ");",
            }),
            .with_statement => self.print(.{ "with", sp, "(", n.object, ")", sp, n.body }),
            .while_statement => self.print(.{ "while", sp, "(", n.@"test", ")", sp, n.body }),
            .try_statement => self.print(.{
                "try ", n.block, .{ sp, n.handler }, .{ sp, "finally ", n.finalizer },
            }),
            .catch_clause => self.print(.{ "catch", .{ sp, "(", n.param, ")" }, sp, n.body }),
            .for_in_statement, .for_of_statement => {
                const of = tag == .for_of_statement;
                // `for (async of …)` is forbidden, it could start the arrow `async of => …`
                const parens = of and !n.await and isNamed(self.tree, n.left, "async");
                self.print(.{ "for", .{ of and n.await, " await" }, sp, "(", .{ parens, "(" } });
                switch (self.nodeData(n.left)) {
                    .variable_declaration => self.printForDeclaration(n.left, false),
                    else => self.emitAssignTarget(n.left, .{}),
                }
                self.print(.{ .{ parens, ")" }, if (of) " of " else " in " });
                self.print(.{ if (of) value(n.right) else at(n.right, .{}), ")", sp, n.body });
            },

            .variable_declarator => {
                self.emitExpr(n.id, .{ .definite = n.definite });
                self.print(.{.{ sp, "=", sp, at(n.init, ctx.assignment()) }});
            },
            .sequence_expression => {
                self.emitItems(self.tree.extra(n.expressions), .null, ctx.assignment(), .value);
            },
            .conditional_expression => {
                self.emitExpr(n.@"test", .{ .prec = Precedence.LogicalOr, .no_in = ctx.no_in });
                self.print(.{ sp, "?", sp, value(n.consequent), sp, ":", sp });
                self.emitExpr(n.alternate, ctx.assignment());
            },
            .assignment_expression => {
                self.emitAssignTarget(n.left, .{});
                self.print(.{ sp, n.operator.toString(), sp, at(n.right, ctx.assignment()) });
            },
            .parenthesized_expression => self.print(.{ "(", n.expression, ")" }),
            .await_expression => self.print(.{ "await ", at(n.argument, ctx.unary()) }),
            .yield_expression => self.print(.{
                "yield", .{ n.delegate, "*" }, restricted(n.argument, ctx.assignment()),
            }),
            .spread_element => self.print(.{ "...", value(n.argument) }),
            .array_expression => self.printArray(idx, n.elements, .null, self.in_assign_target),
            .meta_property => self.print(.{ n.meta, ".", n.property }),
            .template_literal => self.printTemplate(n.quasis, n.expressions, ctx.tagged),
            .template_element => self.printTemplateElement(n.*, false),
            .bigint_literal => self.print(.{ n.raw, "n" }),
            .identifier_reference,
            .identifier_name,
            .label_identifier,
            .jsx_identifier,
            => self.writeString(n.name),
            .private_identifier => self.print(.{ "#", n.name }),

            .assignment_pattern => {
                self.printDecorators(n.decorators, .same_line);
                self.print(.{ n.left, .{ n.optional, "?" }, n.type_annotation });
                self.print(.{ sp, "=", sp, value(n.right) });
            },
            .binding_rest_element => {
                self.printDecorators(n.decorators, .same_line);
                self.print(.{ "...", n.argument, .{ n.optional, "?" }, n.type_annotation });
            },
            .array_pattern => {
                self.printDecorators(n.decorators, .same_line);
                self.printArray(idx, n.elements, n.rest, true);
                self.print(.{ .{ n.optional, "?" }, .{ ctx.definite, "!" }, n.type_annotation });
            },
            .object_pattern => {
                self.printDecorators(n.decorators, .same_line);
                self.print(.{ "{", inside(idx) });
                self.printBraced(self.tree.extra(n.properties), n.rest);
                self.print(.{ .{ n.optional, "?" }, .{ ctx.definite, "!" }, n.type_annotation });
            },

            .import_default_specifier => self.emit(n.local),
            .import_namespace_specifier => self.print(.{ "* as ", n.local }),
            .import_specifier => self.printSpecifier(n.import_kind == .type, n.imported, n.local),
            .export_specifier => self.printSpecifier(n.export_kind == .type, n.local, n.exported),
            .import_attribute => self.print(.{ n.key, ":", sp, n.value }),
            .export_all_declaration => {
                self.print(.{ "export", .{ n.export_kind == .type, " type" }, " *" });
                self.print(.{ .{ " as ", n.exported }, " from ", n.source });
                self.print(.{ .{ " with ", "{", sp, n.attributes, sp, "}" }, semi });
            },

            .ts_type_annotation => self.print(.{ ":", sp, n.type_annotation }),
            .ts_type_reference => self.print(.{ n.type_name, n.type_arguments }),
            .ts_interface_heritage, .ts_class_implements => {
                self.print(.{ n.expression, n.type_arguments });
            },
            .ts_type_query => self.print(.{ "typeof ", n.expr_name, n.type_arguments }),
            .ts_import_type => {
                self.print(.{ "import(", n.source, .{ ",", sp, n.options }, ")" });
                self.print(.{ .{ ".", n.qualifier }, n.type_arguments });
            },
            .ts_type_parameter => {
                self.print(.{ .{ n.@"const", "const " }, .{ n.in, "in " }, .{ n.out, "out " } });
                self.print(.{ n.name, .{ " extends ", n.constraint } });
                self.print(.{.{ sp, "=", sp, n.default }});
            },
            .ts_type_parameter_instantiation => self.print(.{ "<", n.params, ">" }),
            .ts_template_literal_type => self.printTemplate(n.quasis, n.types, false),
            .ts_tuple_type => self.print(.{ "[", inside(idx), n.element_types, "]" }),
            .ts_named_tuple_member => self.print(.{
                n.label, .{ n.optional, "?" }, ":", sp, n.element_type,
            }),
            .ts_optional_type => self.print(.{
                at(n.type_annotation, .{ .prec = TPrec.primary }), "?",
            }),
            .ts_rest_type => self.print(.{ "...", n.type_annotation }),
            .ts_union_type => self.emitTypeList(n.types, "|", TPrec.intersection),
            .ts_intersection_type => self.emitTypeList(n.types, "&", TPrec.operator),
            .ts_infer_type => self.print(.{ "infer ", n.type_parameter }),
            .ts_type_operator => self.print(.{
                n.operator.toString(), " ", at(n.type_annotation, .{ .prec = TPrec.operator }),
            }),
            .ts_parenthesized_type => self.print(.{ "(", n.type_annotation, ")" }),
            .ts_function_type, .ts_constructor_type => {
                const new = tag == .ts_constructor_type;
                self.print(.{ .{ new and n.abstract, "abstract " }, .{ new, "new " } });
                self.emit(n.type_parameters);
                self.printParams(idx, n.params);
                self.print(.{ sp, "=>", sp });
                self.emitUnwrappedType(n.return_type);
            },
            .ts_call_signature_declaration => self.printSignature(idx, n, .null),
            .ts_construct_signature_declaration => {
                self.out.writeStr("new ");
                self.printSignature(idx, n, .null);
            },
            .ts_index_signature => {
                self.print(.{ .{ n.static, "static " }, .{ n.readonly, "readonly " } });
                self.print(.{ "[", n.parameters, "]", n.type_annotation, semi });
            },
            .ts_interface_declaration => {
                self.print(.{ .{ n.declare, "declare " }, "interface ", n.id, n.type_parameters });
                self.print(.{ .{ " extends ", n.extends }, sp, n.body });
            },
            .ts_enum_declaration => self.print(.{
                .{ n.declare, "declare " }, .{ n.is_const, "const " }, "enum ", n.id, sp, n.body,
            }),
            .ts_enum_member => self.print(.{
                .{ n.computed, "[" }, n.id, .{ n.computed, "]" }, .{ sp, "=", sp, n.initializer },
            }),
            .ts_global_declaration => self.print(.{ .{ n.declare, "declare " }, n.id, sp, n.body }),
            .ts_export_assignment => self.print(.{ "export", sp, "=", sp, n.expression, semi }),
            .ts_namespace_export_declaration => self.print(.{ "export as namespace ", n.id, semi }),
            .ts_import_equals_declaration => {
                self.print(.{ "import ", .{ n.import_kind == .type, "type " }, n.id });
                self.print(.{ sp, "=", sp, n.module_reference, semi });
            },
            .ts_external_module_reference => self.print(.{ "require(", n.expression, ")" }),
            .ts_this_parameter => self.print(.{ "this", n.type_annotation }),

            .jsx_element => self.printChildren(n.opening_element, n.children, n.closing_element),
            .jsx_fragment => self.printChildren(n.opening_fragment, n.children, n.closing_fragment),
            .jsx_closing_element => self.print(.{ "</", n.name, ">" }),
            .jsx_namespaced_name => self.print(.{ n.namespace, ":", n.name }),
            .jsx_spread_attribute => self.print(.{ "{...", value(n.argument), "}" }),
            .jsx_spread_child => self.print(.{ "{...", value(n.expression), "}" }),
            .jsx_expression_container => self.print(.{ "{", value(n.expression), "}" }),
            .jsx_empty_expression => self.print(.{inside(idx)}),
            .jsx_opening_fragment => self.print(.{ "<", inside(idx), ">" }),
            .jsx_closing_fragment => self.print(.{ "</", inside(idx), ">" }),

            else => {
                const emitter = @field(Self, "emit_" ++ @tagName(tag));
                const params = @typeInfo(@TypeOf(emitter)).@"fn".param_types;
                const with_idx = params[1].? == NodeIndex;
                const with_ctx = params[params.len - 1].? == Ctx;
                const args = .{self} ++ (if (with_idx) .{idx} else .{}) ++ .{n} ++
                    (if (with_ctx) .{ctx} else .{});
                @call(.always_inline, emitter, args);
            },
        }
    }

    fn emit_program(self: *Self, idx: NodeIndex, p: *const ast.Program) void {
        if (p.hashbang) |h| {
            self.out.writeStr("#!");
            self.out.writeRawStr(self.tree.string(h.value));
            self.out.writeByte('\n');
        }
        self.printStmtList(self.tree.extra(p.body), .prologue);
        self.pending_semi = false;
        if (self.comments_on) self.emitInsideComments(idx, true);
    }

    fn printStmtList(self: *Self, items: []const NodeIndex, body: Body) void {
        const statements = body == .statements or body == .prologue;
        var prologue = body == .prologue;
        for (items, 0..) |s, i| {
            if (i > 0) self.newline();
            self.flushSemi();
            if (statements) self.in_prologue = prologue;
            if (body == .enumerators) {
                self.emitExpr(s, .{ .defer_trailing = true });
                self.closeItem(i + 1 < items.len, null);
            } else self.emit(s);
            if (prologue and self.nodeData(s) != .directive) prologue = false;
        }
        if (statements) self.in_prologue = false;
    }

    fn printIndentedStmtList(self: *Self, items: []const NodeIndex, body: Body) void {
        std.debug.assert(items.len > 0);
        self.indent_depth += 1;
        defer self.indent_depth -= 1;
        self.newline();
        self.printStmtList(items, body);
    }

    fn printBlock(self: *Self, idx: NodeIndex, items: IndexRange, body: Body) void {
        self.out.writeByte('{');
        if (items.len > 0) {
            self.printIndentedStmtList(self.tree.extra(items), body);
            self.pending_semi = false;
            self.newline();
        } else if (self.comments_on) {
            self.emitInsideComments(idx, true);
        }
        self.out.writeByte('}');
    }

    fn emitInsideComments(self: *Self, idx: NodeIndex, block: bool) void {
        var comments = self.tree.commentsOf(idx);
        var scan = comments;
        const broken = block or while (scan.next()) |c| {
            if (c.position == .inside and c.type == .line and self.allowComment(c)) break true;
        } else false;
        var any = false;
        self.indent_depth += @intFromBool(broken);
        while (comments.next()) |c| {
            if (c.position != .inside or !self.allowComment(c)) continue;
            any = true;
            if (broken) {
                self.breakLine();
            } else if (self.out.pretty) switch (self.out.lastByte()) {
                0, ' ', '\n', '(', '[', '{', '<' => {},
                else => self.out.writeByte(' '),
            };
            self.writeCommentBody(c);
        }
        self.indent_depth -= @intFromBool(broken);
        if (broken and any) self.breakLine();
    }

    // compact mode defers `;` so a closing `}` can drop it
    inline fn softSemi(self: *Self) void {
        if (self.out.pretty) self.out.writeByte(';') else self.pending_semi = true;
    }

    inline fn flushSemi(self: *Self) void {
        if (self.pending_semi) {
            self.pending_semi = false;
            self.out.writeByte(';');
        }
    }

    fn emit_static_block(self: *Self, idx: NodeIndex, b: *const ast.StaticBlock) void {
        self.print(.{ "static", sp });
        self.printBlock(idx, b.body, .statements);
    }

    fn emit_directive(self: *Self, d: *const ast.Directive) void {
        // an escaped `"use strict"` does not enable strict mode, so the raw lexeme must survive
        if (self.nodeData(d.expression) == .string_literal) {
            self.writeVerbatimString(d.expression, self.tree.string(d.value), false);
        } else {
            self.emit(d.expression);
        }
        self.softSemi();
    }

    fn emit_expression_statement(self: *Self, s: *const ast.ExpressionStatement) void {
        const as_directive = self.in_prologue and self.nodeData(s.expression) == .string_literal;
        self.in_prologue = false;
        self.out.lead = .stmt;
        self.print(.{ .{ as_directive, "(" }, s.expression, .{ as_directive, ")" }, semi });
    }

    // ASI splits an operand whose first token follows a line break, so that break opens a paren
    fn emitRestrictedArg(self: *Self, idx: NodeIndex, ctx: Ctx) void {
        if (idx == .null) return;
        self.out.writeByte(' ');
        var paren_open = false;
        self.restricted = &paren_open;
        self.restricted_len = self.out.len();
        self.emitExpr(idx, ctx);
        self.restricted = null;
        if (paren_open) self.out.writeByte(')');
    }

    inline fn restrictedArmed(self: *Self) bool {
        if (self.restricted == null) return false;
        if (self.out.len() == self.restricted_len) return true;
        self.restricted = null;
        return false;
    }

    fn openRestrictedParen(self: *Self) void {
        const paren_open = self.restricted.?;
        std.debug.assert(!paren_open.*);
        paren_open.* = true;
        self.restricted = null;
        // the keyword's separator space becomes part of ` (`
        self.out.held_spaces = 0;
        self.out.writeStr(" (");
    }

    fn emit_for_statement(self: *Self, s: *const ast.ForStatement) void {
        self.print(.{ "for", sp, "(" });
        if (s.init != .null) switch (self.nodeData(s.init)) {
            .variable_declaration => self.printForDeclaration(s.init, true),
            else => self.emitExpr(s.init, .{ .no_in = true }),
        };
        self.print(.{ ";", .{ sp, s.@"test" }, ";", .{ sp, s.update }, ")", sp, s.body });
    }

    fn printForDeclaration(self: *Self, idx: NodeIndex, no_in: bool) void {
        const scope = self.openComments(idx);
        self.printVariableDecl(self.nodeData(idx).variable_declaration, .{ .no_in = no_in });
        self.closeComments(idx, scope, false);
    }

    fn emit_switch_statement(self: *Self, s: *const ast.SwitchStatement) void {
        self.print(.{ "switch", sp, "(", s.discriminant, ")", sp, "{" });
        if (s.cases.len > 0) {
            self.newline();
            self.printStmtList(self.tree.extra(s.cases), .members);
            self.pending_semi = false;
            self.newline();
        }
        self.out.writeByte('}');
    }

    fn emit_switch_case(self: *Self, idx: NodeIndex, c: *const ast.SwitchCase) void {
        if (c.@"test" != .null) {
            self.print(.{ "case ", c.@"test", ":" });
        } else {
            self.print(.{ "default", inside(idx), ":" });
        }
        if (c.consequent.len == 0) return;
        self.printIndentedStmtList(self.tree.extra(c.consequent), .statements);
    }

    fn emit_variable_declaration(self: *Self, d: *const ast.VariableDeclaration) void {
        self.printVariableDecl(d.*, .{});
        self.softSemi();
    }

    fn printVariableDecl(self: *Self, d: ast.VariableDeclaration, ctx: Ctx) void {
        self.print(.{ .{ d.declare, "declare " }, d.kind.toString(), " " });
        self.emitItems(self.tree.extra(d.declarators), .null, ctx, .value);
    }

    fn emit_unary_expression(self: *Self, e: *const ast.UnaryExpression, ctx: Ctx) void {
        const op = e.operator.toString();
        self.print(.{ op, .{ utils.isWordOp(op), " " }, at(e.argument, ctx.unary()) });
    }

    fn emit_update_expression(self: *Self, e: *const ast.UpdateExpression) void {
        const op = e.operator.toString();
        if (e.prefix) self.out.writeStr(op);
        self.emitAssignTarget(e.argument, .{ .postfix_operand = !e.prefix });
        if (!e.prefix) self.out.writeStr(op);
    }

    // the parser drops the parens a ts cast needs inside a destructuring target
    fn emitAssignTarget(self: *Self, idx: NodeIndex, ctx: Ctx) void {
        if (idx == .null) return;
        const prev = self.in_assign_target;
        defer self.in_assign_target = prev;

        const cast = switch (self.nodeData(idx)) {
            .ts_as_expression, .ts_satisfies_expression, .ts_type_assertion => true,
            else => false,
        };
        const wrap = cast or (ctx.postfix_operand and self.breaksLineAfter(idx));
        self.in_assign_target = !wrap;
        self.print(.{ .{ wrap, "(" }, at(idx, ctx), .{ wrap, ")" } });
    }

    // a trailing hole needs its own comma, else `[a,]` is one element
    fn printArray(
        self: *Self,
        idx: NodeIndex,
        elements: IndexRange,
        rest: NodeIndex,
        targets: bool,
    ) void {
        const list = self.tree.extra(elements);
        self.print(.{ "[", inside(idx) });
        if (targets) {
            self.emitItems(list, rest, .{}, .target);
        } else {
            self.emitItems(list, rest, .{ .prec = Precedence.Assignment }, .value);
        }
        const hole = rest == .null and list.len > 0 and list[list.len - 1] == .null;
        self.print(.{ .{ hole, "," }, "]" });
    }

    fn emit_object_expression(self: *Self, idx: NodeIndex, e: *const ast.ObjectExpression) void {
        self.print(.{ "{", inside(idx) });
        self.printBraced(self.tree.extra(e.properties), .null);
    }

    fn emit_object_property(self: *Self, p: *const ast.ObjectProperty) void {
        const is_method = p.method or p.kind == .get or p.kind == .set;
        if (is_method and self.nodeData(p.value) == .function) {
            self.printMethodKind(p.kind, self.nodeData(p.value).function);
            self.printKey(p.key, p.computed, &.{"__proto__"});
            return self.emitMethodValue(p.value);
        }
        self.printKeyUnlessShorthand(p);
        if (!self.in_assign_target) return self.print(.{value(p.value)});
        self.emitAssignTarget(p.value, .{});
    }

    fn printMethodKind(self: *Self, kind: anytype, function: ast.Function) void {
        switch (kind) {
            .get => self.out.writeStr("get "),
            .set => self.out.writeStr("set "),
            else => self.print(.{ .{ function.async, "async " }, .{ function.generator, "*" } }),
        }
    }

    fn printKeyUnlessShorthand(self: *Self, p: anytype) void {
        if (p.shorthand and shorthandStillValid(self.tree, p.key, p.value)) return;
        self.printKey(p.key, p.computed, &.{"__proto__"});
        self.print(.{ ":", sp });
    }

    fn emit_new_expression(self: *Self, e: *const ast.NewExpression) void {
        const callee: Ctx = .{ .prec = Precedence.New, .no_call = true, .no_instantiation = true };
        self.print(.{ "new ", at(e.callee, callee), e.type_arguments });
        self.printArgList(e.arguments);
    }

    fn printArgList(self: *Self, args: IndexRange) void {
        self.out.writeByte('(');
        self.emitItems(self.tree.extra(args), .null, .{ .prec = Precedence.Assignment }, .value);
        self.out.writeByte(')');
    }

    fn emit_string_literal(self: *Self, lit: *const ast.StringLiteral) void {
        const text = self.tree.string(lit.value);
        const q = self.pickQuote(text, lit.raw);
        if (plainRunLength(text, 0, q, self.options.minify) == text.len) {
            return self.out.writeQuoted(q, text);
        }
        self.emitEscapedString(text, q);
    }

    noinline fn emitEscapedString(self: *Self, text: []const u8, q: u8) void {
        self.out.writeByte(q);
        self.writeEscaped(text, q, self.options.minify);
        self.out.writeRawByte(q);
    }

    inline fn pickQuote(self: *const Self, s: []const u8, raw: ast.String) u8 {
        return switch (self.options.quotes) {
            .preserve => blk: {
                const lexeme = self.tree.string(raw);
                break :blk if (lexeme.len != 0 and lexeme[0] == '\'') '\'' else '"';
            },
            .single => '\'',
            .double => '"',
            .shortest => if (std.mem.findScalar(u8, s, '"') == null) '"' else fewerEscapes(s),
        };
    }

    fn fewerEscapes(s: []const u8) u8 {
        const doubles = std.mem.countScalar(u8, s, '"');
        return if (std.mem.countScalar(u8, s, '\'') < doubles) '\'' else '"';
    }

    fn writeEscaped(self: *Self, s: []const u8, quote: u8, script_safe: bool) void {
        const template = quote == '`';
        var start: usize = 0;
        var i: usize = 0;
        while (i < s.len) : (i += 1) {
            if (!template) i += plainRunLength(s, i, quote, script_safe);
            if (i == s.len) break;
            const c = s[i];
            var unicode: [6]u8 = undefined;
            const esc: []const u8 = if (c >= 0x80) lone: {
                const unit = util.Utf.loneSurrogateAt(s, i) orelse continue;
                break :lone std.fmt.bufPrint(&unicode, "\\u{x:0>4}", .{unit}) catch unreachable;
            } else (if (script_safe) utils.scriptEscape(s, i) else null) orelse switch (c) {
                '\\' => "\\\\",
                '\r' => "\\r",
                0 => if (i + 1 < s.len and std.ascii.isDigit(s[i + 1])) "\\x00" else "\\0",
                '$' => if (template and i + 1 < s.len and s[i + 1] == '{') "\\$" else continue,
                '"' => if (quote == '"') "\\\"" else continue,
                '\'' => if (quote == '\'') "\\'" else continue,
                '`' => if (template) "\\`" else continue,
                else => if (template) continue else switch (c) {
                    '\n' => "\\n",
                    '\t' => "\\t",
                    0x08 => "\\b",
                    0x0C => "\\f",
                    0x0B => "\\v",
                    else => continue,
                },
            };
            self.out.writeRawStr(s[start..i]);
            self.out.writeRawStr(esc);
            if (c >= 0x80) i += 2;
            start = i + 1;
        }
        self.out.writeRawStr(s[start..]);
    }

    inline fn plainRunLength(s: []const u8, from: usize, quote: u8, minify: bool) usize {
        std.debug.assert(from <= s.len);
        var i = from;
        while (i < s.len) {
            const chunk = Simd.loadAt(s, i);
            var special = (chunk < Simd.splat(0x0E)) | (chunk >= Simd.splat(0x80)) |
                (chunk == Simd.splat('\\')) | (chunk == Simd.splat(quote));
            if (minify) special = special | (chunk == Simd.splat('<')) | (chunk == Simd.splat('>'));
            const run = Simd.firstTrueLane(special);
            i += run;
            if (run < 16) break;
        }
        std.debug.assert(i <= s.len);
        return i - from;
    }

    fn emit_numeric_literal(self: *Self, lit: *const ast.NumericLiteral) void {
        if (self.options.minify) return self.writeShortestNumber(lit.*);
        self.out.writeNumber(self.tree.string(lit.raw));
    }

    // out of line so its buffers stay off the recursion's frame
    noinline fn writeShortestNumber(self: *Self, lit: ast.NumericLiteral) void {
        const raw = self.tree.string(lit.raw);
        if (lit.kind == .decimal and utils.isMinimalInteger(raw)) return self.out.writeNumber(raw);
        var cleaned_buf: [128]u8 = undefined;
        const cleaned = utils.stripUnderscores(raw, &cleaned_buf) orelse
            return self.out.writeNumber(raw);
        if (lit.kind != .decimal) return self.out.writeNumber(cleaned);
        // `010` is legacy octal and `08` sloppy decimal
        if (cleaned.len > 1 and cleaned[0] == '0' and std.ascii.isDigit(cleaned[1])) {
            return self.out.writeNumber(cleaned);
        }
        var shortest_buf: [128]u8 = undefined;
        self.out.writeNumber(utils.shortestDecimal(cleaned, &shortest_buf));
    }

    fn emit_boolean_literal(self: *Self, lit: *const ast.BooleanLiteral) void {
        if (self.options.minify) return self.out.writeStr(if (lit.value) "!0" else "!1");
        self.out.writeStr(if (lit.value) "true" else "false");
    }

    fn emit_regexp_literal(self: *Self, lit: *const ast.RegExpLiteral) void {
        const pattern = self.tree.string(lit.pattern);
        self.out.writeByte('/');
        self.out.writeRawStr(if (pattern.len != 0) pattern else "(?:)");
        self.out.writeRawByte('/');
        self.out.writeRawStr(self.tree.string(lit.flags));
    }

    fn printTemplate(self: *Self, quasis: IndexRange, subs: IndexRange, tagged: bool) void {
        self.out.writeByte('`');
        const xs = self.tree.extra(subs);
        for (self.tree.extra(quasis), 0..) |q, i| {
            self.printTemplateElement(self.nodeData(q).template_element, tagged);
            if (i < xs.len) {
                self.out.writeRawStr("${");
                self.print(.{ xs[i], "}" });
            }
        }
        self.out.writeRawByte('`');
    }

    fn printTemplateElement(self: *Self, el: ast.TemplateElement, tagged: bool) void {
        const script_safe = self.options.minify and !tagged;
        const raw = self.tree.string(el.raw);
        if (raw.len != 0) return self.writeTemplateRaw(raw, script_safe);
        self.writeEscaped(self.tree.string(el.cooked), '`', script_safe);
    }

    // template text reads a raw `\r\n` or `\r` as `\n`
    fn writeTemplateRaw(self: *Self, raw: []const u8, script_safe: bool) void {
        std.debug.assert(raw.len > 0);
        var start: usize = 0;
        var i: usize = 0;
        while (i < raw.len) : (i += 1) {
            const esc = if (raw[i] == '\r')
                "\n"
            else if (script_safe)
                utils.scriptEscape(raw, i) orelse continue
            else
                continue;
            if (i > start) self.out.writeRawStr(raw[start..i]);
            self.out.writeRawStr(esc);
            if (raw[i] == '\r' and i + 1 < raw.len and raw[i + 1] == '\n') i += 1;
            start = i + 1;
        }
        if (start < raw.len) self.out.writeRawStr(raw[start..]);
    }

    fn emit_binding_identifier(
        self: *Self,
        idx: NodeIndex,
        id: *const ast.BindingIdentifier,
        ctx: Ctx,
    ) void {
        const plain = id.decorators.len == 0 and !id.optional and
            id.type_annotation == .null and !ctx.definite;
        if (plain) return self.writeString(id.name);
        self.emitAdornedBinding(idx, id, ctx.definite);
    }

    noinline fn emitAdornedBinding(
        self: *Self,
        idx: NodeIndex,
        id: *const ast.BindingIdentifier,
        definite: bool,
    ) void {
        self.printDecorators(id.decorators, .same_line);
        self.print(.{ id.name, .{ id.optional, inside(idx), "?" } });
        self.print(.{ .{ definite, "!" }, id.type_annotation });
    }

    fn emit_binding_property(self: *Self, p: *const ast.BindingProperty) void {
        self.printKeyUnlessShorthand(p);
        self.emitAssignTarget(p.value, .{});
    }

    fn printPropertyKey(self: *Self, key: NodeIndex, computed: bool) void {
        if (computed) self.print(.{ "[", value(key), "]" }) else self.emit(key);
    }

    fn printKey(self: *Self, key: NodeIndex, computed: bool, special: []const []const u8) void {
        if (self.options.minify) if (simpleStringKey(self.tree, key)) |s| {
            const clash = computed and for (special) |name| {
                if (std.mem.eql(u8, s, name)) break true;
            } else false;
            if (!clash) return self.writeNodeText(key, s);
        };
        self.printPropertyKey(key, computed);
    }

    fn emit_function(self: *Self, idx: NodeIndex, f: *const ast.Function) void {
        self.print(.{ .{ f.declare, "declare " }, .{ f.async, "async " }, "function" });
        self.print(.{ .{ f.generator, "*" }, .{ " ", f.id } });
        self.printSignature(idx, f, f.body);
    }

    fn emitMethodValue(self: *Self, idx: NodeIndex) void {
        const f = &self.node_data[@backingInt(idx)].function;
        const scope = self.openComments(idx);
        self.printSignature(idx, f, f.body);
        self.closeComments(idx, scope, false);
    }

    fn printSignature(self: *Self, idx: NodeIndex, n: anytype, body: NodeIndex) void {
        self.emit(n.type_parameters);
        self.printParams(idx, n.params);
        self.emit(n.return_type);
        if (body != .null) self.print(.{ sp, body }) else self.softSemi();
    }

    fn emit_arrow_function_expression(
        self: *Self,
        idx: NodeIndex,
        a: *const ast.ArrowFunctionExpression,
        ctx: Ctx,
    ) void {
        self.print(.{ .{ a.async, "async " }, at(a.type_parameters, .{ .no_jsx_tag = true }) });
        self.printParams(idx, a.params);
        self.print(.{ at(a.return_type, .{ .defer_trailing = true }), sp, "=>" });
        self.emitOwedComments();
        if (!self.out.atLineStart()) self.out.space();
        if (a.expression) {
            self.out.lead = .arrow;
            self.emitExpr(a.body, ctx.assignment());
        } else {
            self.emit(a.body);
        }
    }

    fn printParams(self: *Self, owner: NodeIndex, idx: NodeIndex) void {
        std.debug.assert(!self.tree.hasComments(idx));
        const params = self.nodeData(idx).formal_parameters;
        self.out.writeByte('(');
        self.emitItems(self.tree.extra(params.items), params.rest, .{}, .parameter);
        self.print(.{ inside(owner), ")" });
    }

    fn emit_class(self: *Self, c: *const ast.Class, ctx: Ctx) void {
        const heritage: Ctx = .{ .prec = Precedence.Call, .no_instantiation = true };
        if (!ctx.no_decorators) self.printDecorators(c.decorators, .own_line);
        self.print(.{ .{ c.declare, "declare " }, .{ c.abstract, "abstract " }, "class" });
        self.print(.{ .{ " ", c.id }, c.type_parameters });
        self.print(.{.{ " extends ", at(c.super_class, heritage), c.super_type_arguments }});
        self.print(.{ .{ " implements ", c.implements }, sp, c.body });
    }

    fn emit_method_definition(self: *Self, m: *const ast.MethodDefinition) void {
        const access = m.accessibility;
        const function = self.nodeData(m.value).function;
        self.printDecorators(m.decorators, .own_line);
        self.hoistKeyComments(m.key);
        defer self.skip_leading_of = .null;
        self.print(.{ .{ access != .none, access.toString(), " " }, .{ m.static, "static " } });
        self.print(.{ .{ m.abstract, "abstract " }, .{ m.override, "override " } });
        self.printMethodKind(m.kind, function);
        self.printKey(m.key, m.computed, if (m.static) &.{"prototype"} else &.{"constructor"});
        self.print(.{.{ m.optional, "?" }});
        self.emitMethodValue(m.value);
    }

    fn emit_property_definition(self: *Self, p: *const ast.PropertyDefinition) void {
        const access = p.accessibility;
        self.printDecorators(p.decorators, .own_line);
        self.hoistKeyComments(p.key);
        defer self.skip_leading_of = .null;
        self.print(.{ .{ p.declare, "declare " }, .{ access != .none, access.toString(), " " } });
        self.print(.{ .{ p.static, "static " }, .{ p.abstract, "abstract " } });
        self.print(.{ .{ p.override, "override " }, .{ p.readonly, "readonly " } });
        self.print(.{.{ p.accessor, "accessor " }});
        if (p.definite) self.defer_trailing_of = p.key;
        const special: []const []const u8 = &.{ "constructor", "prototype" };
        self.printKey(p.key, p.computed, if (p.static) special else special[0..1]);
        self.defer_trailing_of = .null;
        self.print(.{ .{ p.optional, "?" }, .{ p.definite, "!" } });
        self.emitOwedComments();
        self.print(.{ p.type_annotation, .{ sp, "=", sp, value(p.value) }, semi });
    }

    fn emit_decorator(self: *Self, d: *const ast.Decorator) void {
        const simple = self.decoratorIsSimple(d.expression);
        const prec = if (simple) Precedence.Lowest else Precedence.Grouping;
        self.print(.{ "@", at(d.expression, .{ .prec = prec }) });
    }

    fn decoratorIsSimple(self: *const Self, idx: NodeIndex) bool {
        var node = idx;
        for (0..self.tree.nodes.len) |_| {
            node = switch (self.nodeData(node)) {
                .identifier_reference => return true,
                .member_expression => |m| if (m.computed) return false else m.object,
                .call_expression => |c| c.callee,
                else => return false,
            };
        }
        unreachable;
    }

    inline fn printDecorators(
        self: *Self,
        decs: IndexRange,
        comptime layout: DecoratorLayout,
    ) void {
        if (decs.len > 0) self.printDecoratorList(self.tree.extra(decs), layout);
    }

    fn printDecoratorList(
        self: *Self,
        list: []const NodeIndex,
        comptime layout: DecoratorLayout,
    ) void {
        const carry = self.out.map_start;
        for (list, 0..) |d, i| {
            self.emit(d);
            // `@a.b class` would fuse to `@a.bclass` without a separator
            if (self.out.pretty and layout == .own_line) {
                self.newline();
            } else if (self.out.pretty or i + 1 == list.len) {
                self.out.writeByte(' ');
            }
        }
        self.out.map_start = carry;
    }

    fn emit_import_declaration(self: *Self, d: *const ast.ImportDeclaration) void {
        const list = self.tree.extra(d.specifiers);
        const type_only = d.import_kind == .type;

        self.print(.{ "import", .{ type_only, " type" } });
        if (d.phase) |ph| self.print(.{ " ", @tagName(ph) });
        self.out.writeByte(' ');

        if (list.len > 0) {
            const depth = self.indent_depth;
            var i: usize = 0;
            if (self.nodeData(list[0]) == .import_default_specifier) {
                self.emitExpr(list[0], .{ .defer_trailing = true });
                self.closeItem(list.len > 1, depth);
                i = 1;
            }
            if (i < list.len) {
                if (self.nodeData(list[i]) == .import_namespace_specifier) {
                    self.emit(list[i]);
                } else {
                    self.out.writeByte('{');
                    self.printBraced(list[i..], .null);
                }
            }
            self.closeList(depth);
            self.out.writeStr(" from ");
        } else if (type_only) {
            self.out.writeStr("{} from ");
        }

        self.print(.{ d.source, .{ " with ", "{", sp, d.attributes, sp, "}" }, semi });
    }

    fn emit_import_expression(self: *Self, e: *const ast.ImportExpression) void {
        self.out.writeStr("import");
        if (e.phase) |ph| self.print(.{ ".", @tagName(ph) });
        self.print(.{ "(", value(e.source), .{ ",", sp, value(e.options) }, ")" });
    }

    fn emit_export_named_declaration(
        self: *Self,
        idx: NodeIndex,
        d: *const ast.ExportNamedDeclaration,
    ) void {
        const hoisted = self.hoistedDecorators(d.declaration);
        self.printDecorators(hoisted, .own_line);
        self.out.writeStr("export");
        if (d.declaration != .null) {
            self.out.writeByte(' ');
            return self.emitExpr(d.declaration, .{ .no_decorators = hoisted.len > 0 });
        }
        self.print(.{ .{ d.export_kind == .type, " type" }, sp, "{", inside(idx) });
        self.printBraced(self.tree.extra(d.specifiers), .null);
        self.print(.{.{ " from ", d.source }});
        self.print(.{ .{ " with ", "{", sp, d.attributes, sp, "}" }, semi });
    }

    fn emit_export_default_declaration(self: *Self, d: *const ast.ExportDefaultDeclaration) void {
        const declares = self.nodeData(d.declaration).isDeclaration();
        const hoisted: IndexRange = if (declares) self.hoistedDecorators(d.declaration) else .empty;
        self.printDecorators(hoisted, .own_line);
        self.out.writeStr("export default ");
        if (declares) return self.emitExpr(d.declaration, .{ .no_decorators = hoisted.len > 0 });
        self.out.lead = .export_default;
        self.print(.{ value(d.declaration), semi });
    }

    fn hoistedDecorators(self: *const Self, declaration: NodeIndex) IndexRange {
        if (declaration == .null or self.nodeData(declaration) != .class) return .empty;
        const decorators = self.nodeData(declaration).class.decorators;
        if (decorators.len == 0) return .empty;
        const first = self.tree.span(self.tree.extra(decorators)[0]);
        return if (first.start < self.tree.span(declaration).start) decorators else .empty;
    }

    fn printSpecifier(self: *Self, type_only: bool, name: NodeIndex, alias: NodeIndex) void {
        const aliased = !sameIdentifier(self.tree, name, alias) or self.hasPrintedComments(alias);
        self.print(.{ .{ type_only, "type " }, name, .{ aliased, " as ", alias } });
    }

    fn emit_ts_type_parameter_declaration(
        self: *Self,
        d: *const ast.TSTypeParameterDeclaration,
        ctx: Ctx,
    ) void {
        // in TSX a lone `<T>` opens a JSX tag, and `<T,>` is valid TS too
        const params = self.tree.extra(d.params);
        const comma = ctx.no_jsx_tag and params.len == 1 and
            self.nodeData(params[0]).ts_type_parameter.constraint == .null;
        self.print(.{ "<", d.params, .{ comma, "," }, ">" });
    }

    fn emit_ts_literal_type(self: *Self, t: *const ast.TSLiteralType) void {
        // `!0` is no type
        switch (self.nodeData(t.literal)) {
            .boolean_literal => |b| self.out.writeStr(if (b.value) "true" else "false"),
            else => self.emit(t.literal),
        }
    }

    // a lone member keeps its leading operator (`type X = | A`) so reparse keeps the wrapper
    fn emitTypeList(self: *Self, types: IndexRange, comptime op: *const [1:0]u8, floor: u8) void {
        const list = self.tree.extra(types);
        if (list.len == 1) self.print(.{ op, sp });
        for (list, 0..) |x, i| {
            if (i > 0) self.print(.{ sp, op, sp });
            self.emitExpr(x, .{ .prec = floor });
        }
    }

    fn emit_ts_conditional_type(self: *Self, t: *const ast.TSConditionalType) void {
        self.emitExpr(t.check_type, .{ .prec = TPrec.@"union", .defer_trailing = true });
        self.writeKeywordThenOwed(" extends");
        self.emitExpr(t.extends_type, .{ .prec = TPrec.@"union" });
        self.print(.{ sp, "?", sp, t.true_type, sp, ":", sp, t.false_type });
    }

    fn emitUnwrappedType(self: *Self, idx: NodeIndex) void {
        if (idx == .null) return;
        switch (self.nodeData(idx)) {
            .ts_type_annotation => |t| {
                const scope = self.openComments(idx);
                self.emit(t.type_annotation);
                self.closeComments(idx, scope, false);
            },
            else => self.emit(idx),
        }
    }

    fn emit_ts_type_predicate(self: *Self, t: *const ast.TSTypePredicate) void {
        if (t.asserts) self.out.writeStr("asserts ");
        if (t.type_annotation == .null) return self.emit(t.parameter_name);
        self.emitExpr(t.parameter_name, .{ .defer_trailing = true });
        self.writeKeywordThenOwed(" is");
        self.emitUnwrappedType(t.type_annotation);
    }

    fn emit_ts_mapped_type(self: *Self, t: *const ast.TSMappedType) void {
        self.print(.{ "{", sp, .{ t.readonly != .none, mappedSign(t.readonly), "readonly " } });
        self.print(.{ "[", t.key, " in ", t.constraint, .{ " as ", t.name_type }, "]" });
        self.print(.{.{ t.optional != .none, mappedSign(t.optional), "?" }});
        self.print(.{ .{ ":", sp, t.type_annotation }, semi });
        self.pending_semi = false;
        self.print(.{ sp, "}" });
    }

    fn emit_ts_property_signature(self: *Self, s: *const ast.TSPropertySignature) void {
        if (s.readonly) self.out.writeStr("readonly ");
        self.printPropertyKey(s.key, s.computed);
        self.print(.{ .{ s.optional, "?" }, s.type_annotation, semi });
    }

    fn emit_ts_method_signature(self: *Self, idx: NodeIndex, s: *const ast.TSMethodSignature) void {
        self.print(.{ .{ s.kind == .get, "get " }, .{ s.kind == .set, "set " } });
        self.printPropertyKey(s.key, s.computed);
        if (s.optional) self.out.writeByte('?');
        self.printSignature(idx, s, .null);
    }

    fn emit_ts_type_alias_declaration(self: *Self, d: *const ast.TSTypeAliasDeclaration) void {
        self.print(.{ .{ d.declare, "declare " }, "type ", d.id, d.type_parameters, sp, "=", sp });
        // a leftmost bare `intrinsic` reference would reparse as the keyword
        const intrinsic = self.isLeftmostIntrinsicReference(d.type_annotation);
        self.print(.{ .{ intrinsic, "(" }, d.type_annotation, .{ intrinsic, ")" }, semi });
    }

    fn isLeftmostIntrinsicReference(self: *const Self, idx: NodeIndex) bool {
        var node = idx;
        for (0..self.tree.nodes.len) |_| {
            node = switch (self.nodeData(node)) {
                .ts_type_reference => |r| return r.type_arguments == .null and
                    isNamed(self.tree, r.type_name, "intrinsic"),
                .ts_array_type => |a| a.element_type,
                .ts_indexed_access_type => |a| a.object_type,
                inline .ts_union_type, .ts_intersection_type => |t| if (t.types.len > 0)
                    self.tree.extra(t.types)[0]
                else
                    return false,
                .ts_conditional_type => |c| c.check_type,
                else => return false,
            };
        }
        unreachable;
    }

    fn emit_ts_module_declaration(self: *Self, d: *const ast.TSModuleDeclaration) void {
        self.print(.{ .{ d.declare, "declare " }, d.kind.toString(), " ", d.id });
        if (d.body != .null) self.print(.{ sp, d.body }) else self.softSemi();
    }

    fn emit_ts_type_assertion(self: *Self, e: *const ast.TSTypeAssertion, ctx: Ctx) void {
        // `<<T>` would re-lex as `<<`
        const angled = typeStartsWithLeftAngle(self.tree, e.type_annotation);
        self.print(.{ "<", .{ angled, " " }, e.type_annotation, ">" });
        self.emitExpr(e.expression, ctx.unary());
    }

    fn emit_ts_parameter_property(self: *Self, p: *const ast.TSParameterProperty) void {
        const access = p.accessibility;
        self.printDecorators(p.decorators, .same_line);
        self.print(.{ .{ access != .none, access.toString(), " " }, .{ p.override, "override " } });
        self.print(.{ .{ p.readonly, "readonly " }, p.parameter });
    }

    fn printChildren(self: *Self, open: NodeIndex, children: IndexRange, close: NodeIndex) void {
        self.emit(open);
        for (self.tree.extra(children)) |c| self.emit(c);
        self.emit(close);
    }

    fn emit_jsx_opening_element(self: *Self, o: *const ast.JSXOpeningElement) void {
        self.print(.{ "<", o.name, o.type_arguments });
        for (self.tree.extra(o.attributes)) |a| self.print(.{ " ", a });
        if (o.self_closing) self.print(.{ sp, "/>" }) else self.out.writeByte('>');
    }

    fn emit_jsx_attribute(self: *Self, a: *const ast.JSXAttribute) void {
        self.emit(a.name);
        if (a.value == .null) return;
        self.out.writeByte('=');
        switch (self.nodeData(a.value)) {
            .string_literal => |lit| {
                self.writeVerbatimString(a.value, self.tree.string(lit.value), true);
            },
            else => self.emit(a.value),
        }
    }

    fn emit_jsx_text(self: *Self, t: *const ast.JSXText) void {
        const raw = self.tree.string(t.raw);
        const text = self.tree.string(t.value);
        if (self.jsxDecodesTo(raw, text)) return self.out.writeRawStr(raw);
        self.writeCharReferences(text, "&<>{}");
    }
};

fn fixedString(comptime tag: NodeTag) ?[]const u8 {
    return switch (tag) {
        .super => "super",
        .this_expression, .ts_this_type => "this",
        .null_literal, .ts_null_keyword => "null",
        .ts_any_keyword => "any",
        .ts_unknown_keyword => "unknown",
        .ts_never_keyword => "never",
        .ts_void_keyword => "void",
        .ts_undefined_keyword => "undefined",
        .ts_string_keyword => "string",
        .ts_number_keyword => "number",
        .ts_bigint_keyword => "bigint",
        .ts_boolean_keyword => "boolean",
        .ts_symbol_keyword => "symbol",
        .ts_object_keyword => "object",
        .ts_intrinsic_keyword => "intrinsic",
        .ts_jsdoc_unknown_type => "?",
        else => null,
    };
}

fn sameIdentifier(tree: *const Tree, a: NodeIndex, b: NodeIndex) bool {
    return isNamed(tree, b, identifierName(tree, a) orelse return false);
}

fn shorthandStillValid(tree: *const Tree, key: NodeIndex, val: NodeIndex) bool {
    var v = val;
    if (tree.data(v) == .assignment_pattern) v = tree.data(v).assignment_pattern.left;
    return sameIdentifier(tree, key, v);
}

fn identifierName(tree: *const Tree, idx: NodeIndex) ?[]const u8 {
    if (idx == .null) return null;
    return tree.string(switch (tree.data(idx)) {
        inline .identifier_name, .identifier_reference, .binding_identifier => |id| id.name,
        else => return null,
    });
}

fn isNamed(tree: *const Tree, idx: NodeIndex, name: []const u8) bool {
    return std.mem.eql(u8, identifierName(tree, idx) orelse return false, name);
}

fn typeStartsWithLeftAngle(tree: *const Tree, idx: NodeIndex) bool {
    if (idx == .null) return false;
    return switch (tree.data(idx)) {
        .ts_function_type => |t| t.type_parameters != .null,
        .ts_constructor_type => |t| !t.abstract and t.type_parameters != .null,
        else => false,
    };
}

fn simpleStringKey(tree: *const Tree, idx: NodeIndex) ?[]const u8 {
    const lit = switch (tree.data(idx)) {
        .string_literal => |l| l,
        else => return null,
    };
    const s = tree.string(lit.value);
    return if (lexical.isIdentifierName(s)) s else null;
}

fn mappedSign(modifier: ast.TSMappedTypeModifier) []const u8 {
    return switch (modifier) {
        .plus => "+",
        .minus => "-",
        .none, .true => "",
    };
}

fn hasLineTerminator(text: []const u8) bool {
    for (0..text.len) |i| if (spec.line_terminators.lineBreakLen(text, i) > 0) return true;
    return false;
}

fn endsWithTsCast(tree: *const Tree, idx: NodeIndex) bool {
    var node = idx;
    for (0..tree.nodes.len) |_| {
        node = switch (tree.data(node)) {
            .ts_as_expression, .ts_satisfies_expression => return true,
            inline .binary_expression, .logical_expression, .assignment_expression => |e| e.right,
            .conditional_expression => |c| c.alternate,
            else => return false,
        };
    }
    unreachable;
}

// `x as T < y` would re-lex as the type arguments `T<y>`
fn binaryLeftPrecedence(tree: *const Tree, e: ast.BinaryExpression) u8 {
    if (e.operator == .exponent) return Precedence.Postfix;
    const op = e.operator.toString();
    if (op[0] == '<' and endsWithTsCast(tree, e.left)) return Precedence.Grouping;
    return e.operator.toToken().precedence();
}

// TypeScript reads `f<T>` as comparisons before `<`, `+`, `-`, or any operator starting with `>`
fn canFollowTypeArguments(operator: ast.BinaryOperator) bool {
    return switch (operator) {
        .less_than,
        .greater_than,
        .greater_than_or_equal,
        .right_shift,
        .unsigned_right_shift,
        .add,
        .subtract,
        => false,
        else => true,
    };
}

fn isChainLink(tag: NodeTag) bool {
    return switch (tag) {
        .binary_expression,
        .logical_expression,
        .member_expression,
        .call_expression,
        .tagged_template_expression,
        .chain_expression,
        .ts_non_null_expression,
        .ts_instantiation_expression,
        .ts_as_expression,
        .ts_satisfies_expression,
        .ts_array_type,
        .ts_indexed_access_type,
        .ts_jsdoc_nullable_type,
        .ts_jsdoc_non_nullable_type,
        .ts_qualified_name,
        .jsx_member_expression,
        => true,
        else => false,
    };
}

const handlers = blk: {
    const tags = std.enums.values(NodeTag);
    var table: [tags.len]*const fn (*Printer, NodeIndex, Ctx) void = undefined;
    for (tags) |tag| table[@backingInt(tag)] = Printer.Handler(tag).run;
    break :blk table;
};

fn isLeaf(comptime tag: NodeTag) bool {
    return fixedString(tag) != null or switch (tag) {
        .identifier_reference,
        .identifier_name,
        .label_identifier,
        .private_identifier,
        .jsx_identifier,
        .string_literal,
        .numeric_literal,
        .bigint_literal,
        .boolean_literal,
        .regexp_literal,
        .template_element,
        .empty_statement,
        .jsx_text,
        => true,
        else => false,
    };
}

fn precedenceOf(tag: NodeTag) u8 {
    return switch (tag) {
        .sequence_expression => Precedence.Comma,
        .assignment_expression,
        .arrow_function_expression,
        .yield_expression,
        .conditional_expression,
        => Precedence.Assignment,
        .unary_expression, .await_expression, .ts_type_assertion => Precedence.Unary,
        .update_expression => Precedence.Postfix,
        .ts_as_expression, .ts_satisfies_expression => Precedence.Relational,
        .new_expression,
        .call_expression,
        .member_expression,
        .chain_expression,
        .tagged_template_expression,
        .import_expression,
        .ts_non_null_expression,
        .ts_instantiation_expression,
        => Precedence.Call,
        // decided per node by `needsParens`
        .logical_expression, .binary_expression, .boolean_literal => Precedence.Lowest,
        .ts_function_type,
        .ts_constructor_type,
        .ts_conditional_type,
        .ts_infer_type,
        => TPrec.trailing,
        .ts_union_type => TPrec.@"union",
        .ts_intersection_type => TPrec.intersection,
        .ts_type_operator => TPrec.operator,
        else => Precedence.Grouping,
    };
}

test {
    _ = output;
    _ = sourcemap;
}
