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
const IndexRange = ast.IndexRange;
const Precedence = ast.Precedence;

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
    stack_bytes: usize = util.StackGuard.default_bytes,
};

const Diagnostic = ast.Diagnostic;

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

const Error = std.mem.Allocator.Error;

pub fn generate(allocator: Allocator, tree: *const Tree, options: Options) Error!Result {
    std.debug.assert(tree.root != .null);
    var p = try Printer.init(allocator, tree, options);
    defer p.deinit();
    p.emit(tree.root);
    if (p.out.failed) return error.OutOfMemory;
    std.debug.assert(p.owed == .null);
    std.debug.assert(p.defer_trailing_of == .null);
    std.debug.assert(p.links.items.len == 0);
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
    _: u14 = 0,
};

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
    decl_no_in: bool = false,
    definite_pending: bool = false,
    restricted: ?*bool = null,
    restricted_len: usize = 0,
    // the node whose trailing comments follow its parent's next token
    owed: NodeIndex = .null,

    stack: util.StackGuard,
    chain_stack: util.StackGuard,
    links: std.ArrayList(Link) = .empty,
    decoded: std.ArrayList(u8) = .empty,

    const DecoratorLayout = enum { own_line, same_line };

    const Link = struct {
        idx: NodeIndex,
        inner: Ctx,
        wrap: bool,
        defer_trailing: bool,
        comments: ?ast.CommentIterator,
    };

    const Head = struct { idx: NodeIndex, ctx: Ctx };
    const Self = @This();

    fn init(allocator: Allocator, tree: *const Tree, options: Options) Error!Self {
        return .{
            .allocator = allocator,
            .tree = tree,
            .node_data = tree.nodes.items(.data),
            .options = options,
            .comments_on = options.comments != .none and
                tree.comment_index.owners.items.len > 0,
            .out = try Output.init(
                allocator,
                options.format == .pretty,
                .{ .code = tree.source.len, .map = tree.nodes.len * 8 + 64 },
                options.source_map,
                tree.source,
            ),
            .stack = .init(options.stack_bytes),
            .chain_stack = .init(options.stack_bytes / 8),
        };
    }

    fn deinit(self: *Self) void {
        self.out.deinit();
        self.diagnostics.deinit(self.allocator);
        self.links.deinit(self.allocator);
        self.decoded.deinit(self.allocator);
    }

    inline fn pretty(self: *const Self) bool {
        return self.out.pretty;
    }

    inline fn nodeData(self: *const Self, idx: NodeIndex) NodeData {
        return self.node_data[@backingInt(idx)];
    }

    inline fn writeString(self: *Self, id: ast.String) void {
        self.out.writeToken(self.tree.string(id));
    }

    inline fn printSpaced(self: *Self, byte: u8) void {
        self.out.space();
        self.out.writeByte(byte);
        self.out.space();
    }

    inline fn printColon(self: *Self) void {
        self.out.writeByte(':');
        self.out.space();
    }

    fn emitList(self: *Self, items: IndexRange) void {
        self.emitItems(self.tree.extra(items), .null, .{});
    }

    fn emitItems(self: *Self, items: []const NodeIndex, rest: NodeIndex, ctx: Ctx) void {
        var item_ctx = ctx;
        item_ctx.defer_trailing = true;
        const depth = self.indent_depth;
        for (items, 0..) |x, i| {
            self.emitExpr(x, item_ctx);
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

    inline fn printBindingSuffix(
        self: *Self,
        optional: bool,
        definite: bool,
        annotation: NodeIndex,
    ) void {
        if (optional) self.out.writeByte('?');
        if (definite) self.out.writeByte('!');
        self.emit(annotation);
    }

    inline fn takeDefinite(self: *Self) bool {
        const d = self.definite_pending;
        self.definite_pending = false;
        return d;
    }

    fn newline(self: *Self) void {
        if (self.pretty()) self.breakLine();
    }

    fn hasPrintedComments(self: *const Self, idx: NodeIndex) bool {
        if (!self.comments_on) return false;
        var comments = self.tree.commentsOf(idx);
        while (comments.next()) |c| {
            const printed = switch (c.position) {
                .before => idx != self.skip_leading_of,
                .after => true,
                .inside => false,
            };
            if (printed and self.allowComment(c)) return true;
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
                if (!ctx.bare) {
                    const exhausted = if (comptime isLeaf(tag))
                        false
                    else if (comptime isChainLink(tag))
                        self.chain_stack.exhausted() and self.stack.exhausted()
                    else
                        self.stack.exhausted();
                    if (exhausted) return self.diagnose(idx, "Code is nested too deeply to print");
                    const wrap = self.needsParens(tag, node, ctx);
                    if (wrap or self.hasComments(idx)) return self.emitDressed(idx, ctx, wrap);
                }
                var inner = ctx;
                inner.defer_trailing = false;
                inner.bare = false;
                self.recordMapping(idx);

                if (comptime isChainLink(tag)) return self.emitLink(tag, node, inner);
                if (comptime fixedString(tag)) |text| return self.out.writeStr(text);
                if (comptime emittedByParent(tag)) {
                    std.debug.panic("codegen: {s} is emitted by its parent", .{@tagName(tag)});
                }
                const emitter = @field(Self, "emit_" ++ @tagName(tag));
                const params = @typeInfo(@TypeOf(emitter)).@"fn".param_types;
                const with_idx = params[1].? == NodeIndex;
                const with_ctx = params[params.len - 1].? == Ctx;
                const args = .{self} ++ (if (with_idx) .{idx} else .{}) ++ .{node} ++
                    (if (with_ctx) .{inner} else .{});
                return @call(.always_inline, emitter, args);
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

    fn writeNodeQuoted(
        self: *Self,
        idx: NodeIndex,
        quote: u8,
        value: []const u8,
        comptime references: []const u8,
    ) void {
        std.debug.assert(quote == '"' or quote == '\'');
        const scope = self.openComments(idx);
        self.recordMapping(idx);
        self.out.writeByte(quote);
        self.writeCharReferences(value, references);
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
        std.debug.assert(start <= text.len);
        self.out.writeRawStr(text[start..]);
    }

    fn jsxDecodesTo(self: *Self, raw: []const u8, value: []const u8) bool {
        if (std.mem.findScalar(u8, raw, '&') == null) return std.mem.eql(u8, raw, value);
        self.decoded.clearRetainingCapacity();
        lexical.decodeJSXEntities(self.allocator, raw, &self.decoded) catch {
            self.out.failed = true;
            return false;
        };
        std.debug.assert(self.decoded.items.len <= raw.len);
        return std.mem.eql(u8, self.decoded.items, value);
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
        if (self.chain_stack.exhausted()) {
            self.emitChainIteratively(head);
        } else {
            self.emitExpr(head.idx, head.ctx);
        }
        self.emitLinkSuffix(tag, node, ctx);
    }

    noinline fn emitChainIteratively(self: *Self, head: Head) void {
        if (head.idx == .null or !isChainLink(self.tagOf(head.idx))) {
            return self.emitExpr(head.idx, head.ctx);
        }

        const base = self.links.items.len;
        var link = self.openLink(head.idx, head.ctx);
        while (true) {
            const next_head = self.linkHeadOf(link.idx, link.inner);
            if (next_head.idx == .null or !isChainLink(self.tagOf(next_head.idx))) {
                self.emitExpr(next_head.idx, next_head.ctx);
                break;
            }
            std.debug.assert(self.links.items.len - base < self.tree.nodes.len);
            self.links.append(self.allocator, link) catch {
                self.out.failed = true;
                return;
            };
            link = self.openLink(next_head.idx, next_head.ctx);
        }
        while (true) {
            self.emitLinkSuffixOf(link);
            self.closeLink(link);
            if (self.links.items.len == base) break;
            link = self.links.pop().?;
        }
    }

    fn openLink(self: *Self, idx: NodeIndex, ctx: Ctx) Link {
        const wrap = switch (self.node_data[@backingInt(idx)]) {
            inline else => |*node, tag| if (comptime isChainLink(tag))
                self.needsParens(tag, node, ctx)
            else
                unreachable,
        };
        if (wrap) self.out.writeByte('(');
        const comments = self.openComments(idx);
        self.recordMapping(idx);
        var inner: Ctx = if (wrap) .{} else ctx;
        inner.defer_trailing = false;
        return .{
            .idx = idx,
            .inner = inner,
            .wrap = wrap,
            .defer_trailing = ctx.defer_trailing,
            .comments = comments,
        };
    }

    fn closeLink(self: *Self, link: Link) void {
        self.closeComments(link.idx, link.comments, link.defer_trailing);
        if (link.wrap) self.out.writeByte(')');
    }

    inline fn tagOf(self: *const Self, idx: NodeIndex) NodeTag {
        return std.meta.activeTag(self.node_data[@backingInt(idx)]);
    }

    fn linkHeadOf(self: *const Self, idx: NodeIndex, ctx: Ctx) Head {
        return switch (self.node_data[@backingInt(idx)]) {
            inline else => |*node, tag| if (comptime isChainLink(tag))
                self.linkHead(tag, node, ctx)
            else
                unreachable,
        };
    }

    fn emitLinkSuffixOf(self: *Self, link: Link) void {
        switch (self.node_data[@backingInt(link.idx)]) {
            inline else => |*node, tag| if (comptime isChainLink(tag)) {
                self.emitLinkSuffix(tag, node, link.inner);
            } else unreachable,
        }
    }

    inline fn linkHead(self: *const Self, comptime tag: NodeTag, node: anytype, ctx: Ctx) Head {
        return switch (tag) {
            .binary_expression => .{
                .idx = node.left,
                .ctx = .{
                    .prec = binaryLeftPrecedence(self.tree, node.*),
                    .no_in = ctx.no_in,
                    .no_instantiation = !canFollowTypeArguments(node.operator),
                },
            },
            .logical_expression => .{
                .idx = node.left,
                .ctx = .{
                    .prec = self.logicalOperandPrecedence(
                        node.left,
                        node.operator.toToken().precedence(),
                        node.operator,
                    ),
                    .no_in = ctx.no_in,
                },
            },
            // TypeScript rejects `f<T>?.x`, and minify prints `?.["x"]` as `?.x`
            .member_expression => .{
                .idx = node.object,
                .ctx = .{
                    .prec = Precedence.Call,
                    .no_call = ctx.no_call,
                    .no_instantiation = true,
                },
            },
            // `f<T>?.()` keeps its type arguments where `f<T>()` takes them as the call's own
            .call_expression => .{
                .idx = node.callee,
                .ctx = .{ .prec = Precedence.Call, .no_instantiation = !node.optional },
            },
            .tagged_template_expression => .{
                .idx = node.tag,
                .ctx = .{
                    .prec = Precedence.Call,
                    .no_call = ctx.no_call,
                    .no_instantiation = true,
                },
            },
            .chain_expression => .{ .idx = node.expression, .ctx = ctx },
            .ts_non_null_expression => .{
                .idx = node.expression,
                .ctx = .{
                    .prec = Precedence.Postfix,
                    .no_instantiation = true,
                    .no_chain = true,
                },
            },
            .ts_instantiation_expression => .{
                .idx = node.expression,
                .ctx = .{ .prec = Precedence.Postfix, .no_instantiation = true },
            },
            .ts_as_expression, .ts_satisfies_expression => .{
                .idx = node.expression,
                .ctx = .{
                    .prec = Precedence.Relational,
                    .no_in = ctx.no_in,
                    .defer_trailing = true,
                },
            },
            else => @compileError("not a chain link: " ++ @tagName(tag)),
        };
    }

    inline fn emitLinkSuffix(
        self: *Self,
        comptime tag: NodeTag,
        node: anytype,
        ctx: Ctx,
    ) void {
        switch (tag) {
            .binary_expression => {
                const op = node.operator.toString();
                const p: u8 = node.operator.toToken().precedence();
                const right_min = if (node.operator == .exponent) p else p + 1;
                if (utils.isWordOp(op)) {
                    self.out.writeByte(' ');
                    self.out.writeStr(op);
                    self.out.writeByte(' ');
                } else {
                    self.out.space();
                    // `f<T>==x` would re-lex the type argument closer into `>=`
                    if (self.out.lastByte() == '>' and (op[0] == '>' or op[0] == '=')) {
                        self.out.writeByte(' ');
                    }
                    self.out.writeStr(op);
                    self.out.space();
                }
                self.emitExpr(node.right, .{
                    .prec = right_min,
                    .no_in = ctx.no_in,
                    .no_instantiation = ctx.no_instantiation,
                });
            },
            .logical_expression => {
                const p: u8 = node.operator.toToken().precedence();
                self.out.space();
                self.out.writeStr(node.operator.toString());
                self.out.space();
                const prec = self.logicalOperandPrecedence(node.right, p + 1, node.operator);
                self.emitExpr(node.right, .{ .prec = prec, .no_in = ctx.no_in });
            },
            .member_expression => {
                const static_key = if (self.options.minify and node.computed)
                    simpleStringKey(self.tree, node.property)
                else
                    null;

                if (node.computed and static_key == null) {
                    if (node.optional) self.out.writeStr("?.");
                    self.out.writeByte('[');
                    self.emit(node.property);
                    self.out.writeByte(']');
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
                if (node.optional) self.out.writeStr("?.");
                self.emit(node.type_arguments);
                self.printArgList(node.arguments);
            },
            .tagged_template_expression => {
                self.emit(node.type_arguments);
                self.emitExpr(node.quasi, .{ .tagged = true });
            },
            .chain_expression => {},
            .ts_non_null_expression => self.out.writeByte('!'),
            .ts_instantiation_expression => self.emit(node.type_arguments),
            .ts_as_expression => {
                self.writeKeywordThenOwed(" as");
                self.emit(node.type_annotation);
            },
            .ts_satisfies_expression => {
                self.writeKeywordThenOwed(" satisfies");
                self.emit(node.type_annotation);
            },
            else => @compileError("not a chain link: " ++ @tagName(tag)),
        }
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
        const fixed = comptime node_precedence[@backingInt(tag)];
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
        if (!self.comments_on) return;
        if (key == .null) return;
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
            if (self.pretty() and last != 0 and last != ' ' and last != '\n') {
                self.out.writeByte(' ');
            }
            self.writeCommentBody(c);
            if (self.pretty()) self.out.writeByte(' ');
        } else {
            self.breakLine();
            self.writeCommentBody(c);
            self.breakLine();
        }
        // a comment is not the operand's first token
        if (self.restricted != null) self.restricted_len = self.out.len();
    }

    fn writeTrailing(self: *Self, c: ast.Comment) void {
        if (c.same_line) {
            if (self.pretty()) self.out.writeByte(' ');
            self.writeCommentBody(c);
            if (c.type == .line) self.breakLine();
            return;
        }
        self.breakLine();
        self.writeCommentBody(c);
        self.breakLine();
    }

    inline fn writeCommentBody(self: *Self, c: ast.Comment) void {
        const value = self.tree.string(c.value);
        self.out.writeStr(if (c.type == .line) "//" else "/*");
        if (c.type == .block) {
            self.writeBlockBody(value);
            self.out.writeComment("*/");
        } else {
            self.out.writeComment(value);
        }
    }

    fn writeBlockBody(self: *Self, value: []const u8) void {
        if (!self.pretty() or !utils.isJsdocBody(value)) {
            self.out.writeComment(value);
            return;
        }
        var it = std.mem.splitScalar(u8, value, '\n');
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

    fn emit_program(self: *Self, idx: NodeIndex, p: *const ast.Program) void {
        if (p.hashbang) |h| {
            self.out.writeStr("#!");
            self.out.writeRawStr(self.tree.string(h.value));
            self.out.writeByte('\n');
        }
        self.printStmtList(self.tree.extra(p.body), true);
        self.pending_semi = false;
        if (self.comments_on) self.emitInsideComments(idx);
    }

    fn printStmtList(self: *Self, items: []const NodeIndex, prologue: bool) void {
        var first = true;
        var prol = prologue;
        for (items) |s| {
            if (!first) self.newline();
            self.flushSemi();
            self.in_prologue = prol;
            self.emit(s);
            first = false;
            if (prol and self.nodeData(s) != .directive) prol = false;
        }
        self.in_prologue = false;
    }

    fn printIndentedStmtList(self: *Self, items: []const NodeIndex, prologue: bool) void {
        std.debug.assert(items.len > 0);
        self.indent_depth += 1;
        defer self.indent_depth -= 1;
        self.newline();
        self.printStmtList(items, prologue);
    }

    fn printBlock(self: *Self, idx: NodeIndex, items: []const NodeIndex, prologue: bool) void {
        self.out.writeByte('{');
        if (items.len > 0) {
            self.printIndentedStmtList(items, prologue);
            self.pending_semi = false;
            self.newline();
        } else if (self.comments_on) {
            self.emitInsideComments(idx);
        }
        self.out.writeByte('}');
    }

    fn emitInsideComments(self: *Self, idx: NodeIndex) void {
        var any = false;
        var comments = self.tree.commentsOf(idx);
        while (comments.next()) |c| {
            if (c.position != .inside or !self.allowComment(c)) continue;
            if (!any) {
                any = true;
                self.indent_depth += 1;
            }
            self.breakLine();
            self.writeCommentBody(c);
        }
        if (any) {
            self.indent_depth -= 1;
            self.breakLine();
        }
    }

    inline fn emitInsideCommentsInline(self: *Self, idx: NodeIndex) void {
        if (self.hasComments(idx)) self.emitInsideCommentsFlat(idx);
    }

    // a line comment breaks the node open like a block
    fn emitInsideCommentsFlat(self: *Self, idx: NodeIndex) void {
        var scan = self.tree.commentsOf(idx);
        var any = false;
        while (scan.next()) |c| {
            if (c.position != .inside or !self.allowComment(c)) continue;
            if (c.type == .line) return self.emitInsideComments(idx);
            any = true;
        }
        if (!any) return;
        var comments = self.tree.commentsOf(idx);
        while (comments.next()) |c| {
            if (c.position != .inside or !self.allowComment(c)) continue;
            if (self.pretty() and needsSpaceBeforeInlineComment(self.out.lastByte())) {
                self.out.writeByte(' ');
            }
            self.writeCommentBody(c);
        }
    }

    // compact mode defers `;` so a closing `}` can drop it
    inline fn softSemi(self: *Self) void {
        if (self.pretty()) self.out.writeByte(';') else self.pending_semi = true;
    }

    inline fn flushSemi(self: *Self) void {
        if (self.pending_semi) {
            self.pending_semi = false;
            self.out.writeByte(';');
        }
    }

    fn emit_block_statement(self: *Self, idx: NodeIndex, s: *const ast.BlockStatement) void {
        self.printBlock(idx, self.tree.extra(s.body), false);
    }

    fn emit_function_body(self: *Self, idx: NodeIndex, b: *const ast.FunctionBody) void {
        self.printBlock(idx, self.tree.extra(b.body), true);
    }

    fn emit_static_block(self: *Self, idx: NodeIndex, b: *const ast.StaticBlock) void {
        self.out.writeStr("static");
        self.out.space();
        self.printBlock(idx, self.tree.extra(b.body), false);
    }

    fn emit_directive(self: *Self, d: *const ast.Directive) void {
        // an escaped `"use strict"` does not enable strict mode, so the raw lexeme must survive
        switch (self.nodeData(d.expression)) {
            .string_literal => |lit| {
                const raw = self.tree.string(lit.raw);
                const text = self.tree.string(d.value);
                if (raw.len >= 2 and std.mem.eql(u8, raw[1 .. raw.len - 1], text)) {
                    self.writeNodeText(d.expression, raw);
                } else {
                    const quote: u8 = if (std.mem.findScalar(u8, text, '"') == null) '"' else '\'';
                    self.writeNodeQuoted(d.expression, quote, text, "");
                }
            },
            else => self.emit(d.expression),
        }
        self.softSemi();
    }

    fn emit_empty_statement(self: *Self, _: *const ast.EmptyStatement) void {
        // not deferred, `if(x);` needs the `;` to materialize the body
        self.out.writeByte(';');
    }

    fn emit_debugger_statement(self: *Self, idx: NodeIndex, _: *const ast.DebuggerStatement) void {
        self.out.writeStr("debugger");
        self.emitInsideCommentsInline(idx);
        self.softSemi();
    }

    fn emit_expression_statement(self: *Self, s: *const ast.ExpressionStatement) void {
        const as_directive = self.in_prologue and self.nodeData(s.expression) == .string_literal;
        self.in_prologue = false;
        self.out.lead = .stmt;
        self.wrapIf(as_directive, s.expression, .{});
        self.softSemi();
    }

    fn printParenHead(self: *Self, keyword: []const u8, head: NodeIndex) void {
        self.out.writeStr(keyword);
        self.out.space();
        self.out.writeByte('(');
        self.emit(head);
        self.out.writeByte(')');
        self.out.space();
    }

    fn emit_if_statement(self: *Self, s: *const ast.IfStatement) void {
        self.printParenHead("if", s.@"test");
        self.emit(s.consequent);
        if (s.alternate != .null) {
            self.flushSemi();
            self.out.space();
            self.out.writeStr("else ");
            self.emit(s.alternate);
        }
    }

    fn emit_return_statement(self: *Self, idx: NodeIndex, s: *const ast.ReturnStatement) void {
        self.out.writeStr("return");
        self.emitInsideCommentsInline(idx);
        self.emitRestrictedArg(s.argument, .{});
        self.softSemi();
    }

    fn emit_throw_statement(self: *Self, s: *const ast.ThrowStatement) void {
        self.out.writeStr("throw");
        self.emitRestrictedArg(s.argument, .{});
        self.softSemi();
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

    fn emit_break_statement(self: *Self, idx: NodeIndex, s: *const ast.BreakStatement) void {
        self.printJump(idx, "break", s.label);
    }

    fn emit_continue_statement(self: *Self, idx: NodeIndex, s: *const ast.ContinueStatement) void {
        self.printJump(idx, "continue", s.label);
    }

    fn printJump(self: *Self, idx: NodeIndex, keyword: []const u8, label: NodeIndex) void {
        self.out.writeStr(keyword);
        self.emitInsideCommentsInline(idx);
        if (label != .null) {
            self.out.writeByte(' ');
            self.emit(label);
        }
        self.softSemi();
    }

    fn emit_labeled_statement(self: *Self, s: *const ast.LabeledStatement) void {
        self.emit(s.label);
        self.printColon();
        self.emit(s.body);
    }

    fn emit_with_statement(self: *Self, s: *const ast.WithStatement) void {
        self.printParenHead("with", s.object);
        self.emit(s.body);
    }

    fn emit_while_statement(self: *Self, s: *const ast.WhileStatement) void {
        self.printParenHead("while", s.@"test");
        self.emit(s.body);
    }

    fn emit_do_while_statement(self: *Self, s: *const ast.DoWhileStatement) void {
        self.out.writeStr("do ");
        self.emit(s.body);
        self.flushSemi();
        self.out.space();
        self.out.writeStr("while");
        self.out.space();
        self.out.writeByte('(');
        self.emit(s.@"test");
        self.out.writeStr(");");
    }

    fn emit_for_statement(self: *Self, s: *const ast.ForStatement) void {
        self.out.writeStr("for");
        self.out.space();
        self.out.writeByte('(');
        if (s.init != .null) switch (self.nodeData(s.init)) {
            .variable_declaration => |d| self.printForDeclaration(s.init, d, true),
            else => self.emitExpr(s.init, .{ .no_in = true }),
        };
        self.out.writeByte(';');
        if (s.@"test" != .null) {
            self.out.space();
            self.emit(s.@"test");
        }
        self.out.writeByte(';');
        if (s.update != .null) {
            self.out.space();
            self.emit(s.update);
        }
        self.out.writeByte(')');
        self.out.space();
        self.emit(s.body);
    }

    fn emit_for_in_statement(self: *Self, s: *const ast.ForInStatement) void {
        self.out.writeStr("for");
        self.out.space();
        self.out.writeByte('(');
        self.printForLeft(s.left);
        self.out.writeStr(" in ");
        self.emit(s.right);
        self.out.writeByte(')');
        self.out.space();
        self.emit(s.body);
    }

    fn emit_for_of_statement(self: *Self, s: *const ast.ForOfStatement) void {
        self.out.writeStr("for");
        if (s.await) self.out.writeStr(" await");
        self.out.space();
        self.out.writeByte('(');
        // `for (async of …)` is forbidden, it could start the arrow `async of => …`
        const wrap_async = !s.await and isNamed(self.tree, s.left, "async");
        if (wrap_async) self.out.writeByte('(');
        self.printForLeft(s.left);
        if (wrap_async) self.out.writeByte(')');
        self.out.writeStr(" of ");
        self.emitValue(s.right);
        self.out.writeByte(')');
        self.out.space();
        self.emit(s.body);
    }

    fn printForLeft(self: *Self, idx: NodeIndex) void {
        switch (self.nodeData(idx)) {
            .variable_declaration => |d| self.printForDeclaration(idx, d, false),
            else => self.emitAssignTarget(idx, .{}),
        }
    }

    fn printForDeclaration(
        self: *Self,
        idx: NodeIndex,
        d: ast.VariableDeclaration,
        no_in: bool,
    ) void {
        const scope = self.openComments(idx);
        self.printVariableDecl(d, false, no_in);
        self.closeComments(idx, scope, false);
    }

    fn emit_switch_statement(self: *Self, s: *const ast.SwitchStatement) void {
        self.printParenHead("switch", s.discriminant);
        self.out.writeByte('{');
        if (s.cases.len > 0) {
            for (self.tree.extra(s.cases)) |c| {
                self.flushSemi();
                self.newline();
                self.emit(c);
            }
            self.pending_semi = false;
            self.newline();
        }
        self.out.writeByte('}');
    }

    fn emit_switch_case(self: *Self, idx: NodeIndex, c: *const ast.SwitchCase) void {
        if (c.@"test" != .null) {
            self.out.writeStr("case ");
            self.emit(c.@"test");
        } else {
            self.out.writeStr("default");
            self.emitInsideCommentsInline(idx);
        }
        self.out.writeByte(':');
        if (c.consequent.len == 0) return;
        self.printIndentedStmtList(self.tree.extra(c.consequent), false);
    }

    fn emit_try_statement(self: *Self, s: *const ast.TryStatement) void {
        self.out.writeStr("try ");
        self.emit(s.block);
        if (s.handler != .null) {
            self.out.space();
            self.emit(s.handler);
        }
        if (s.finalizer != .null) {
            self.out.space();
            self.out.writeStr("finally ");
            self.emit(s.finalizer);
        }
    }

    fn emit_catch_clause(self: *Self, c: *const ast.CatchClause) void {
        self.out.writeStr("catch");
        if (c.param != .null) {
            self.out.space();
            self.out.writeByte('(');
            self.emit(c.param);
            self.out.writeByte(')');
        }
        self.out.space();
        self.emit(c.body);
    }

    fn emit_variable_declaration(self: *Self, d: *const ast.VariableDeclaration) void {
        self.printVariableDecl(d.*, true, false);
    }

    fn printVariableDecl(
        self: *Self,
        d: ast.VariableDeclaration,
        with_semicolon: bool,
        no_in: bool,
    ) void {
        if (d.declare) self.out.writeStr("declare ");
        self.out.writeStr(d.kind.toString());
        self.out.writeByte(' ');
        const prev = self.decl_no_in;
        self.decl_no_in = no_in;
        defer self.decl_no_in = prev;
        self.emitList(d.declarators);
        if (with_semicolon) self.softSemi();
    }

    fn emit_variable_declarator(self: *Self, d: *const ast.VariableDeclarator) void {
        self.definite_pending = d.definite;
        self.emit(d.id);
        self.definite_pending = false;
        if (d.init != .null) {
            self.printSpaced('=');
            self.emitExpr(d.init, .{ .prec = Precedence.Assignment, .no_in = self.decl_no_in });
        }
    }

    fn emit_sequence_expression(
        self: *Self,
        e: *const ast.SequenceExpression,
        ctx: Ctx,
    ) void {
        const expression_ctx: Ctx = .{ .prec = Precedence.Assignment, .no_in = ctx.no_in };
        self.emitItems(self.tree.extra(e.expressions), .null, expression_ctx);
    }

    fn emit_parenthesized_expression(
        self: *Self,
        e: *const ast.ParenthesizedExpression,
    ) void {
        self.out.writeByte('(');
        self.emit(e.expression);
        self.out.writeByte(')');
    }

    // `??` mixed with `&&` or `||` must be parenthesized
    fn logicalOperandPrecedence(
        self: *const Self,
        child: NodeIndex,
        min: u8,
        parent: ast.LogicalOperator,
    ) u8 {
        const nullish = parent == .nullish_coalescing;
        return switch (self.nodeData(child)) {
            .logical_expression => |l| if (nullish != (l.operator == .nullish_coalescing))
                Precedence.Grouping
            else
                min,
            else => min,
        };
    }

    fn emit_conditional_expression(
        self: *Self,
        e: *const ast.ConditionalExpression,
        ctx: Ctx,
    ) void {
        self.emitExpr(e.@"test", .{ .prec = Precedence.LogicalOr, .no_in = ctx.no_in });
        self.printSpaced('?');
        self.emitExpr(e.consequent, .{ .prec = Precedence.Assignment });
        self.printSpaced(':');
        self.emitExpr(e.alternate, .{ .prec = Precedence.Assignment, .no_in = ctx.no_in });
    }

    fn emit_unary_expression(self: *Self, e: *const ast.UnaryExpression, ctx: Ctx) void {
        const op = e.operator.toString();
        self.out.writeStr(op);
        if (utils.isWordOp(op)) self.out.writeByte(' ');
        self.emitExpr(e.argument, .{
            .prec = Precedence.Unary,
            .no_instantiation = ctx.no_instantiation,
        });
    }

    fn emit_update_expression(self: *Self, e: *const ast.UpdateExpression) void {
        const op = e.operator.toString();
        if (e.prefix) {
            self.out.writeStr(op);
            self.emitAssignTarget(e.argument, .{});
        } else {
            self.emitAssignTarget(e.argument, .{ .postfix_operand = true });
            self.out.writeStr(op);
        }
    }

    fn emit_assignment_expression(
        self: *Self,
        e: *const ast.AssignmentExpression,
        ctx: Ctx,
    ) void {
        self.emitAssignTarget(e.left, .{});
        self.out.space();
        self.out.writeStr(e.operator.toString());
        self.out.space();
        self.emitExpr(e.right, .{ .prec = Precedence.Assignment, .no_in = ctx.no_in });
    }

    fn emitAssignTarget(self: *Self, idx: NodeIndex, ctx: Ctx) void {
        if (idx == .null) return;
        const prev = self.in_assign_target;
        defer self.in_assign_target = prev;

        const wrap = needsParensAsAssignTarget(self.tree, idx) or
            (ctx.postfix_operand and self.breaksLineAfter(idx));
        self.in_assign_target = !wrap;
        self.wrapIf(wrap, idx, ctx);
    }

    inline fn emitChildOfAssignTarget(self: *Self, idx: NodeIndex) void {
        if (self.in_assign_target) self.emitAssignTarget(idx, .{}) else self.emitValue(idx);
    }

    inline fn emitValue(self: *Self, idx: NodeIndex) void {
        self.emitExpr(idx, .{ .prec = Precedence.Assignment });
    }

    fn emit_array_expression(self: *Self, idx: NodeIndex, e: *const ast.ArrayExpression) void {
        const in_target = self.in_assign_target;
        self.in_assign_target = false;
        defer self.in_assign_target = in_target;

        self.out.writeByte('[');
        self.emitInsideCommentsInline(idx);
        const list = self.tree.extra(e.elements);
        const depth = self.indent_depth;
        for (list, 0..) |x, i| {
            if (in_target) {
                self.emitAssignTarget(x, .{ .defer_trailing = true });
            } else {
                self.emitExpr(x, .{ .prec = Precedence.Assignment, .defer_trailing = true });
            }
            self.closeItem(i + 1 < list.len, depth);
        }
        self.closeList(depth);
        self.closeArray(list, .null);
    }

    // a trailing hole needs its own comma, else `[a,]` is one element
    fn closeArray(self: *Self, elements: []const NodeIndex, rest: NodeIndex) void {
        const hole = rest == .null and elements.len > 0 and elements[elements.len - 1] == .null;
        if (hole) self.out.writeByte(',');
        self.out.writeByte(']');
    }

    fn emit_object_expression(self: *Self, idx: NodeIndex, e: *const ast.ObjectExpression) void {
        const in_target = self.in_assign_target;
        defer self.in_assign_target = in_target;

        self.out.writeByte('{');
        self.emitInsideCommentsInline(idx);
        const list = self.tree.extra(e.properties);
        if (list.len > 0) {
            self.out.space();
            const depth = self.indent_depth;
            for (list, 0..) |x, i| {
                self.in_assign_target = in_target;
                self.emitExpr(x, .{ .defer_trailing = true });
                self.closeItem(i + 1 < list.len, depth);
            }
            self.closeList(depth);
            if (!self.out.atLineStart()) self.out.space();
        }
        self.out.writeByte('}');
    }

    fn emit_object_property(self: *Self, p: *const ast.ObjectProperty) void {
        const is_method = p.method or p.kind == .get or p.kind == .set;
        if (is_method and self.nodeData(p.value) == .function) {
            const fn_data = self.nodeData(p.value).function;
            switch (p.kind) {
                .get => self.out.writeStr("get "),
                .set => self.out.writeStr("set "),
                .init => {
                    if (fn_data.async) self.out.writeStr("async ");
                    if (fn_data.generator) self.out.writeByte('*');
                },
            }
            self.printObjectKey(p.key, p.computed);
            self.emitMethodValue(p.value);
            return;
        }

        if (p.shorthand and shorthandStillValid(self.tree, p.key, p.value)) {
            self.emitChildOfAssignTarget(p.value);
            return;
        }

        self.printObjectKey(p.key, p.computed);
        self.printColon();
        self.emitChildOfAssignTarget(p.value);
    }

    fn emit_spread_element(self: *Self, s: *const ast.SpreadElement) void {
        self.out.writeStr("...");
        self.emitValue(s.argument);
    }

    fn emit_new_expression(self: *Self, e: *const ast.NewExpression) void {
        self.out.writeStr("new ");
        self.emitExpr(e.callee, .{
            .prec = Precedence.New,
            .no_call = true,
            .no_instantiation = true,
        });
        self.emit(e.type_arguments);
        self.printArgList(e.arguments);
    }

    fn emit_await_expression(self: *Self, e: *const ast.AwaitExpression, ctx: Ctx) void {
        self.out.writeStr("await ");
        self.emitExpr(e.argument, .{
            .prec = Precedence.Unary,
            .no_instantiation = ctx.no_instantiation,
        });
    }

    fn emit_yield_expression(self: *Self, e: *const ast.YieldExpression, ctx: Ctx) void {
        self.out.writeStr("yield");
        if (e.delegate) self.out.writeByte('*');
        self.emitRestrictedArg(e.argument, .{
            .prec = Precedence.Assignment,
            .no_in = ctx.no_in,
        });
    }

    fn emit_meta_property(self: *Self, p: *const ast.MetaProperty) void {
        self.emit(p.meta);
        self.out.writeByte('.');
        self.emit(p.property);
    }

    fn printArgList(self: *Self, args: IndexRange) void {
        self.out.writeByte('(');
        self.emitItems(self.tree.extra(args), .null, .{ .prec = Precedence.Assignment });
        self.out.writeByte(')');
    }

    fn emit_string_literal(self: *Self, lit: *const ast.StringLiteral) void {
        const value = self.tree.string(lit.value);
        const q = self.pickQuote(value, lit.raw);
        if (plainRunLength(value, 0, q, self.options.minify) == value.len) {
            return self.out.writeQuoted(q, value);
        }
        self.emitEscapedString(value, q);
    }

    noinline fn emitEscapedString(self: *Self, value: []const u8, q: u8) void {
        self.out.writeByte(q);
        self.writeEscapedString(value, q);
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
        var singles: usize = 0;
        var doubles: usize = 0;
        for (s) |c| {
            singles += @intFromBool(c == '\'');
            doubles += @intFromBool(c == '"');
        }
        return if (singles < doubles) '\'' else '"';
    }

    fn writeEscapedString(self: *Self, s: []const u8, quote: u8) void {
        var start: usize = 0;
        var i: usize = 0;
        while (i < s.len) : (i += 1) {
            i += plainRunLength(s, i, quote, self.options.minify);
            if (i == s.len) break;
            const c = s[i];
            if (c >= 0x80) {
                if (c == 0xED) {
                    if (util.Utf.loneSurrogateAt(s, i)) |cp| {
                        if (i > start) self.out.writeRawStr(s[start..i]);
                        self.writeUnicodeEscape(cp);
                        i += 2;
                        start = i + 1;
                    }
                }
                continue;
            }
            const script_escape = if (self.options.minify) utils.scriptEscape(s, i) else null;
            const esc: ?[]const u8 = script_escape orelse switch (c) {
                '\\' => "\\\\",
                '\n' => "\\n",
                '\r' => "\\r",
                '\t' => "\\t",
                0x08 => "\\b",
                0x0C => "\\f",
                0x0B => "\\v",
                0 => if (i + 1 < s.len and std.ascii.isDigit(s[i + 1])) "\\x00" else "\\0",
                else => if (c == quote) (if (quote == '"') "\\\"" else "\\'") else null,
            };
            if (esc) |e| {
                if (i > start) self.out.writeRawStr(s[start..i]);
                self.out.writeRawStr(e);
                start = i + 1;
            }
        }
        if (start < s.len) self.out.writeRawStr(s[start..]);
    }

    inline fn plainRunLength(s: []const u8, from: usize, quote: u8, minify: bool) usize {
        std.debug.assert(from <= s.len);
        var i = from;
        while (i < s.len) {
            const chunk = if (s.len - i >= 16) Simd.loadChunk(s, i) else Simd.loadTail(s[i..]);
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

    fn writeUnicodeEscape(self: *Self, cp: u32) void {
        const hex = "0123456789abcdef";
        const buf = [_]u8{
            '\\',                  'u',
            hex[(cp >> 12) & 0xF], hex[(cp >> 8) & 0xF],
            hex[(cp >> 4) & 0xF],  hex[cp & 0xF],
        };
        self.out.writeRawStr(&buf);
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

    fn emit_bigint_literal(self: *Self, lit: *const ast.BigIntLiteral) void {
        self.writeString(lit.raw);
        self.out.writeByte('n');
    }

    fn emit_boolean_literal(self: *Self, lit: *const ast.BooleanLiteral) void {
        if (self.options.minify) {
            self.out.writeStr(if (lit.value) "!0" else "!1");
        } else {
            self.out.writeStr(if (lit.value) "true" else "false");
        }
    }

    fn emit_regexp_literal(self: *Self, lit: *const ast.RegExpLiteral) void {
        const pattern = self.tree.string(lit.pattern);
        self.out.writeByte('/');
        self.out.writeRawStr(if (pattern.len != 0) pattern else "(?:)");
        self.out.writeRawByte('/');
        self.out.writeRawStr(self.tree.string(lit.flags));
    }

    fn emit_template_element(self: *Self, el: *const ast.TemplateElement) void {
        self.printTemplateElement(el.*, false);
    }

    fn emit_template_literal(self: *Self, lit: *const ast.TemplateLiteral, ctx: Ctx) void {
        self.printTemplate(lit.quasis, lit.expressions, ctx.tagged);
    }

    fn printTemplate(self: *Self, quasis: IndexRange, subs: IndexRange, tagged: bool) void {
        self.out.writeByte('`');
        const xs = self.tree.extra(subs);
        for (self.tree.extra(quasis), 0..) |q, i| {
            self.printTemplateElement(self.tree.data(q).template_element, tagged);
            if (i < xs.len) {
                self.out.writeRawStr("${");
                self.emit(xs[i]);
                self.out.writeByte('}');
            }
        }
        self.out.writeRawByte('`');
    }

    fn printTemplateElement(self: *Self, el: ast.TemplateElement, tagged: bool) void {
        const script_safe = self.options.minify and !tagged;
        const raw = self.tree.string(el.raw);
        if (raw.len != 0) return self.writeTemplateRaw(raw, script_safe);
        const s = self.tree.string(el.cooked);
        var i: usize = 0;
        var start: usize = 0;
        while (i < s.len) : (i += 1) {
            const c = s[i];
            if (util.Utf.loneSurrogateAt(s, i)) |cp| {
                if (i > start) self.out.writeRawStr(s[start..i]);
                self.writeUnicodeEscape(cp);
                i += 2;
                start = i + 1;
                continue;
            }
            const script_escape = if (script_safe) utils.scriptEscape(s, i) else null;
            const esc: ?[]const u8 = script_escape orelse switch (c) {
                '\\' => "\\\\",
                '`' => "\\`",
                '$' => if (i + 1 < s.len and s[i + 1] == '{') "\\$" else null,
                '\r' => "\\r",
                0 => if (i + 1 < s.len and std.ascii.isDigit(s[i + 1])) "\\x00" else "\\0",
                else => null,
            };
            if (esc) |e| {
                if (i > start) self.out.writeRawStr(s[start..i]);
                self.out.writeRawStr(e);
                start = i + 1;
            }
        }
        if (start < s.len) self.out.writeRawStr(s[start..]);
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

    fn emit_identifier_reference(self: *Self, id: *const ast.IdentifierReference) void {
        self.writeString(id.name);
    }

    fn emit_identifier_name(self: *Self, id: *const ast.IdentifierName) void {
        self.writeString(id.name);
    }

    fn emit_binding_identifier(self: *Self, idx: NodeIndex, id: *const ast.BindingIdentifier) void {
        const plain = id.decorators.len == 0 and !id.optional and
            id.type_annotation == .null and !self.definite_pending;
        if (plain) return self.writeString(id.name);
        self.emitAdornedBinding(idx, id);
    }

    noinline fn emitAdornedBinding(
        self: *Self,
        idx: NodeIndex,
        id: *const ast.BindingIdentifier,
    ) void {
        const definite = self.takeDefinite();
        self.printDecorators(id.decorators, .same_line);
        self.writeString(id.name);
        if (id.optional) self.emitInsideCommentsInline(idx);
        self.printBindingSuffix(id.optional, definite, id.type_annotation);
    }

    fn emit_label_identifier(self: *Self, id: *const ast.LabelIdentifier) void {
        self.writeString(id.name);
    }

    fn emit_private_identifier(self: *Self, id: *const ast.PrivateIdentifier) void {
        self.out.writeByte('#');
        self.writeString(id.name);
    }

    fn emit_assignment_pattern(self: *Self, p: *const ast.AssignmentPattern) void {
        self.printDecorators(p.decorators, .same_line);
        self.emit(p.left);
        self.printBindingSuffix(p.optional, false, p.type_annotation);
        self.printSpaced('=');
        self.emitValue(p.right);
    }

    fn emit_binding_rest_element(self: *Self, r: *const ast.BindingRestElement) void {
        self.printDecorators(r.decorators, .same_line);
        self.out.writeStr("...");
        self.emit(r.argument);
        self.printBindingSuffix(r.optional, false, r.type_annotation);
    }

    fn emit_array_pattern(self: *Self, idx: NodeIndex, p: *const ast.ArrayPattern) void {
        const definite = self.takeDefinite();
        self.printDecorators(p.decorators, .same_line);
        self.out.writeByte('[');
        self.emitInsideCommentsInline(idx);
        const list = self.tree.extra(p.elements);
        const depth = self.indent_depth;
        for (list, 0..) |x, i| {
            self.emitAssignTarget(x, .{ .defer_trailing = true });
            self.closeItem(i + 1 < list.len or p.rest != .null, depth);
        }
        if (p.rest != .null) {
            self.emitExpr(p.rest, .{ .defer_trailing = true });
            self.closeItem(false, depth);
        }
        self.closeList(depth);
        self.closeArray(list, p.rest);
        self.printBindingSuffix(p.optional, definite, p.type_annotation);
    }

    fn emit_object_pattern(self: *Self, idx: NodeIndex, p: *const ast.ObjectPattern) void {
        const definite = self.takeDefinite();
        self.printDecorators(p.decorators, .same_line);
        self.out.writeByte('{');
        self.emitInsideCommentsInline(idx);
        const list = self.tree.extra(p.properties);
        const has_any = list.len > 0 or p.rest != .null;
        if (has_any) self.out.space();
        self.emitItems(list, p.rest, .{});
        if (has_any and !self.out.atLineStart()) self.out.space();
        self.out.writeByte('}');
        self.printBindingSuffix(p.optional, definite, p.type_annotation);
    }

    fn emit_binding_property(self: *Self, p: *const ast.BindingProperty) void {
        if (p.shorthand and shorthandStillValid(self.tree, p.key, p.value)) {
            self.emitAssignTarget(p.value, .{});
            return;
        }
        self.printObjectKey(p.key, p.computed);
        self.printColon();
        self.emitAssignTarget(p.value, .{});
    }

    fn printPropertyKey(self: *Self, key: NodeIndex, computed: bool) void {
        if (computed) {
            self.out.writeByte('[');
            self.emitValue(key);
            self.out.writeByte(']');
        } else {
            self.emit(key);
        }
    }

    // computed `["__proto__"]` defines an own property, bare `__proto__` sets the prototype
    fn printObjectKey(self: *Self, key: NodeIndex, computed: bool) void {
        if (self.options.minify) {
            if (simpleStringKey(self.tree, key)) |s| {
                const proto_clash = computed and std.mem.eql(u8, s, "__proto__");
                if (!proto_clash) return self.writeNodeText(key, s);
            }
        }
        self.printPropertyKey(key, computed);
    }

    // `["constructor"]` would become the constructor or a SyntaxError, `static ["prototype"]` too
    fn printClassKey(
        self: *Self,
        key: NodeIndex,
        computed: bool,
        static: bool,
        is_field: bool,
    ) void {
        if (self.options.minify) {
            if (simpleStringKey(self.tree, key)) |s| {
                if (!computed) return self.writeNodeText(key, s);
                const ctor_clash = std.mem.eql(u8, s, "constructor") and (is_field or !static);
                const proto_clash = static and std.mem.eql(u8, s, "prototype");
                if (!ctor_clash and !proto_clash) return self.writeNodeText(key, s);
            }
        }
        self.printPropertyKey(key, computed);
    }

    fn emit_function(self: *Self, idx: NodeIndex, f: *const ast.Function) void {
        if (f.declare) self.out.writeStr("declare ");
        if (f.async) self.out.writeStr("async ");
        self.out.writeStr("function");
        if (f.generator) self.out.writeByte('*');
        if (f.id != .null) {
            self.out.writeByte(' ');
            self.emit(f.id);
        }
        self.printFunctionAsMethod(idx, f.*);
    }

    fn emitMethodValue(self: *Self, idx: NodeIndex) void {
        const scope = self.openComments(idx);
        self.printFunctionAsMethod(idx, self.nodeData(idx).function);
        self.closeComments(idx, scope, false);
    }

    fn printFunctionAsMethod(self: *Self, idx: NodeIndex, f: ast.Function) void {
        self.emit(f.type_parameters);
        self.printParams(idx, f.params);
        self.emit(f.return_type);
        if (f.body != .null) {
            self.out.space();
            self.emit(f.body);
        } else {
            self.softSemi();
        }
    }

    fn emit_arrow_function_expression(
        self: *Self,
        idx: NodeIndex,
        a: *const ast.ArrowFunctionExpression,
        ctx: Ctx,
    ) void {
        if (a.async) self.out.writeStr("async ");
        self.emitExpr(a.type_parameters, .{ .no_jsx_tag = true });
        self.printParams(idx, a.params);
        self.emitExpr(a.return_type, .{ .defer_trailing = true });
        self.out.space();
        self.out.writeStr("=>");
        self.emitOwedComments();
        if (!self.out.atLineStart()) self.out.space();
        if (a.expression) {
            self.out.lead = .arrow;
            self.emitExpr(a.body, .{ .prec = Precedence.Assignment, .no_in = ctx.no_in });
        } else {
            self.emit(a.body);
        }
    }

    fn printParams(self: *Self, owner: NodeIndex, idx: NodeIndex) void {
        std.debug.assert(!self.tree.hasComments(idx));
        const params = self.tree.data(idx).formal_parameters;
        const items = self.tree.extra(params.items);
        self.out.writeByte('(');
        const depth = self.indent_depth;
        for (items, 0..) |item, i| {
            self.emitExpr(parameterOf(self.tree, item), .{ .defer_trailing = true });
            self.closeItem(i + 1 < items.len or params.rest != .null, depth);
        }
        if (params.rest != .null) {
            self.emitExpr(params.rest, .{ .defer_trailing = true });
            self.closeItem(false, depth);
        }
        self.closeList(depth);
        self.emitInsideCommentsInline(owner);
        self.out.writeByte(')');
    }

    fn emit_class(self: *Self, c: *const ast.Class, ctx: Ctx) void {
        if (!ctx.no_decorators) self.printDecorators(c.decorators, .own_line);
        if (c.declare) self.out.writeStr("declare ");
        if (c.abstract) self.out.writeStr("abstract ");
        self.out.writeStr("class");
        if (c.id != .null) {
            self.out.writeByte(' ');
            self.emit(c.id);
        }
        self.emit(c.type_parameters);
        if (c.super_class != .null) {
            self.out.writeStr(" extends ");
            self.emitExpr(c.super_class, .{
                .prec = Precedence.Call,
                .no_instantiation = true,
            });
            self.emit(c.super_type_arguments);
        }
        if (c.implements.len > 0) {
            self.out.writeStr(" implements ");
            self.emitList(c.implements);
        }
        self.out.space();
        self.emit(c.body);
    }

    fn emit_class_body(self: *Self, idx: NodeIndex, b: *const ast.ClassBody) void {
        self.printMemberBody(idx, b.body);
    }

    fn printMemberBody(self: *Self, idx: NodeIndex, members: IndexRange) void {
        self.out.writeByte('{');
        self.indent_depth += 1;
        var any = false;
        for (self.tree.extra(members)) |m| {
            self.flushSemi();
            self.newline();
            self.emit(m);
            any = true;
        }
        self.indent_depth -= 1;
        if (any) {
            self.pending_semi = false;
            self.newline();
        } else if (self.comments_on) {
            self.emitInsideComments(idx);
        }
        self.out.writeByte('}');
    }

    fn emit_method_definition(self: *Self, m: *const ast.MethodDefinition) void {
        const fn_data = self.nodeData(m.value).function;
        self.printDecorators(m.decorators, .own_line);
        self.hoistKeyComments(m.key);
        defer self.skip_leading_of = .null;
        self.printAccessibility(m.accessibility);
        if (m.static) self.out.writeStr("static ");
        if (m.abstract) self.out.writeStr("abstract ");
        if (m.override) self.out.writeStr("override ");
        switch (m.kind) {
            .get => self.out.writeStr("get "),
            .set => self.out.writeStr("set "),
            .constructor, .method => {
                if (fn_data.async) self.out.writeStr("async ");
                if (fn_data.generator) self.out.writeByte('*');
            },
        }
        self.printClassKey(m.key, m.computed, m.static, false);
        if (m.optional) self.out.writeByte('?');
        self.emitMethodValue(m.value);
    }

    fn emit_property_definition(self: *Self, p: *const ast.PropertyDefinition) void {
        self.printDecorators(p.decorators, .own_line);
        self.hoistKeyComments(p.key);
        defer self.skip_leading_of = .null;
        if (p.declare) self.out.writeStr("declare ");
        self.printAccessibility(p.accessibility);
        if (p.static) self.out.writeStr("static ");
        if (p.abstract) self.out.writeStr("abstract ");
        if (p.override) self.out.writeStr("override ");
        if (p.readonly) self.out.writeStr("readonly ");
        if (p.accessor) self.out.writeStr("accessor ");
        if (p.definite) self.defer_trailing_of = p.key;
        self.printClassKey(p.key, p.computed, p.static, true);
        self.defer_trailing_of = .null;
        if (p.optional) self.out.writeByte('?');
        if (p.definite) self.out.writeByte('!');
        self.emitOwedComments();
        self.emit(p.type_annotation);
        if (p.value != .null) {
            self.printSpaced('=');
            self.emitValue(p.value);
        }
        self.softSemi();
    }

    fn printAccessibility(self: *Self, accessibility: ast.Accessibility) void {
        if (accessibility == .none) return;
        self.out.writeStr(accessibility.toString());
        self.out.writeByte(' ');
    }

    fn emit_decorator(self: *Self, d: *const ast.Decorator) void {
        self.out.writeByte('@');
        const simple = self.decoratorIsSimple(d.expression);
        self.emitExpr(d.expression, .{
            .prec = if (simple) Precedence.Lowest else Precedence.Grouping,
        });
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
            if (self.pretty() and layout == .own_line) {
                self.newline();
            } else if (self.pretty() or i + 1 == list.len) {
                self.out.writeByte(' ');
            }
        }
        self.out.map_start = carry;
    }

    fn emit_import_declaration(self: *Self, d: *const ast.ImportDeclaration) void {
        const list = self.tree.extra(d.specifiers);

        self.out.writeStr("import");
        if (d.import_kind == .type) self.out.writeStr(" type");
        if (d.phase) |ph| {
            self.out.writeByte(' ');
            self.out.writeStr(@tagName(ph));
        }

        if (list.len > 0) {
            self.out.writeByte(' ');
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
                    self.out.space();
                    self.emitItems(list[i..], .null, .{});
                    if (!self.out.atLineStart()) self.out.space();
                    self.out.writeByte('}');
                }
            }
            self.closeList(depth);
            self.out.writeStr(" from ");
        } else if (d.import_kind == .type) {
            self.out.writeByte(' ');
            self.out.writeStr("{} from ");
        } else {
            self.out.writeByte(' ');
        }

        self.emit(d.source);
        self.printAttributes(d.attributes);
        self.softSemi();
    }

    fn emit_import_specifier(self: *Self, s: *const ast.ImportSpecifier) void {
        if (s.import_kind == .type) self.out.writeStr("type ");
        self.printSpecifier(s.imported, s.local);
    }

    fn emit_import_default_specifier(self: *Self, s: *const ast.ImportDefaultSpecifier) void {
        self.emit(s.local);
    }

    fn emit_import_namespace_specifier(
        self: *Self,
        s: *const ast.ImportNamespaceSpecifier,
    ) void {
        self.out.writeStr("* as ");
        self.emit(s.local);
    }

    fn emit_import_attribute(self: *Self, a: *const ast.ImportAttribute) void {
        self.emit(a.key);
        self.printColon();
        self.emit(a.value);
    }

    fn emit_import_expression(self: *Self, e: *const ast.ImportExpression) void {
        self.out.writeStr("import");
        if (e.phase) |ph| {
            self.out.writeByte('.');
            self.out.writeStr(@tagName(ph));
        }
        self.out.writeByte('(');
        self.emitValue(e.source);
        if (e.options != .null) {
            self.out.writeByte(',');
            self.out.space();
            self.emitValue(e.options);
        }
        self.out.writeByte(')');
    }

    fn emit_export_named_declaration(self: *Self, idx: NodeIndex, d: *const ast.ExportNamedDeclaration) void {
        const list = self.tree.extra(d.specifiers);
        const hoisted = self.decoratorsBeforeExport(d.declaration);
        if (hoisted) |decorators| self.printDecorators(decorators, .own_line);
        self.out.writeStr("export");
        if (d.export_kind == .type and d.declaration == .null) self.out.writeStr(" type");
        if (d.declaration != .null) {
            self.out.writeByte(' ');
            return self.emitExpr(d.declaration, .{ .no_decorators = hoisted != null });
        }
        self.out.space();
        self.out.writeByte('{');
        self.emitInsideCommentsInline(idx);
        if (list.len > 0) {
            self.out.space();
            self.emitItems(list, .null, .{});
            if (!self.out.atLineStart()) self.out.space();
        }
        self.out.writeByte('}');
        if (d.source != .null) {
            self.out.writeStr(" from ");
            self.emit(d.source);
        }
        self.printAttributes(d.attributes);
        self.softSemi();
    }

    fn emit_export_default_declaration(
        self: *Self,
        d: *const ast.ExportDefaultDeclaration,
    ) void {
        if (self.nodeData(d.declaration).isDeclaration()) {
            const hoisted = self.decoratorsBeforeExport(d.declaration);
            if (hoisted) |decorators| self.printDecorators(decorators, .own_line);
            self.out.writeStr("export default ");
            return self.emitExpr(d.declaration, .{ .no_decorators = hoisted != null });
        }
        self.out.writeStr("export default ");
        self.out.lead = .export_default;
        self.emitExpr(d.declaration, .{ .prec = Precedence.Assignment });
        self.softSemi();
    }

    fn decoratorsBeforeExport(self: *const Self, declaration: NodeIndex) ?IndexRange {
        if (declaration == .null) return null;
        const decorators = switch (self.nodeData(declaration)) {
            .class => |c| c.decorators,
            else => return null,
        };
        if (decorators.len == 0) return null;
        const first = self.tree.extra(decorators)[0];
        if (self.tree.span(first).start < self.tree.span(declaration).start) return decorators;
        return null;
    }

    fn emit_export_all_declaration(self: *Self, d: *const ast.ExportAllDeclaration) void {
        self.out.writeStr("export");
        if (d.export_kind == .type) self.out.writeStr(" type");
        self.out.writeStr(" *");
        if (d.exported != .null) {
            self.out.writeStr(" as ");
            self.emit(d.exported);
        }
        self.out.writeStr(" from ");
        self.emit(d.source);
        self.printAttributes(d.attributes);
        self.softSemi();
    }

    fn emit_export_specifier(self: *Self, s: *const ast.ExportSpecifier) void {
        if (s.export_kind == .type) self.out.writeStr("type ");
        self.printSpecifier(s.local, s.exported);
    }

    fn printSpecifier(self: *Self, name: NodeIndex, alias: NodeIndex) void {
        self.emit(name);
        if (!sameIdentifier(self.tree, name, alias) or self.hasPrintedComments(alias)) {
            self.out.writeStr(" as ");
            self.emit(alias);
        }
    }

    fn printAttributes(self: *Self, attrs: IndexRange) void {
        if (attrs.len == 0) return;
        self.out.writeStr(" with ");
        self.out.writeByte('{');
        self.out.space();
        self.emitList(attrs);
        self.out.space();
        self.out.writeByte('}');
    }

    fn emit_ts_type_annotation(self: *Self, t: *const ast.TSTypeAnnotation) void {
        self.printColon();
        self.emit(t.type_annotation);
    }

    fn emit_ts_type_reference(self: *Self, t: *const ast.TSTypeReference) void {
        self.emit(t.type_name);
        self.emit(t.type_arguments);
    }

    fn emit_ts_qualified_name(self: *Self, q: *const ast.TSQualifiedName) void {
        self.emit(q.left);
        self.out.writeByte('.');
        self.emit(q.right);
    }

    fn emit_ts_type_query(self: *Self, q: *const ast.TSTypeQuery) void {
        self.out.writeStr("typeof ");
        self.emit(q.expr_name);
        self.emit(q.type_arguments);
    }

    fn emit_ts_import_type(self: *Self, t: *const ast.TSImportType) void {
        self.out.writeStr("import(");
        self.emit(t.source);
        if (t.options != .null) {
            self.out.writeByte(',');
            self.out.space();
            self.emit(t.options);
        }
        self.out.writeByte(')');
        if (t.qualifier != .null) {
            self.out.writeByte('.');
            self.emit(t.qualifier);
        }
        self.emit(t.type_arguments);
    }

    fn emit_ts_type_parameter(self: *Self, p: *const ast.TSTypeParameter) void {
        if (p.@"const") self.out.writeStr("const ");
        if (p.in) self.out.writeStr("in ");
        if (p.out) self.out.writeStr("out ");
        self.emit(p.name);
        if (p.constraint != .null) {
            self.out.writeStr(" extends ");
            self.emit(p.constraint);
        }
        if (p.default != .null) {
            self.printSpaced('=');
            self.emit(p.default);
        }
    }

    fn emit_ts_type_parameter_declaration(
        self: *Self,
        d: *const ast.TSTypeParameterDeclaration,
        ctx: Ctx,
    ) void {
        self.out.writeByte('<');
        self.emitList(d.params);
        // in TSX a lone `<T>` opens a JSX tag, and `<T,>` is valid TS too
        const params = self.tree.extra(d.params);
        if (ctx.no_jsx_tag and params.len == 1) {
            const param = self.tree.data(params[0]).ts_type_parameter;
            if (param.constraint == .null) self.out.writeByte(',');
        }
        self.out.writeByte('>');
    }

    fn emit_ts_type_parameter_instantiation(
        self: *Self,
        d: *const ast.TSTypeParameterInstantiation,
    ) void {
        self.out.writeByte('<');
        self.emitList(d.params);
        self.out.writeByte('>');
    }

    fn emit_ts_literal_type(self: *Self, t: *const ast.TSLiteralType) void {
        // `!0` is no type
        switch (self.nodeData(t.literal)) {
            .boolean_literal => |b| self.out.writeStr(if (b.value) "true" else "false"),
            else => self.emit(t.literal),
        }
    }

    fn emit_ts_template_literal_type(self: *Self, t: *const ast.TSTemplateLiteralType) void {
        self.printTemplate(t.quasis, t.types, false);
    }

    fn emitType(self: *Self, idx: NodeIndex, floor: u8, ctx: Ctx) void {
        self.wrapIf(self.typePrec(idx) < floor, idx, ctx);
    }

    fn typePrec(self: *const Self, idx: NodeIndex) u8 {
        return switch (self.nodeData(idx)) {
            .ts_function_type,
            .ts_constructor_type,
            .ts_conditional_type,
            .ts_infer_type,
            => TPrec.trailing,
            .ts_union_type => TPrec.@"union",
            .ts_intersection_type => TPrec.intersection,
            .ts_type_operator => TPrec.operator,
            else => TPrec.primary,
        };
    }

    fn emit_ts_array_type(self: *Self, t: *const ast.TSArrayType) void {
        self.emitType(t.element_type, TPrec.primary, .{ .defer_trailing = true });
        self.out.writeByte('[');
        self.emitOwedComments();
        self.out.writeByte(']');
    }

    fn emit_ts_indexed_access_type(self: *Self, t: *const ast.TSIndexedAccessType) void {
        self.emitType(t.object_type, TPrec.primary, .{ .defer_trailing = true });
        self.out.writeByte('[');
        self.emitOwedComments();
        self.emit(t.index_type);
        self.out.writeByte(']');
    }

    fn emit_ts_tuple_type(self: *Self, idx: NodeIndex, t: *const ast.TSTupleType) void {
        self.out.writeByte('[');
        self.emitInsideCommentsInline(idx);
        self.emitList(t.element_types);
        self.out.writeByte(']');
    }

    fn emit_ts_named_tuple_member(self: *Self, m: *const ast.TSNamedTupleMember) void {
        self.emit(m.label);
        if (m.optional) self.out.writeByte('?');
        self.printColon();
        self.emit(m.element_type);
    }

    fn emit_ts_optional_type(self: *Self, t: *const ast.TSOptionalType) void {
        self.emitType(t.type_annotation, TPrec.primary, .{});
        self.out.writeByte('?');
    }

    fn emit_ts_rest_type(self: *Self, t: *const ast.TSRestType) void {
        self.out.writeStr("...");
        self.emit(t.type_annotation);
    }

    fn emit_ts_jsdoc_nullable_type(self: *Self, t: *const ast.TSJSDocNullableType) void {
        self.printJSDocNullability('?', t.type_annotation, t.postfix);
    }

    fn emit_ts_jsdoc_non_nullable_type(
        self: *Self,
        t: *const ast.TSJSDocNonNullableType,
    ) void {
        self.printJSDocNullability('!', t.type_annotation, t.postfix);
    }

    fn printJSDocNullability(self: *Self, marker: u8, idx: NodeIndex, postfix: bool) void {
        if (!postfix) self.out.writeByte(marker);
        self.emit(idx);
        if (postfix) self.out.writeByte(marker);
    }

    fn emit_ts_union_type(self: *Self, t: *const ast.TSUnionType) void {
        self.emitTypeList(t.types, '|');
    }

    fn emit_ts_intersection_type(self: *Self, t: *const ast.TSIntersectionType) void {
        self.emitTypeList(t.types, '&');
    }

    // a lone member keeps its leading operator (`type X = | A`) so reparse keeps the wrapper
    fn emitTypeList(self: *Self, types: IndexRange, comptime op: u8) void {
        const list = self.tree.extra(types);
        if (list.len == 1) {
            self.out.writeByte(op);
            self.out.space();
        }
        const floor: u8 = if (op == '|') TPrec.intersection else TPrec.operator;
        for (list, 0..) |x, i| {
            if (i > 0) self.printSpaced(op);
            self.emitType(x, floor, .{});
        }
    }

    fn emit_ts_conditional_type(self: *Self, t: *const ast.TSConditionalType) void {
        self.emitType(t.check_type, TPrec.@"union", .{ .defer_trailing = true });
        self.writeKeywordThenOwed(" extends");
        self.emitType(t.extends_type, TPrec.@"union", .{});
        self.printSpaced('?');
        self.emit(t.true_type);
        self.printSpaced(':');
        self.emit(t.false_type);
    }

    fn emit_ts_infer_type(self: *Self, t: *const ast.TSInferType) void {
        self.out.writeStr("infer ");
        self.emit(t.type_parameter);
    }

    fn emit_ts_type_operator(self: *Self, t: *const ast.TSTypeOperator) void {
        self.out.writeStr(t.operator.toString());
        self.out.writeByte(' ');
        self.emitType(t.type_annotation, TPrec.operator, .{});
    }

    fn emit_ts_parenthesized_type(self: *Self, t: *const ast.TSParenthesizedType) void {
        self.out.writeByte('(');
        self.emit(t.type_annotation);
        self.out.writeByte(')');
    }

    fn emit_ts_function_type(self: *Self, idx: NodeIndex, t: *const ast.TSFunctionType) void {
        self.printArrowType(idx, t.type_parameters, t.params, t.return_type);
    }

    fn emit_ts_constructor_type(self: *Self, idx: NodeIndex, t: *const ast.TSConstructorType) void {
        if (t.abstract) self.out.writeStr("abstract ");
        self.out.writeStr("new ");
        self.printArrowType(idx, t.type_parameters, t.params, t.return_type);
    }

    fn printArrowType(
        self: *Self,
        idx: NodeIndex,
        type_parameters: NodeIndex,
        params: NodeIndex,
        return_type: NodeIndex,
    ) void {
        self.emit(type_parameters);
        self.printParams(idx, params);
        self.out.space();
        self.out.writeStr("=>");
        self.out.space();
        self.emitUnwrappedType(return_type);
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

    fn emit_ts_type_literal(self: *Self, idx: NodeIndex, t: *const ast.TSTypeLiteral) void {
        self.printMemberBody(idx, t.members);
    }

    fn emit_ts_mapped_type(self: *Self, t: *const ast.TSMappedType) void {
        self.out.writeByte('{');
        self.out.space();
        self.printMappedReadonly(t.readonly);
        self.out.writeByte('[');
        self.emit(t.key);
        self.out.writeStr(" in ");
        self.emit(t.constraint);
        if (t.name_type != .null) {
            self.out.writeStr(" as ");
            self.emit(t.name_type);
        }
        self.out.writeByte(']');
        self.printMappedOptional(t.optional);
        if (t.type_annotation != .null) {
            self.printColon();
            self.emit(t.type_annotation);
        }
        self.softSemi();
        self.pending_semi = false;
        self.out.space();
        self.out.writeByte('}');
    }

    fn printMappedReadonly(self: *Self, m: ast.TSMappedTypeModifier) void {
        switch (m) {
            .none => return,
            .true => {},
            .plus => self.out.writeByte('+'),
            .minus => self.out.writeByte('-'),
        }
        self.out.writeStr("readonly ");
    }

    fn printMappedOptional(self: *Self, m: ast.TSMappedTypeModifier) void {
        switch (m) {
            .none => {},
            .true => self.out.writeByte('?'),
            .plus => self.out.writeStr("+?"),
            .minus => self.out.writeStr("-?"),
        }
    }

    fn emit_ts_property_signature(self: *Self, s: *const ast.TSPropertySignature) void {
        if (s.readonly) self.out.writeStr("readonly ");
        self.printPropertyKey(s.key, s.computed);
        if (s.optional) self.out.writeByte('?');
        self.emit(s.type_annotation);
        self.softSemi();
    }

    fn emit_ts_method_signature(self: *Self, idx: NodeIndex, s: *const ast.TSMethodSignature) void {
        switch (s.kind) {
            .get => self.out.writeStr("get "),
            .set => self.out.writeStr("set "),
            .method => {},
        }
        self.printPropertyKey(s.key, s.computed);
        if (s.optional) self.out.writeByte('?');
        self.printSignatureTail(idx, s.type_parameters, s.params, s.return_type);
    }

    fn printSignatureTail(
        self: *Self,
        idx: NodeIndex,
        type_parameters: NodeIndex,
        params: NodeIndex,
        return_type: NodeIndex,
    ) void {
        self.emit(type_parameters);
        self.printParams(idx, params);
        self.emit(return_type);
        self.softSemi();
    }

    fn emit_ts_call_signature_declaration(
        self: *Self,
        idx: NodeIndex,
        s: *const ast.TSCallSignatureDeclaration,
    ) void {
        self.printSignatureTail(idx, s.type_parameters, s.params, s.return_type);
    }

    fn emit_ts_construct_signature_declaration(
        self: *Self,
        idx: NodeIndex,
        s: *const ast.TSConstructSignatureDeclaration,
    ) void {
        self.out.writeStr("new ");
        self.printSignatureTail(idx, s.type_parameters, s.params, s.return_type);
    }

    fn emit_ts_index_signature(self: *Self, s: *const ast.TSIndexSignature) void {
        if (s.static) self.out.writeStr("static ");
        if (s.readonly) self.out.writeStr("readonly ");
        self.out.writeByte('[');
        self.emitList(s.parameters);
        self.out.writeByte(']');
        self.emit(s.type_annotation);
        self.softSemi();
    }

    fn emit_ts_type_alias_declaration(
        self: *Self,
        d: *const ast.TSTypeAliasDeclaration,
    ) void {
        if (d.declare) self.out.writeStr("declare ");
        self.out.writeStr("type ");
        self.emit(d.id);
        self.emit(d.type_parameters);
        self.printSpaced('=');
        // a leftmost bare `intrinsic` reference would reparse as the keyword
        const intrinsic = self.isLeftmostIntrinsicReference(d.type_annotation);
        self.wrapIf(intrinsic, d.type_annotation, .{});
        self.softSemi();
    }

    inline fn wrapIf(self: *Self, cond: bool, idx: NodeIndex, ctx: Ctx) void {
        if (cond) self.out.writeByte('(');
        self.emitExpr(idx, ctx);
        if (cond) self.out.writeByte(')');
    }

    fn isLeftmostIntrinsicReference(self: *const Self, idx: NodeIndex) bool {
        var node = idx;
        for (0..self.tree.nodes.len) |_| {
            node = switch (self.nodeData(node)) {
                .ts_type_reference => |r| return r.type_arguments == .null and
                    isNamed(self.tree, r.type_name, "intrinsic"),
                .ts_array_type => |a| a.element_type,
                .ts_indexed_access_type => |a| a.object_type,
                .ts_union_type => |u| self.firstType(u.types) orelse return false,
                .ts_intersection_type => |i| self.firstType(i.types) orelse return false,
                .ts_conditional_type => |c| c.check_type,
                else => return false,
            };
        }
        unreachable;
    }

    fn firstType(self: *const Self, types: IndexRange) ?NodeIndex {
        return if (types.len > 0) self.tree.extra(types)[0] else null;
    }

    fn emit_ts_interface_declaration(self: *Self, d: *const ast.TSInterfaceDeclaration) void {
        if (d.declare) self.out.writeStr("declare ");
        self.out.writeStr("interface ");
        self.emit(d.id);
        self.emit(d.type_parameters);
        if (d.extends.len > 0) {
            self.out.writeStr(" extends ");
            self.emitList(d.extends);
        }
        self.out.space();
        self.emit(d.body);
    }

    fn emit_ts_interface_body(self: *Self, idx: NodeIndex, b: *const ast.TSInterfaceBody) void {
        self.printMemberBody(idx, b.body);
    }

    fn emit_ts_interface_heritage(self: *Self, h: *const ast.TSInterfaceHeritage) void {
        self.emit(h.expression);
        self.emit(h.type_arguments);
    }

    fn emit_ts_class_implements(self: *Self, c: *const ast.TSClassImplements) void {
        self.emit(c.expression);
        self.emit(c.type_arguments);
    }

    fn emit_ts_enum_declaration(self: *Self, d: *const ast.TSEnumDeclaration) void {
        if (d.declare) self.out.writeStr("declare ");
        if (d.is_const) self.out.writeStr("const ");
        self.out.writeStr("enum ");
        self.emit(d.id);
        self.out.space();
        self.emit(d.body);
    }

    fn emit_ts_enum_body(self: *Self, idx: NodeIndex, b: *const ast.TSEnumBody) void {
        self.out.writeByte('{');
        const list = self.tree.extra(b.members);
        if (list.len > 0) {
            self.indent_depth += 1;
            for (list, 0..) |m, i| {
                self.newline();
                self.emitExpr(m, .{ .defer_trailing = true });
                self.closeItem(i + 1 < list.len, null);
            }
            self.indent_depth -= 1;
            self.newline();
        } else if (self.comments_on) {
            self.emitInsideComments(idx);
        }
        self.out.writeByte('}');
    }

    fn emit_ts_enum_member(self: *Self, m: *const ast.TSEnumMember) void {
        if (m.computed) {
            self.out.writeByte('[');
            self.emit(m.id);
            self.out.writeByte(']');
        } else {
            self.emit(m.id);
        }
        if (m.initializer != .null) {
            self.printSpaced('=');
            self.emit(m.initializer);
        }
    }

    fn emit_ts_module_declaration(self: *Self, d: *const ast.TSModuleDeclaration) void {
        if (d.declare) self.out.writeStr("declare ");
        self.out.writeStr(d.kind.toString());
        self.out.writeByte(' ');
        self.emit(d.id);
        if (d.body != .null) {
            self.out.space();
            self.emit(d.body);
        } else {
            self.softSemi();
        }
    }

    fn emit_ts_module_block(self: *Self, idx: NodeIndex, b: *const ast.TSModuleBlock) void {
        self.printBlock(idx, self.tree.extra(b.body), false);
    }

    fn emit_ts_global_declaration(self: *Self, d: *const ast.TSGlobalDeclaration) void {
        if (d.declare) self.out.writeStr("declare ");
        self.emit(d.id);
        self.out.space();
        self.emit(d.body);
    }

    fn emit_ts_type_assertion(self: *Self, e: *const ast.TSTypeAssertion, ctx: Ctx) void {
        self.out.writeByte('<');
        // `<<T>` would re-lex as `<<`
        if (typeStartsWithLeftAngle(self.tree, e.type_annotation)) self.out.writeByte(' ');
        self.emit(e.type_annotation);
        self.out.writeByte('>');
        self.emitExpr(e.expression, .{
            .prec = Precedence.Unary,
            .no_instantiation = ctx.no_instantiation,
        });
    }

    fn emit_ts_export_assignment(self: *Self, e: *const ast.TSExportAssignment) void {
        self.out.writeStr("export");
        self.printSpaced('=');
        self.emit(e.expression);
        self.softSemi();
    }

    fn emit_ts_namespace_export_declaration(
        self: *Self,
        d: *const ast.TSNamespaceExportDeclaration,
    ) void {
        self.out.writeStr("export as namespace ");
        self.emit(d.id);
        self.softSemi();
    }

    fn emit_ts_import_equals_declaration(
        self: *Self,
        d: *const ast.TSImportEqualsDeclaration,
    ) void {
        self.out.writeStr("import ");
        if (d.import_kind == .type) self.out.writeStr("type ");
        self.emit(d.id);
        self.printSpaced('=');
        self.emit(d.module_reference);
        self.softSemi();
    }

    fn emit_ts_external_module_reference(
        self: *Self,
        r: *const ast.TSExternalModuleReference,
    ) void {
        self.out.writeStr("require(");
        self.emit(r.expression);
        self.out.writeByte(')');
    }

    fn emit_ts_parameter_property(self: *Self, p: *const ast.TSParameterProperty) void {
        self.printDecorators(p.decorators, .same_line);
        self.printAccessibility(p.accessibility);
        if (p.override) self.out.writeStr("override ");
        if (p.readonly) self.out.writeStr("readonly ");
        self.emit(p.parameter);
    }

    fn emit_ts_this_parameter(self: *Self, p: *const ast.TSThisParameter) void {
        self.out.writeStr("this");
        self.emit(p.type_annotation);
    }

    fn emit_jsx_element(self: *Self, e: *const ast.JSXElement) void {
        self.emit(e.opening_element);
        for (self.tree.extra(e.children)) |c| self.emit(c);
        self.emit(e.closing_element);
    }

    fn emit_jsx_opening_element(self: *Self, o: *const ast.JSXOpeningElement) void {
        self.out.writeByte('<');
        self.emit(o.name);
        self.emit(o.type_arguments);
        for (self.tree.extra(o.attributes)) |a| {
            self.out.writeByte(' ');
            self.emit(a);
        }
        if (o.self_closing) {
            self.out.space();
            self.out.writeStr("/>");
        } else {
            self.out.writeByte('>');
        }
    }

    fn emit_jsx_closing_element(self: *Self, c: *const ast.JSXClosingElement) void {
        self.out.writeStr("</");
        self.emit(c.name);
        self.out.writeByte('>');
    }

    fn emit_jsx_fragment(self: *Self, f: *const ast.JSXFragment) void {
        self.emit(f.opening_fragment);
        for (self.tree.extra(f.children)) |c| self.emit(c);
        self.emit(f.closing_fragment);
    }

    fn emit_jsx_identifier(self: *Self, id: *const ast.JSXIdentifier) void {
        self.writeString(id.name);
    }

    fn emit_jsx_namespaced_name(self: *Self, n: *const ast.JSXNamespacedName) void {
        self.emit(n.namespace);
        self.out.writeByte(':');
        self.emit(n.name);
    }

    fn emit_jsx_member_expression(self: *Self, m: *const ast.JSXMemberExpression) void {
        self.emit(m.object);
        self.out.writeByte('.');
        self.emit(m.property);
    }

    fn emit_jsx_attribute(self: *Self, a: *const ast.JSXAttribute) void {
        self.emit(a.name);
        if (a.value != .null) {
            self.out.writeByte('=');
            switch (self.nodeData(a.value)) {
                .string_literal => |lit| {
                    const raw = self.tree.string(lit.raw);
                    const value = self.tree.string(lit.value);
                    if (raw.len >= 2 and self.jsxDecodesTo(raw[1 .. raw.len - 1], value)) {
                        self.writeNodeText(a.value, raw);
                    } else {
                        self.writeNodeQuoted(a.value, '"', value, "\"&");
                    }
                },
                else => self.emit(a.value),
            }
        }
    }

    fn emit_jsx_spread_attribute(self: *Self, a: *const ast.JSXSpreadAttribute) void {
        self.printJSXSpread(a.argument);
    }

    fn printJSXSpread(self: *Self, idx: NodeIndex) void {
        self.out.writeStr("{...");
        self.emitValue(idx);
        self.out.writeByte('}');
    }

    fn emit_jsx_expression_container(self: *Self, c: *const ast.JSXExpressionContainer) void {
        self.out.writeByte('{');
        self.emitValue(c.expression);
        self.out.writeByte('}');
    }

    fn emit_jsx_empty_expression(self: *Self, idx: NodeIndex, _: *const ast.JSXEmptyExpression) void {
        self.emitInsideCommentsInline(idx);
    }

    fn emit_jsx_opening_fragment(self: *Self, idx: NodeIndex, _: *const ast.JSXOpeningFragment) void {
        self.out.writeByte('<');
        self.emitInsideCommentsInline(idx);
        self.out.writeByte('>');
    }

    fn emit_jsx_closing_fragment(self: *Self, idx: NodeIndex, _: *const ast.JSXClosingFragment) void {
        self.out.writeStr("</");
        self.emitInsideCommentsInline(idx);
        self.out.writeByte('>');
    }

    fn emit_jsx_text(self: *Self, t: *const ast.JSXText) void {
        const raw = self.tree.string(t.raw);
        const value = self.tree.string(t.value);
        if (self.jsxDecodesTo(raw, value)) return self.out.writeRawStr(raw);
        self.writeCharReferences(value, "&<>{}");
    }

    fn emit_jsx_spread_child(self: *Self, c: *const ast.JSXSpreadChild) void {
        self.printJSXSpread(c.expression);
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

fn emittedByParent(comptime tag: NodeTag) bool {
    return switch (tag) {
        .formal_parameters, .formal_parameter => true,
        else => false,
    };
}

fn sameIdentifier(tree: *const Tree, a: NodeIndex, b: NodeIndex) bool {
    if (a == .null or b == .null) return false;
    const an = identifierStringOrNull(tree, a) orelse return false;
    const bn = identifierStringOrNull(tree, b) orelse return false;
    return std.mem.eql(u8, tree.string(an), tree.string(bn));
}

fn shorthandStillValid(tree: *const Tree, key: NodeIndex, value: NodeIndex) bool {
    var v = value;
    if (tree.data(v) == .assignment_pattern) v = tree.data(v).assignment_pattern.left;
    return sameIdentifier(tree, key, v);
}

fn identifierStringOrNull(tree: *const Tree, idx: NodeIndex) ?ast.String {
    return switch (tree.data(idx)) {
        .identifier_name => |id| id.name,
        .identifier_reference => |id| id.name,
        .binding_identifier => |id| id.name,
        else => null,
    };
}

fn isNamed(tree: *const Tree, idx: NodeIndex, name: []const u8) bool {
    const id = identifierStringOrNull(tree, idx) orelse return false;
    return std.mem.eql(u8, tree.string(id), name);
}

fn parameterOf(tree: *const Tree, idx: NodeIndex) NodeIndex {
    return switch (tree.data(idx)) {
        .formal_parameter => |p| p.pattern,
        else => idx,
    };
}

// the parser drops the parens a ts cast needs inside a destructuring target
fn needsParensAsAssignTarget(tree: *const Tree, idx: NodeIndex) bool {
    return switch (tree.data(idx)) {
        .ts_as_expression,
        .ts_satisfies_expression,
        .ts_type_assertion,
        => true,
        else => false,
    };
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

fn needsSpaceBeforeInlineComment(last: u8) bool {
    return switch (last) {
        0, ' ', '\n', '(', '[', '{', '<' => false,
        else => true,
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
            .binary_expression => |b| b.right,
            .logical_expression => |b| b.right,
            .assignment_expression => |a| a.right,
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
        => true,
        else => false,
    };
}

const NodeTag = std.meta.Tag(NodeData);

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

// decided per node by `needsParens`
const operator_precedence = Precedence.Lowest;

comptime {
    std.debug.assert(operator_precedence < Precedence.Comma);
}

const node_precedence = blk: {
    var table: [@typeInfo(NodeTag).@"enum".field_names.len]u8 = undefined;
    for (std.enums.values(NodeTag)) |tag| table[@backingInt(tag)] = switch (tag) {
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
        .logical_expression, .binary_expression, .boolean_literal => operator_precedence,
        else => Precedence.Grouping,
    };
    break :blk table;
};

const TPrec = struct {
    const trailing: u8 = 1;
    const @"union": u8 = 2;
    const intersection: u8 = 3;
    const operator: u8 = 4;
    const primary: u8 = 5;
};
