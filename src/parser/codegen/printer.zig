const std = @import("std");
const util = @import("util");
const Simd = util.Simd;
const ast = @import("../ast.zig");
const output = @import("output.zig");
const sourcemap = @import("sourcemap.zig");
const utils = @import("utils.zig");

const source_maps = @import("codegen_options").source_maps;

const Allocator = std.mem.Allocator;
const Output = output.Output;
const Tree = ast.Tree;
const NodeIndex = ast.NodeIndex;
const NodeData = ast.NodeData;
const IndexRange = ast.IndexRange;
const Precedence = @import("../token.zig").Precedence;

pub const SourceMap = sourcemap.SourceMap;
pub const SourceMapOptions = sourcemap.Options;

/// Whitespace mode. `compact` emits only the whitespace the grammar requires.
pub const Format = enum { pretty, compact };

/// Quote style for string literals. `preserve` keeps each literal's source quote and
/// `shortest` picks the quote with fewer escapes, double on a tie.
pub const Quotes = enum { preserve, double, single, shortest };

/// Comment passthrough filter. `some` keeps legal headers, JSDoc, and annotations.
pub const Comments = enum { none, all, some, line, block };

pub const Options = struct {
    /// Drop TypeScript-only syntax.
    strip: bool = false,
    /// Apply size-reducing syntax rewrites.
    minify: bool = false,
    format: Format = .pretty,
    /// Spaces per level in pretty format.
    indent: u8 = 2,
    quotes: Quotes = .preserve,
    source_map: ?SourceMapOptions = null,
    comments: Comments = .some,
};

pub const Diagnostic = ast.Diagnostic;

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

pub const Error = error{OutOfMemory};

pub fn generate(allocator: Allocator, tree: *Tree, options: Options) Error!Result {
    std.debug.assert(tree.root != .null);
    var p = try Printer.init(allocator, tree, options);
    defer p.deinit();
    try p.emit(tree.root);
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

const Ctx = struct {
    prec: u8 = Precedence.Lowest,
    no_in: bool = false,
    no_call: bool = false,
    // the next token would change how TypeScript reads a trailing `f<T>`
    no_instantiation: bool = false,
    no_jsx_tag: bool = false,
    tagged: bool = false,
    defer_trailing: bool = false,
    no_decorators: bool = false,
};

const Printer = struct {
    allocator: Allocator,
    tree: *Tree,
    node_data: []const NodeData,
    options: Options,
    out: Output,
    diagnostics: std.ArrayList(Diagnostic) = .empty,

    indent_depth: u32 = 0,
    pending_semi: bool = false,
    // the node whose inside-comments a container prints
    current_idx: NodeIndex = .null,
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

    stack_floor: usize = 0,
    links: std.ArrayList(Link) = .empty,

    const Link = struct {
        idx: NodeIndex,
        inner: Ctx,
        wrap: bool,
        // a cast strip removes, open for its comments
        stripped: bool,
        defer_trailing: bool,
        scope: CommentScope,
    };

    const CommentScope = struct { prev_idx: NodeIndex, commented: bool };

    const Head = struct { idx: NodeIndex, ctx: Ctx };
    const Self = @This();

    fn init(allocator: Allocator, tree: *Tree, options: Options) Error!Self {
        return .{
            .allocator = allocator,
            .tree = tree,
            .node_data = tree.nodes.items(.data),
            .options = options,
            .out = try Output.init(
                allocator,
                options.format == .pretty,
                .{ .code = tree.source.len, .map = tree.nodes.len * 8 + 64 },
                options.source_map,
            ),
            .stack_floor = @frameAddress() -| chain_stack_bytes_max,
        };
    }

    fn deinit(self: *Self) void {
        self.out.deinit();
        self.diagnostics.deinit(self.allocator);
        self.links.deinit(self.allocator);
    }

    inline fn pretty(self: *const Self) bool {
        return self.out.pretty;
    }

    inline fn nodeData(self: *const Self, idx: NodeIndex) NodeData {
        return self.node_data[@backingInt(idx)];
    }

    inline fn writeString(self: *Self, id: ast.String) Error!void {
        try self.out.writeStr(self.tree.string(id));
    }

    inline fn printEq(self: *Self) Error!void {
        try self.out.space();
        try self.out.writeByte('=');
        try self.out.space();
    }

    fn emitList(self: *Self, items: IndexRange) Error!void {
        try self.emitItems(self.tree.extra(items), .null, .{});
    }

    fn emitItems(self: *Self, items: []const NodeIndex, rest: NodeIndex, ctx: Ctx) Error!void {
        var item_ctx = ctx;
        item_ctx.defer_trailing = true;
        const depth = self.indent_depth;
        for (items, 0..) |x, i| {
            try self.emitExpr(x, item_ctx);
            try self.closeItem(i + 1 < items.len or rest != .null, depth);
        }
        if (rest != .null) {
            try self.emitExpr(rest, item_ctx);
            try self.closeItem(false, depth);
        }
        try self.closeList(depth);
    }

    // an item's separator precedes its trailing comments, so a line comment cannot swallow it
    fn closeItem(self: *Self, separated: bool, hang_depth: ?u32) Error!void {
        if (separated) try self.out.writeByte(',');
        try self.emitOwedComments();
        if (!separated) return;
        if (hang_depth) |depth| if (self.indent_depth == depth and self.out.atLineStart()) {
            self.indent_depth = depth + 1;
            try self.breakLine();
        };
        if (!self.out.atLineStart()) try self.out.space();
    }

    fn closeList(self: *Self, depth: u32) Error!void {
        std.debug.assert(self.owed == .null);
        if (self.indent_depth == depth) return;
        std.debug.assert(self.indent_depth == depth + 1);
        self.indent_depth = depth;
        if (self.out.atLineStart()) try self.breakLine();
    }

    fn printBindingSuffix(
        self: *Self,
        optional: bool,
        definite: bool,
        annotation: NodeIndex,
    ) Error!void {
        if (!self.options.strip) if (optional) try self.out.writeByte('?');
        if (definite) try self.out.writeByte('!');
        try self.emit(annotation);
    }

    inline fn takeDefinite(self: *Self) bool {
        if (self.options.strip) return false;
        const d = self.definite_pending;
        self.definite_pending = false;
        return d;
    }

    fn newline(self: *Self) Error!void {
        if (self.pretty()) try self.breakLine();
    }

    fn emitStmt(self: *Self, idx: NodeIndex) Error!void {
        if (self.options.strip and self.stripsToNothing(idx)) {
            try self.emitNothing(idx);
            try self.out.writeByte(';');
        } else {
            try self.emit(idx);
        }
    }

    fn emitNothing(self: *Self, idx: NodeIndex) Error!void {
        std.debug.assert(self.stripsToNothing(idx));
        const written = self.out.len();
        const held = self.out.held_spaces;
        // the item's own mapping would land on whatever prints next
        const map_start = self.out.map_start;
        try self.emit(idx);
        self.out.map_start = map_start;
        std.debug.assert(self.out.len() == written);
        std.debug.assert(self.out.held_spaces == held);
    }

    noinline fn stripsToNothing(self: *const Self, idx: NodeIndex) bool {
        std.debug.assert(self.options.strip);
        if (idx == .null) return true;
        const data = self.nodeData(idx);
        if (data.isTypeContext()) return true;
        switch (data) {
            .ts_type_alias_declaration,
            .ts_interface_declaration,
            .ts_global_declaration,
            .ts_namespace_export_declaration,
            .ts_this_parameter,
            .ts_enum_declaration,
            .ts_module_declaration,
            .ts_import_equals_declaration,
            .ts_export_assignment,
            => return true,
            // its comments alone would leave a bare `,`
            .import_specifier => |sp| if (sp.import_kind == .type) return true,
            .export_specifier => |sp| if (sp.export_kind == .type) return true,
            else => {},
        }
        if (self.hasPrintedComments(idx)) return false;
        return switch (data) {
            .variable_declaration => |d| isAmbient(self.tree, d),
            .function => |f| f.declare or
                f.type == .ts_declare_function or
                f.type == .ts_empty_body_function_expression,
            .class => |c| c.declare,
            .method_definition => |m| m.abstract or
                self.nodeData(m.value).function.body == .null,
            .property_definition => |d| d.declare or d.abstract,
            .import_declaration => |d| d.import_kind == .type or (d.specifiers.len > 0 and
                !hasValueImportSpecifier(self.tree, self.tree.extra(d.specifiers))),
            .export_named_declaration => |d| if (d.declaration != .null)
                d.export_kind == .type or self.stripsToNothing(d.declaration)
            else
                d.export_kind == .type or (d.specifiers.len > 0 and
                    !hasValueExportSpecifier(self.tree, self.tree.extra(d.specifiers))),
            .export_default_declaration => |d| self.nodeData(d.declaration).isDeclaration() and
                self.stripsToNothing(d.declaration),
            .export_all_declaration => |d| d.export_kind == .type,
            else => false,
        };
    }

    fn hasPrintedComments(self: *const Self, idx: NodeIndex) bool {
        if (self.options.comments == .none) return false;
        for (self.tree.commentsOf(idx)) |c| {
            const printed = switch (c.position) {
                .before => idx != self.skip_leading_of,
                .after => true,
                .inside => false,
            };
            if (printed and self.allowComment(c)) return true;
        }
        return false;
    }

    fn emit(self: *Self, idx: NodeIndex) Error!void {
        return self.emitExpr(idx, .{});
    }

    fn emitExpr(self: *Self, idx: NodeIndex, ctx: Ctx) Error!void {
        if (idx == .null) return;

        if (self.options.strip) {
            if (try self.emitStrippedNode(idx, ctx)) return;
        }

        switch (self.nodeData(idx)) {
            inline .identifier_reference, .identifier_name => |id| {
                const has_comments = self.options.comments != .none and
                    self.tree.commentsOf(idx).len > 0;
                if (!has_comments) {
                    self.recordMapping(idx);
                    return self.writeString(id.name);
                }
            },
            else => {},
        }

        const wrap = self.needsParens(idx, ctx);
        var inner: Ctx = if (wrap) .{} else ctx;
        inner.defer_trailing = false;
        if (wrap) try self.out.writeByte('(');
        if (self.options.comments != .none) {
            const scope = try self.openComments(idx);
            try self.emitNode(idx, inner);
            try self.closeComments(idx, scope, ctx.defer_trailing);
        } else {
            try self.emitNode(idx, inner);
        }
        if (wrap) try self.out.writeByte(')');
    }

    inline fn openComments(self: *Self, idx: NodeIndex) Error!CommentScope {
        const comments = self.tree.commentsOf(idx);
        const scope: CommentScope = .{
            .prev_idx = self.current_idx,
            .commented = self.options.comments != .none and comments.len > 0,
        };
        self.current_idx = idx;
        if (scope.commented) {
            const saved_lead = self.out.lead;
            try self.emitLeadingComments(idx, comments);
            self.out.lead = saved_lead;
        }
        return scope;
    }

    inline fn closeComments(
        self: *Self,
        idx: NodeIndex,
        scope: CommentScope,
        defer_trailing: bool,
    ) Error!void {
        std.debug.assert(self.current_idx == idx);
        if (scope.commented) {
            if (defer_trailing or idx == self.defer_trailing_of) {
                std.debug.assert(self.owed == .null);
                self.owed = idx;
            } else {
                try self.emitTrailingComments(self.tree.commentsOf(idx));
            }
        }
        self.current_idx = scope.prev_idx;
    }

    fn writeNodeText(self: *Self, idx: NodeIndex, text: []const u8) Error!void {
        const scope = try self.openComments(idx);
        try self.out.writeStr(text);
        try self.closeComments(idx, scope, false);
    }

    fn emitOwedComments(self: *Self) Error!void {
        const owed = self.owed;
        if (owed == .null) return;
        self.owed = .null;
        try self.emitTrailingComments(self.tree.commentsOf(owed));
    }

    fn writeKeywordThenOwed(self: *Self, keyword: []const u8) Error!void {
        try self.out.writeStr(keyword);
        try self.emitOwedComments();
        if (!self.out.atLineStart()) try self.out.writeByte(' ');
    }

    fn emitLink(self: *Self, comptime tag: NodeTag, node: anytype, ctx: Ctx) Error!void {
        const head = self.linkHead(tag, node, ctx);
        if (@frameAddress() > self.stack_floor) {
            try self.emitExpr(head.idx, head.ctx);
        } else {
            try self.emitChainIteratively(head);
        }
        try self.emitLinkSuffix(tag, node, ctx);
    }

    noinline fn emitChainIteratively(self: *Self, head: Head) Error!void {
        std.debug.assert(@frameAddress() <= self.stack_floor);
        const first = self.stripped(head.idx);
        if (first == .null or !isChainLink(self.tagOf(first))) {
            return self.emitExpr(head.idx, head.ctx);
        }

        if (self.options.strip) std.debug.assert(!head.ctx.defer_trailing);
        const base = self.links.items.len;
        _ = try self.openStrippedCasts(head.idx, first, false);
        var link = try self.openLink(first, head.ctx);
        while (true) {
            const next_head = self.linkHeadOf(link.idx, link.inner);
            const next = self.stripped(next_head.idx);
            if (next == .null or !isChainLink(self.tagOf(next))) {
                try self.emitExpr(next_head.idx, next_head.ctx);
                break;
            }
            std.debug.assert(self.links.items.len - base < self.tree.nodes.len);
            if (self.options.strip) std.debug.assert(!next_head.ctx.defer_trailing);
            try self.links.append(self.allocator, link);
            _ = try self.openStrippedCasts(next_head.idx, next, false);
            link = try self.openLink(next, next_head.ctx);
        }
        while (true) {
            if (!link.stripped) try self.emitLinkSuffixOf(link);
            try self.closeLink(link);
            if (self.links.items.len == base) break;
            link = self.links.pop().?;
        }
    }

    fn openLink(self: *Self, idx: NodeIndex, ctx: Ctx) Error!Link {
        std.debug.assert(isChainLink(self.tagOf(idx)));
        const wrap = self.needsParens(idx, ctx);
        if (wrap) try self.out.writeByte('(');
        const scope = try self.openComments(idx);
        self.recordMapping(idx);
        var inner: Ctx = if (wrap) .{} else ctx;
        inner.defer_trailing = false;
        return .{
            .idx = idx,
            .inner = inner,
            .wrap = wrap,
            .stripped = false,
            .defer_trailing = ctx.defer_trailing,
            .scope = scope,
        };
    }

    fn closeLink(self: *Self, link: Link) Error!void {
        try self.closeComments(link.idx, link.scope, link.defer_trailing);
        if (link.wrap) try self.out.writeByte(')');
    }

    // only the outermost cast defers, so the comments print in source order
    fn openStrippedCasts(
        self: *Self,
        outer: NodeIndex,
        operand: NodeIndex,
        defer_trailing: bool,
    ) Error!bool {
        if (self.options.comments == .none) return false;
        var opened = false;
        var cast = outer;
        for (0..self.tree.nodes.len) |_| {
            if (cast == operand) return opened;
            if (self.tree.commentsOf(cast).len > 0) {
                try self.links.append(self.allocator, .{
                    .idx = cast,
                    .inner = .{},
                    .wrap = false,
                    .stripped = true,
                    .defer_trailing = defer_trailing and !opened,
                    .scope = try self.openComments(cast),
                });
                opened = true;
            }
            cast = strippedOperand(self.tree, cast);
        }
        unreachable;
    }

    fn emitStrippedOperand(self: *Self, outer: NodeIndex, operand: NodeIndex, ctx: Ctx) Error!void {
        const base = self.links.items.len;
        var operand_ctx = ctx;
        if (try self.openStrippedCasts(outer, operand, ctx.defer_trailing)) {
            operand_ctx.defer_trailing = false;
        }
        try self.emitExpr(operand, operand_ctx);
        while (self.links.items.len > base) try self.closeLink(self.links.pop().?);
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

    fn emitLinkSuffixOf(self: *Self, link: Link) Error!void {
        switch (self.node_data[@backingInt(link.idx)]) {
            inline else => |*node, tag| if (comptime isChainLink(tag)) {
                try self.emitLinkSuffix(tag, node, link.inner);
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
            .ts_non_null_expression, .ts_instantiation_expression => .{
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
    ) Error!void {
        switch (tag) {
            .binary_expression => {
                const op = node.operator.toString();
                const p: u8 = node.operator.toToken().precedence();
                const right_min = if (node.operator == .exponent) p else p + 1;
                if (utils.isWordOp(op)) {
                    try self.out.writeByte(' ');
                    try self.out.writeStr(op);
                    try self.out.writeByte(' ');
                } else {
                    try self.out.space();
                    // `f<T>==x` would re-lex the type argument closer into `>=`
                    if (self.out.lastByte() == '>' and (op[0] == '>' or op[0] == '=')) {
                        try self.out.writeByte(' ');
                    }
                    try self.out.writeStr(op);
                    try self.out.space();
                }
                try self.emitExpr(node.right, .{
                    .prec = right_min,
                    .no_in = ctx.no_in,
                    .no_instantiation = ctx.no_instantiation,
                });
            },
            .logical_expression => {
                const p: u8 = node.operator.toToken().precedence();
                try self.out.space();
                try self.out.writeStr(node.operator.toString());
                try self.out.space();
                const prec = self.logicalOperandPrecedence(node.right, p + 1, node.operator);
                try self.emitExpr(node.right, .{ .prec = prec, .no_in = ctx.no_in });
            },
            .member_expression => {
                const static_key = if (self.options.minify and node.computed)
                    simpleStringKey(self.tree, node.property)
                else
                    null;

                if (node.computed and static_key == null) {
                    if (node.optional) try self.out.writeStr("?.");
                    try self.out.writeByte('[');
                    try self.emit(node.property);
                    try self.out.writeByte(']');
                } else {
                    try self.out.writeStr(if (node.optional) "?." else ".");
                    if (static_key) |k| {
                        try self.writeNodeText(node.property, k);
                    } else {
                        try self.emit(node.property);
                    }
                }
            },
            .call_expression => {
                if (node.optional) try self.out.writeStr("?.");
                try self.emit(node.type_arguments);
                try self.printArgList(node.arguments);
            },
            .tagged_template_expression => {
                try self.emit(node.type_arguments);
                try self.emitExpr(node.quasi, .{ .tagged = true });
            },
            .chain_expression => {},
            .ts_non_null_expression => try self.out.writeByte('!'),
            .ts_instantiation_expression => try self.emit(node.type_arguments),
            .ts_as_expression => {
                try self.writeKeywordThenOwed(" as");
                try self.emit(node.type_annotation);
            },
            .ts_satisfies_expression => {
                try self.writeKeywordThenOwed(" satisfies");
                try self.emit(node.type_annotation);
            },
            else => @compileError("not a chain link: " ++ @tagName(tag)),
        }
    }

    noinline fn emitStrippedNode(self: *Self, idx: NodeIndex, ctx: Ctx) Error!bool {
        std.debug.assert(self.options.strip);
        const data = self.nodeData(idx);
        if (data.isTypeContext()) return true;

        switch (data) {
            .ts_type_alias_declaration,
            .ts_interface_declaration,
            .ts_global_declaration,
            .ts_namespace_export_declaration,
            .ts_this_parameter,
            => return true,
            .import_specifier => |s| return s.import_kind == .type,
            .export_specifier => |s| return s.export_kind == .type,
            .ts_as_expression,
            .ts_satisfies_expression,
            .ts_type_assertion,
            .ts_non_null_expression,
            .ts_instantiation_expression,
            => {
                try self.emitStrippedOperand(idx, self.stripped(idx), ctx);
                return true;
            },
            .ts_enum_declaration => |e| {
                if (!e.declare) try self.diagnose(
                    idx,
                    "TypeScript enums cannot be stripped to JavaScript",
                );
                return true;
            },
            .ts_module_declaration => |m| {
                if (!m.declare) try self.diagnose(
                    idx,
                    "TypeScript namespaces cannot be stripped to JavaScript",
                );
                return true;
            },
            .ts_import_equals_declaration => |i| {
                if (i.import_kind != .type) try self.diagnose(
                    idx,
                    "`import = require()` cannot be stripped to JavaScript",
                );
                return true;
            },
            .ts_export_assignment => {
                try self.diagnose(idx, "`export =` cannot be stripped to JavaScript");
                return true;
            },
            .ts_parameter_property => |pp| {
                try self.diagnose(
                    idx,
                    "parameter properties cannot be stripped to JavaScript",
                );
                try self.emitStrippedOperand(idx, pp.parameter, ctx);
                return true;
            },
            else => return false,
        }
    }

    // minify's `!0` ranks as unary
    inline fn precedenceOf(self: *const Self, idx: NodeIndex) u8 {
        const data = self.nodeData(idx);
        const fixed = node_precedence[@backingInt(std.meta.activeTag(data))];
        if (fixed != operator_precedence) return fixed;
        return switch (data) {
            .logical_expression => |l| l.operator.toToken().precedence(),
            .binary_expression => |b| b.operator.toToken().precedence(),
            .boolean_literal => if (self.options.minify) Precedence.Unary else Precedence.Grouping,
            else => unreachable,
        };
    }

    inline fn needsParens(self: *const Self, idx: NodeIndex, ctx: Ctx) bool {
        // anything ranked below relational wraps, so only operators above it pass the flag on
        if (ctx.no_instantiation) std.debug.assert(ctx.prec >= Precedence.Relational);
        if (self.out.lead == .none and ctx.prec <= Precedence.Comma and
            !ctx.no_call and !ctx.no_in) return false;

        const data = self.nodeData(idx);

        if (self.out.lead == .stmt or self.out.lead == .arrow) {
            switch (data) {
                .object_expression => return true,
                .assignment_expression => |a| {
                    if (self.nodeData(a.left) == .object_pattern) return true;
                },
                else => {},
            }
        }
        if (self.out.lead == .stmt or self.out.lead == .export_default) {
            switch (data) {
                .function => |f| if (f.type == .function_expression or
                    f.type == .ts_empty_body_function_expression) return true,
                .class => |c| if (c.type == .class_expression) return true,
                .member_expression => |m| {
                    if (m.computed and isNamed(self.tree, m.object, "let")) return true;
                },
                else => {},
            }
        }

        if (ctx.no_call) switch (data) {
            .call_expression, .import_expression, .chain_expression => return true,
            else => {},
        };
        if (ctx.no_instantiation and data == .ts_instantiation_expression) return true;
        if (ctx.prec >= Precedence.Call and data == .chain_expression) return true;

        if (ctx.no_in and data == .binary_expression and
            data.binary_expression.operator == .in) return true;

        return self.precedenceOf(idx) < ctx.prec;
    }

    inline fn stripped(self: *const Self, idx: NodeIndex) NodeIndex {
        if (!self.options.strip) return idx;
        var i = idx;
        while (true) i = switch (self.nodeData(i)) {
            .ts_as_expression => |e| e.expression,
            .ts_satisfies_expression => |e| e.expression,
            .ts_non_null_expression => |e| e.expression,
            .ts_instantiation_expression => |e| e.expression,
            .ts_type_assertion => |e| e.expression,
            else => return i,
        };
    }

    inline fn emitNode(self: *Self, idx: NodeIndex, ctx: Ctx) Error!void {
        @setEvalBranchQuota(10_000);
        self.recordMapping(idx);

        switch (self.node_data[@backingInt(idx)]) {
            // out of line, so the recursion's frame holds only what every node needs
            inline else => |*node, tag| {
                if (comptime isChainLink(tag)) {
                    return @call(.never_inline, emitLink, .{ self, tag, node, ctx });
                }
                if (comptime fixedString(tag)) |s| {
                    try self.out.writeStr(s);
                } else if (comptime emittedByParent(tag)) {
                    std.debug.panic("codegen: {s} is emitted by its parent", .{@tagName(tag)});
                } else {
                    const emitter = @field(Self, "emit_" ++ @tagName(tag));
                    if (comptime @typeInfo(@TypeOf(emitter)).@"fn".param_types.len == 3) {
                        try @call(.never_inline, emitter, .{ self, node, ctx });
                    } else {
                        try @call(.never_inline, emitter, .{ self, node });
                    }
                }
            },
        }
    }

    inline fn allowComment(self: *const Self, c: ast.AttachedComment) bool {
        return switch (self.options.comments) {
            .none => false,
            .all => true,
            .some => c.type == .block and
                utils.isSignificantBlockComment(self.tree.string(c.value)),
            .line => c.type == .line,
            .block => c.type == .block,
        };
    }

    fn emitLeadingComments(
        self: *Self,
        idx: NodeIndex,
        comments: []const ast.AttachedComment,
    ) Error!void {
        if (idx == self.skip_leading_of) return;
        for (comments) |c| {
            if (c.position == .before and self.allowComment(c)) try self.writeLeading(c);
        }
    }

    // a comment between `get`/`async` and its key would split them
    fn hoistKeyComments(self: *Self, key: NodeIndex) Error!void {
        if (self.options.comments == .none) return;
        if (key == .null) return;
        try self.emitLeadingComments(key, self.tree.commentsOf(key));
        self.skip_leading_of = key;
    }

    fn emitTrailingComments(self: *Self, comments: []const ast.AttachedComment) Error!void {
        for (comments) |c| {
            if (c.position == .after and self.allowComment(c)) {
                // a deferred `;` landing after the comment would re-home it on reparse
                try self.flushSemi();
                try self.writeTrailing(c);
            }
        }
    }

    fn writeLeading(self: *Self, c: ast.AttachedComment) Error!void {
        const armed = self.restrictedArmed();
        if (c.type == .block and c.same_line) {
            // a comment spanning lines is a line terminator too
            if (armed and hasLineTerminator(self.tree.string(c.value))) {
                try self.openRestrictedParen();
            }
            const last = self.out.lastByte();
            if (self.pretty() and last != 0 and last != ' ' and last != '\n') {
                try self.out.writeByte(' ');
            }
            try self.writeCommentBody(c);
            if (self.pretty()) try self.out.writeByte(' ');
        } else {
            try self.breakLine();
            try self.writeCommentBody(c);
            try self.breakLine();
        }
        // a comment is not the operand's first token
        if (self.restricted != null) self.restricted_len = self.out.len();
    }

    fn writeTrailing(self: *Self, c: ast.AttachedComment) Error!void {
        if (c.same_line) {
            if (self.pretty()) try self.out.writeByte(' ');
            try self.writeCommentBody(c);
            if (c.type == .line) try self.breakLine();
            return;
        }
        try self.breakLine();
        try self.writeCommentBody(c);
        try self.breakLine();
    }

    inline fn writeCommentBody(self: *Self, c: ast.AttachedComment) Error!void {
        const value = self.tree.string(c.value);
        try self.out.writeStr(if (c.type == .line) "//" else "/*");
        if (c.type == .block) {
            try self.writeBlockBody(value);
            try self.out.writeComment("*/");
        } else {
            try self.out.writeComment(value);
        }
    }

    fn writeBlockBody(self: *Self, value: []const u8) Error!void {
        if (!self.pretty() or !utils.isJsdocBody(value)) {
            try self.out.writeComment(value);
            return;
        }
        var it = std.mem.splitScalar(u8, value, '\n');
        try self.out.writeStr(std.mem.trimEnd(u8, it.first(), "\r"));
        while (it.next()) |line| {
            try self.breakLine();
            const text = std.mem.trimStart(u8, std.mem.trimEnd(u8, line, "\r"), " \t");
            // a line break alone would collapse a blank line
            if (text.len == 0 and it.peek() != null) {
                self.out.held_spaces = 0;
                try self.out.writeComment("\n");
                continue;
            }
            try self.out.writeByte(' ');
            try self.out.writeStr(text);
        }
    }

    fn breakLine(self: *Self) Error!void {
        if (self.restrictedArmed()) try self.openRestrictedParen();
        try self.out.endLine(self.indent_depth * self.options.indent);
    }

    inline fn recordMapping(self: *Self, idx: NodeIndex) void {
        if (comptime !source_maps) return;
        if (self.out.map != null) self.out.recordMapping(self.tree.span(idx));
    }

    fn diagnose(self: *Self, idx: NodeIndex, message: []const u8) Error!void {
        try self.diagnostics.append(self.allocator, .{
            .severity = .@"error",
            .message = message,
            .span = self.tree.span(idx),
        });
    }

    fn emit_program(self: *Self, p: *const ast.Program) Error!void {
        if (p.hashbang) |h| {
            try self.out.writeStr("#!");
            try self.out.writeRawStr(self.tree.string(h.value));
            try self.out.writeByte('\n');
        }
        try self.printStmtList(p.body, true);
        self.pending_semi = false;
        if (self.options.comments != .none) try self.emitInsideComments(self.current_idx);
    }

    fn printStmtList(self: *Self, items: IndexRange, prologue: bool) Error!void {
        var first = true;
        var prol = prologue;
        for (self.tree.extra(items)) |s| {
            if (self.options.strip and self.stripsToNothing(s)) {
                try self.emitNothing(s);
                continue;
            }
            if (!first) try self.newline();
            try self.flushSemi();
            self.in_prologue = prol;
            try self.emit(s);
            first = false;
            if (prol and self.nodeData(s) != .directive) prol = false;
        }
        self.in_prologue = false;
    }

    fn printIndentedStmtList(self: *Self, items: IndexRange, prologue: bool) Error!bool {
        std.debug.assert(items.len > 0);
        const prints = !self.options.strip or self.anyPrints(items);
        self.indent_depth += 1;
        defer self.indent_depth -= 1;
        if (prints) try self.newline();
        try self.printStmtList(items, prologue);
        return prints;
    }

    fn anyPrints(self: *const Self, items: IndexRange) bool {
        for (self.tree.extra(items)) |s| if (!self.stripsToNothing(s)) return true;
        return false;
    }

    fn printBlock(self: *Self, items: IndexRange, prologue: bool) Error!void {
        try self.out.writeByte('{');
        if (items.len > 0) {
            if (try self.printIndentedStmtList(items, prologue)) {
                self.pending_semi = false;
                try self.newline();
            }
        } else if (self.options.comments != .none) {
            try self.emitInsideComments(self.current_idx);
        }
        try self.out.writeByte('}');
    }

    fn emitInsideComments(self: *Self, idx: NodeIndex) Error!void {
        var any = false;
        for (self.tree.commentsOf(idx)) |c| {
            if (c.position != .inside or !self.allowComment(c)) continue;
            if (!any) {
                any = true;
                self.indent_depth += 1;
            }
            try self.breakLine();
            try self.writeCommentBody(c);
        }
        if (any) {
            self.indent_depth -= 1;
            try self.breakLine();
        }
    }

    // a line comment breaks the node open like a block
    fn emitInsideCommentsInline(self: *Self, idx: NodeIndex) Error!void {
        if (self.options.comments == .none) return;
        const comments = self.tree.commentsOf(idx);
        var any = false;
        for (comments) |c| {
            if (c.position != .inside or !self.allowComment(c)) continue;
            if (c.type == .line) return self.emitInsideComments(idx);
            any = true;
        }
        if (!any) return;
        for (comments) |c| {
            if (c.position != .inside or !self.allowComment(c)) continue;
            if (self.pretty() and needsSpaceBeforeInlineComment(self.out.lastByte())) {
                try self.out.writeByte(' ');
            }
            try self.writeCommentBody(c);
        }
    }

    // compact mode defers `;` so a closing `}` can drop it
    inline fn softSemi(self: *Self) Error!void {
        if (self.pretty()) try self.out.writeByte(';') else self.pending_semi = true;
    }

    inline fn flushSemi(self: *Self) Error!void {
        if (self.pending_semi) {
            self.pending_semi = false;
            try self.out.writeByte(';');
        }
    }

    fn emit_block_statement(self: *Self, s: *const ast.BlockStatement) Error!void {
        try self.printBlock(s.body, false);
    }

    fn emit_function_body(self: *Self, b: *const ast.FunctionBody) Error!void {
        try self.printBlock(b.body, true);
    }

    fn emit_static_block(self: *Self, b: *const ast.StaticBlock) Error!void {
        try self.out.writeStr("static");
        try self.out.space();
        try self.printBlock(b.body, false);
    }

    fn emit_directive(self: *Self, d: *const ast.Directive) Error!void {
        // an escaped `"use strict"` does not enable strict mode, so the raw lexeme must survive
        switch (self.nodeData(d.expression)) {
            .string_literal => |lit| {
                try self.writeNodeText(d.expression, self.tree.string(lit.raw));
            },
            else => try self.emit(d.expression),
        }
        try self.softSemi();
    }

    fn emit_empty_statement(self: *Self, _: *const ast.EmptyStatement) Error!void {
        // not deferred, `if(x);` needs the `;` to materialize the body
        try self.out.writeByte(';');
    }

    fn emit_debugger_statement(self: *Self, _: *const ast.DebuggerStatement) Error!void {
        try self.out.writeStr("debugger");
        try self.emitInsideCommentsInline(self.current_idx);
        try self.softSemi();
    }

    fn emit_expression_statement(self: *Self, s: *const ast.ExpressionStatement) Error!void {
        const as_directive = self.in_prologue and self.nodeData(s.expression) == .string_literal;
        self.in_prologue = false;
        if (as_directive) {
            try self.out.writeByte('(');
            try self.emit(s.expression);
            try self.out.writeByte(')');
            try self.softSemi();
            return;
        }
        self.out.lead = .stmt;
        try self.emitExpr(s.expression, .{});
        try self.softSemi();
    }

    fn emit_if_statement(self: *Self, s: *const ast.IfStatement) Error!void {
        try self.out.writeStr("if");
        try self.out.space();
        try self.out.writeByte('(');
        try self.emit(s.@"test");
        try self.out.writeByte(')');
        try self.out.space();
        try self.emitStmt(s.consequent);
        if (s.alternate != .null) {
            try self.flushSemi();
            try self.out.space();
            try self.out.writeStr("else ");
            try self.emitStmt(s.alternate);
        }
    }

    fn emit_return_statement(self: *Self, s: *const ast.ReturnStatement) Error!void {
        try self.out.writeStr("return");
        try self.emitInsideCommentsInline(self.current_idx);
        try self.emitRestrictedArg(s.argument, .{});
        try self.softSemi();
    }

    fn emit_throw_statement(self: *Self, s: *const ast.ThrowStatement) Error!void {
        try self.out.writeStr("throw");
        try self.emitRestrictedArg(s.argument, .{});
        try self.softSemi();
    }

    // asi would split an operand whose first token follows a line terminator, so that opens a
    // paren the operand closes
    fn emitRestrictedArg(self: *Self, idx: NodeIndex, ctx: Ctx) Error!void {
        if (idx == .null) return;
        try self.out.writeByte(' ');
        var paren_open = false;
        self.restricted = &paren_open;
        self.restricted_len = self.out.len();
        try self.emitExpr(idx, ctx);
        self.restricted = null;
        if (paren_open) try self.out.writeByte(')');
    }

    inline fn restrictedArmed(self: *Self) bool {
        if (self.restricted == null) return false;
        if (self.out.len() == self.restricted_len) return true;
        self.restricted = null;
        return false;
    }

    fn openRestrictedParen(self: *Self) Error!void {
        const paren_open = self.restricted.?;
        std.debug.assert(!paren_open.*);
        paren_open.* = true;
        self.restricted = null;
        // the keyword's separator space becomes part of ` (`
        self.out.held_spaces = 0;
        try self.out.writeStr(" (");
    }

    fn emit_break_statement(self: *Self, s: *const ast.BreakStatement) Error!void {
        try self.printJump("break", s.label);
    }

    fn emit_continue_statement(self: *Self, s: *const ast.ContinueStatement) Error!void {
        try self.printJump("continue", s.label);
    }

    fn printJump(self: *Self, keyword: []const u8, label: NodeIndex) Error!void {
        try self.out.writeStr(keyword);
        try self.emitInsideCommentsInline(self.current_idx);
        if (label != .null) {
            try self.out.writeByte(' ');
            try self.emit(label);
        }
        try self.softSemi();
    }

    fn emit_labeled_statement(self: *Self, s: *const ast.LabeledStatement) Error!void {
        try self.emit(s.label);
        try self.out.writeByte(':');
        try self.out.space();
        try self.emitStmt(s.body);
    }

    fn emit_with_statement(self: *Self, s: *const ast.WithStatement) Error!void {
        try self.out.writeStr("with");
        try self.out.space();
        try self.out.writeByte('(');
        try self.emit(s.object);
        try self.out.writeByte(')');
        try self.out.space();
        try self.emitStmt(s.body);
    }

    fn emit_while_statement(self: *Self, s: *const ast.WhileStatement) Error!void {
        try self.out.writeStr("while");
        try self.out.space();
        try self.out.writeByte('(');
        try self.emit(s.@"test");
        try self.out.writeByte(')');
        try self.out.space();
        try self.emitStmt(s.body);
    }

    fn emit_do_while_statement(self: *Self, s: *const ast.DoWhileStatement) Error!void {
        try self.out.writeStr("do ");
        try self.emitStmt(s.body);
        try self.flushSemi();
        try self.out.space();
        try self.out.writeStr("while");
        try self.out.space();
        try self.out.writeByte('(');
        try self.emit(s.@"test");
        try self.out.writeStr(");");
    }

    fn emit_for_statement(self: *Self, s: *const ast.ForStatement) Error!void {
        try self.out.writeStr("for");
        try self.out.space();
        try self.out.writeByte('(');
        if (s.init != .null) switch (self.nodeData(s.init)) {
            .variable_declaration => |d| try self.printForDeclaration(s.init, d, true),
            else => try self.emitExpr(s.init, .{ .no_in = true }),
        };
        try self.out.writeByte(';');
        if (s.@"test" != .null) {
            try self.out.space();
            try self.emit(s.@"test");
        }
        try self.out.writeByte(';');
        if (s.update != .null) {
            try self.out.space();
            try self.emit(s.update);
        }
        try self.out.writeByte(')');
        try self.out.space();
        try self.emitStmt(s.body);
    }

    fn emit_for_in_statement(self: *Self, s: *const ast.ForInStatement) Error!void {
        try self.out.writeStr("for");
        try self.out.space();
        try self.out.writeByte('(');
        try self.printForLeft(s.left);
        try self.out.writeStr(" in ");
        try self.emit(s.right);
        try self.out.writeByte(')');
        try self.out.space();
        try self.emitStmt(s.body);
    }

    fn emit_for_of_statement(self: *Self, s: *const ast.ForOfStatement) Error!void {
        try self.out.writeStr("for");
        if (s.await) try self.out.writeStr(" await");
        try self.out.space();
        try self.out.writeByte('(');
        // `for (async of …)` is forbidden, it could start the arrow `async of => …`
        const wrap_async = !s.await and isNamed(self.tree, s.left, "async");
        if (wrap_async) try self.out.writeByte('(');
        try self.printForLeft(s.left);
        if (wrap_async) try self.out.writeByte(')');
        try self.out.writeStr(" of ");
        try self.emitValue(s.right);
        try self.out.writeByte(')');
        try self.out.space();
        try self.emitStmt(s.body);
    }

    fn printForLeft(self: *Self, idx: NodeIndex) Error!void {
        switch (self.nodeData(idx)) {
            .variable_declaration => |d| try self.printForDeclaration(idx, d, false),
            else => try self.emitAssignTarget(idx, .{}),
        }
    }

    fn printForDeclaration(
        self: *Self,
        idx: NodeIndex,
        d: ast.VariableDeclaration,
        no_in: bool,
    ) Error!void {
        const scope = try self.openComments(idx);
        try self.printVariableDecl(d, false, no_in);
        try self.closeComments(idx, scope, false);
    }

    fn emit_switch_statement(self: *Self, s: *const ast.SwitchStatement) Error!void {
        try self.out.writeStr("switch");
        try self.out.space();
        try self.out.writeByte('(');
        try self.emit(s.discriminant);
        try self.out.writeByte(')');
        try self.out.space();
        try self.out.writeByte('{');
        const cases = self.tree.extra(s.cases);
        if (cases.len > 0) {
            for (cases) |c| {
                try self.flushSemi();
                try self.newline();
                try self.emit(c);
            }
            self.pending_semi = false;
            try self.newline();
        }
        try self.out.writeByte('}');
    }

    fn emit_switch_case(self: *Self, c: *const ast.SwitchCase) Error!void {
        if (c.@"test" != .null) {
            try self.out.writeStr("case ");
            try self.emit(c.@"test");
            try self.out.writeByte(':');
        } else {
            try self.out.writeStr("default");
            try self.emitInsideCommentsInline(self.current_idx);
            try self.out.writeByte(':');
        }
        if (c.consequent.len == 0) return;
        _ = try self.printIndentedStmtList(c.consequent, false);
    }

    fn emit_try_statement(self: *Self, s: *const ast.TryStatement) Error!void {
        try self.out.writeStr("try ");
        try self.emit(s.block);
        if (s.handler != .null) {
            try self.out.space();
            try self.emit(s.handler);
        }
        if (s.finalizer != .null) {
            try self.out.space();
            try self.out.writeStr("finally ");
            try self.emit(s.finalizer);
        }
    }

    fn emit_catch_clause(self: *Self, c: *const ast.CatchClause) Error!void {
        try self.out.writeStr("catch");
        if (c.param != .null) {
            try self.out.space();
            try self.out.writeByte('(');
            try self.emit(c.param);
            try self.out.writeByte(')');
        }
        try self.out.space();
        try self.emit(c.body);
    }

    fn emit_variable_declaration(self: *Self, d: *const ast.VariableDeclaration) Error!void {
        if (self.options.strip) if (isAmbient(self.tree, d.*)) return;
        try self.printVariableDecl(d.*, true, false);
    }

    fn printVariableDecl(
        self: *Self,
        d: ast.VariableDeclaration,
        with_semicolon: bool,
        no_in: bool,
    ) Error!void {
        if (!self.options.strip) if (d.declare) try self.out.writeStr("declare ");
        try self.out.writeStr(d.kind.toString());
        try self.out.writeByte(' ');
        const prev = self.decl_no_in;
        self.decl_no_in = no_in;
        defer self.decl_no_in = prev;
        try self.emitList(d.declarators);
        if (with_semicolon) try self.softSemi();
    }

    fn emit_variable_declarator(self: *Self, d: *const ast.VariableDeclarator) Error!void {
        if (!self.options.strip) self.definite_pending = d.definite;
        try self.emit(d.id);
        std.debug.assert(!self.definite_pending);
        if (d.init != .null) {
            try self.printEq();
            try self.emitExpr(d.init, .{ .prec = Precedence.Assignment, .no_in = self.decl_no_in });
        }
    }

    fn emit_sequence_expression(
        self: *Self,
        e: *const ast.SequenceExpression,
        ctx: Ctx,
    ) Error!void {
        const expression_ctx: Ctx = .{ .prec = Precedence.Assignment, .no_in = ctx.no_in };
        try self.emitItems(self.tree.extra(e.expressions), .null, expression_ctx);
    }

    fn emit_parenthesized_expression(
        self: *Self,
        e: *const ast.ParenthesizedExpression,
    ) Error!void {
        try self.out.writeByte('(');
        try self.emit(e.expression);
        try self.out.writeByte(')');
    }

    fn logicalOperandPrecedence(
        self: *const Self,
        child: NodeIndex,
        min: u8,
        parent: ast.LogicalOperator,
    ) u8 {
        const mismatch = logicalMismatch(self.tree, parent, self.stripped(child));
        return if (mismatch) Precedence.Grouping else min;
    }

    fn emit_conditional_expression(
        self: *Self,
        e: *const ast.ConditionalExpression,
        ctx: Ctx,
    ) Error!void {
        try self.emitExpr(e.@"test", .{ .prec = Precedence.LogicalOr, .no_in = ctx.no_in });
        try self.out.space();
        try self.out.writeByte('?');
        try self.out.space();
        try self.emitExpr(e.consequent, .{ .prec = Precedence.Assignment });
        try self.out.space();
        try self.out.writeByte(':');
        try self.out.space();
        try self.emitExpr(e.alternate, .{ .prec = Precedence.Assignment, .no_in = ctx.no_in });
    }

    fn emit_unary_expression(self: *Self, e: *const ast.UnaryExpression, ctx: Ctx) Error!void {
        const op = e.operator.toString();
        try self.out.writeStr(op);
        if (utils.isWordOp(op)) try self.out.writeByte(' ');
        try self.emitExpr(e.argument, .{
            .prec = Precedence.Unary,
            .no_instantiation = ctx.no_instantiation,
        });
    }

    fn emit_update_expression(self: *Self, e: *const ast.UpdateExpression) Error!void {
        const op = e.operator.toString();
        if (e.prefix) {
            try self.out.writeStr(op);
            try self.emitAssignTarget(e.argument, .{});
        } else {
            try self.emitAssignTarget(e.argument, .{});
            try self.out.writeStr(op);
        }
    }

    fn emit_assignment_expression(
        self: *Self,
        e: *const ast.AssignmentExpression,
        ctx: Ctx,
    ) Error!void {
        try self.emitAssignTarget(e.left, .{});
        try self.out.space();
        try self.out.writeStr(e.operator.toString());
        try self.out.space();
        try self.emitExpr(e.right, .{ .prec = Precedence.Assignment, .no_in = ctx.no_in });
    }

    fn emitAssignTarget(self: *Self, idx: NodeIndex, ctx: Ctx) Error!void {
        if (idx == .null) return;
        const prev = self.in_assign_target;
        defer self.in_assign_target = prev;

        if (needsParensAsAssignTarget(self.tree, idx)) {
            self.in_assign_target = false;
            try self.out.writeByte('(');
            try self.emitExpr(idx, ctx);
            try self.out.writeByte(')');
        } else {
            self.in_assign_target = true;
            try self.emitExpr(idx, ctx);
        }
    }

    inline fn emitChildOfAssignTarget(self: *Self, idx: NodeIndex) Error!void {
        if (self.in_assign_target) try self.emitAssignTarget(idx, .{}) else try self.emitValue(idx);
    }

    inline fn emitValue(self: *Self, idx: NodeIndex) Error!void {
        try self.emitExpr(idx, .{ .prec = Precedence.Assignment });
    }

    fn emit_array_expression(self: *Self, e: *const ast.ArrayExpression) Error!void {
        const in_target = self.in_assign_target;
        self.in_assign_target = false;
        defer self.in_assign_target = in_target;

        try self.out.writeByte('[');
        try self.emitInsideCommentsInline(self.current_idx);
        const list = self.tree.extra(e.elements);
        const depth = self.indent_depth;
        for (list, 0..) |x, i| {
            if (in_target) {
                try self.emitAssignTarget(x, .{ .defer_trailing = true });
            } else {
                try self.emitExpr(x, .{ .prec = Precedence.Assignment, .defer_trailing = true });
            }
            try self.closeItem(i + 1 < list.len, depth);
        }
        try self.closeList(depth);
        // a trailing hole needs its own comma, else `[a,]` is one element
        if (list.len > 0 and list[list.len - 1] == .null) try self.out.writeByte(',');
        try self.out.writeByte(']');
    }

    fn emit_object_expression(self: *Self, e: *const ast.ObjectExpression) Error!void {
        const in_target = self.in_assign_target;
        defer self.in_assign_target = in_target;

        try self.out.writeByte('{');
        try self.emitInsideCommentsInline(self.current_idx);
        const list = self.tree.extra(e.properties);
        if (list.len > 0) {
            try self.out.space();
            const depth = self.indent_depth;
            for (list, 0..) |x, i| {
                self.in_assign_target = in_target;
                try self.emitExpr(x, .{ .defer_trailing = true });
                try self.closeItem(i + 1 < list.len, depth);
            }
            try self.closeList(depth);
            if (!self.out.atLineStart()) try self.out.space();
        }
        try self.out.writeByte('}');
    }

    fn emit_object_property(self: *Self, p: *const ast.ObjectProperty) Error!void {
        if (p.method or p.kind == .get or p.kind == .set) {
            const fn_data = self.nodeData(p.value).function;
            switch (p.kind) {
                .get => try self.out.writeStr("get "),
                .set => try self.out.writeStr("set "),
                .init => {
                    if (fn_data.async) try self.out.writeStr("async ");
                    if (fn_data.generator) try self.out.writeByte('*');
                },
            }
            try self.printObjectKey(p.key, p.computed);
            try self.emitMethodValue(p.value);
            return;
        }

        if (p.shorthand and shorthandStillValid(self.tree, p.key, p.value)) {
            try self.emitChildOfAssignTarget(p.value);
            return;
        }

        try self.printObjectKey(p.key, p.computed);
        try self.out.writeByte(':');
        try self.out.space();
        try self.emitChildOfAssignTarget(p.value);
    }

    fn emit_spread_element(self: *Self, s: *const ast.SpreadElement) Error!void {
        try self.out.writeStr("...");
        try self.emitValue(s.argument);
    }

    fn emit_new_expression(self: *Self, e: *const ast.NewExpression) Error!void {
        try self.out.writeStr("new ");
        try self.emitExpr(e.callee, .{
            .prec = Precedence.New,
            .no_call = true,
            .no_instantiation = true,
        });
        try self.emit(e.type_arguments);
        try self.printArgList(e.arguments);
    }

    fn emit_await_expression(self: *Self, e: *const ast.AwaitExpression, ctx: Ctx) Error!void {
        try self.out.writeStr("await ");
        try self.emitExpr(e.argument, .{
            .prec = Precedence.Unary,
            .no_instantiation = ctx.no_instantiation,
        });
    }

    fn emit_yield_expression(self: *Self, e: *const ast.YieldExpression, ctx: Ctx) Error!void {
        try self.out.writeStr("yield");
        if (e.delegate) try self.out.writeByte('*');
        try self.emitRestrictedArg(e.argument, .{
            .prec = Precedence.Assignment,
            .no_in = ctx.no_in,
        });
    }

    fn emit_meta_property(self: *Self, p: *const ast.MetaProperty) Error!void {
        try self.emit(p.meta);
        try self.out.writeByte('.');
        try self.emit(p.property);
    }

    fn printArgList(self: *Self, args: IndexRange) Error!void {
        try self.out.writeByte('(');
        try self.emitItems(self.tree.extra(args), .null, .{ .prec = Precedence.Assignment });
        try self.out.writeByte(')');
    }

    fn emit_string_literal(self: *Self, lit: *const ast.StringLiteral) Error!void {
        const raw = self.tree.string(lit.raw);
        const value = self.tree.string(lit.value);
        const q = self.pickQuote(value, raw.len != 0 and raw[0] == '\'');
        try self.out.writeByte(q);
        try self.writeEscapedString(value, q);
        try self.out.writeRawByte(q);
    }

    inline fn pickQuote(self: *const Self, s: []const u8, single_quoted: bool) u8 {
        return switch (self.options.quotes) {
            .preserve => if (single_quoted) '\'' else '"',
            .single => '\'',
            .double => '"',
            .shortest => blk: {
                const single = std.mem.count(u8, s, "'");
                const double = std.mem.count(u8, s, "\"");
                break :blk if (single < double) '\'' else '"';
            },
        };
    }

    fn writeEscapedString(self: *Self, s: []const u8, quote: u8) Error!void {
        var start: usize = 0;
        var i: usize = 0;
        while (i < s.len) : (i += 1) {
            i += plainRunLength(s, i, quote, self.options.minify);
            if (i == s.len) break;
            const c = s[i];
            if (c >= 0x80) {
                if (c == 0xED) {
                    if (util.Utf.loneSurrogateAt(s, i)) |cp| {
                        if (i > start) try self.out.writeRawStr(s[start..i]);
                        try self.writeUnicodeEscape(cp);
                        i += 2;
                        start = i + 1;
                    }
                }
                continue;
            }
            const esc: ?[]const u8 = blk: {
                if (self.options.minify) {
                    if (utils.scriptEscape(s, i)) |e| break :blk e;
                }
                break :blk switch (c) {
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
            };
            if (esc) |e| {
                if (i > start) try self.out.writeRawStr(s[start..i]);
                try self.out.writeRawStr(e);
                start = i + 1;
            }
        }
        if (start < s.len) try self.out.writeRawStr(s[start..]);
    }

    inline fn plainRunLength(s: []const u8, from: usize, quote: u8, minify: bool) usize {
        std.debug.assert(from <= s.len);
        var i = from;
        while (i + 16 <= s.len) {
            const chunk = Simd.loadChunk(s, i);
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

    fn writeUnicodeEscape(self: *Self, cp: u32) Error!void {
        const hex = "0123456789abcdef";
        const buf = [_]u8{
            '\\',                  'u',
            hex[(cp >> 12) & 0xF], hex[(cp >> 8) & 0xF],
            hex[(cp >> 4) & 0xF],  hex[cp & 0xF],
        };
        try self.out.writeRawStr(&buf);
    }

    fn emit_numeric_literal(self: *Self, lit: *const ast.NumericLiteral) Error!void {
        if (self.options.minify) return self.writeShortestNumber(lit.*);
        try self.writeNumber(self.tree.string(lit.raw));
    }

    inline fn writeNumber(self: *Self, text: []const u8) Error!void {
        try self.out.writeStr(text);
        if (isBareInteger(text)) self.out.markBareInteger();
    }

    // out of line so its buffers stay off the recursion's frame
    noinline fn writeShortestNumber(self: *Self, lit: ast.NumericLiteral) Error!void {
        const raw = self.tree.string(lit.raw);
        if (lit.kind == .decimal and utils.isMinimalInteger(raw)) return self.writeNumber(raw);
        var cleaned_buf: [128]u8 = undefined;
        const cleaned = utils.stripUnderscores(raw, &cleaned_buf) orelse {
            return self.writeNumber(raw);
        };
        if (lit.kind != .decimal) return self.writeNumber(cleaned);
        // `010` is legacy octal and `08` sloppy decimal
        if (cleaned.len > 1 and cleaned[0] == '0' and std.ascii.isDigit(cleaned[1])) {
            return self.writeNumber(cleaned);
        }
        var shortest_buf: [128]u8 = undefined;
        try self.writeNumber(utils.shortestDecimal(cleaned, &shortest_buf));
    }

    fn emit_bigint_literal(self: *Self, lit: *const ast.BigIntLiteral) Error!void {
        try self.writeString(lit.raw);
        try self.out.writeByte('n');
    }

    fn emit_boolean_literal(self: *Self, lit: *const ast.BooleanLiteral) Error!void {
        if (self.options.minify) {
            try self.out.writeStr(if (lit.value) "!0" else "!1");
        } else {
            try self.out.writeStr(if (lit.value) "true" else "false");
        }
    }

    fn emit_regexp_literal(self: *Self, lit: *const ast.RegExpLiteral) Error!void {
        try self.out.writeByte('/');
        try self.out.writeRawStr(self.tree.string(lit.pattern));
        try self.out.writeRawByte('/');
        try self.out.writeRawStr(self.tree.string(lit.flags));
    }

    fn emit_template_literal(self: *Self, lit: *const ast.TemplateLiteral, ctx: Ctx) Error!void {
        try self.printTemplate(lit.quasis, lit.expressions, ctx.tagged);
    }

    fn printTemplate(self: *Self, quasis: IndexRange, subs: IndexRange, tagged: bool) Error!void {
        try self.out.writeByte('`');
        const xs = self.tree.extra(subs);
        for (self.tree.extra(quasis), 0..) |q, i| {
            try self.printTemplateElement(self.tree.data(q).template_element, tagged);
            if (i < xs.len) {
                try self.out.writeRawStr("${");
                try self.emit(xs[i]);
                try self.out.writeByte('}');
            }
        }
        try self.out.writeRawByte('`');
    }

    fn printTemplateElement(self: *Self, el: ast.TemplateElement, tagged: bool) Error!void {
        const script_safe = self.options.minify and !tagged;
        const raw = self.tree.string(el.raw);
        if (raw.len != 0) return self.writeTemplateRaw(raw, script_safe);
        const s = self.tree.string(el.cooked);
        var i: usize = 0;
        var start: usize = 0;
        while (i < s.len) : (i += 1) {
            const c = s[i];
            if (util.Utf.loneSurrogateAt(s, i)) |cp| {
                if (i > start) try self.out.writeRawStr(s[start..i]);
                try self.writeUnicodeEscape(cp);
                i += 2;
                start = i + 1;
                continue;
            }
            const esc: ?[]const u8 = blk: {
                if (script_safe) {
                    if (utils.scriptEscape(s, i)) |e| break :blk e;
                }
                break :blk switch (c) {
                    '\\' => "\\\\",
                    '`' => "\\`",
                    '$' => if (i + 1 < s.len and s[i + 1] == '{') "\\$" else null,
                    '\r' => "\\r",
                    0 => if (i + 1 < s.len and std.ascii.isDigit(s[i + 1])) "\\x00" else "\\0",
                    else => null,
                };
            };
            if (esc) |e| {
                if (i > start) try self.out.writeRawStr(s[start..i]);
                try self.out.writeRawStr(e);
                start = i + 1;
            }
        }
        if (start < s.len) try self.out.writeRawStr(s[start..]);
    }

    // template text reads a raw `\r\n` or `\r` as `\n`
    fn writeTemplateRaw(self: *Self, raw: []const u8, script_safe: bool) Error!void {
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
            if (i > start) try self.out.writeRawStr(raw[start..i]);
            try self.out.writeRawStr(esc);
            if (raw[i] == '\r' and i + 1 < raw.len and raw[i + 1] == '\n') i += 1;
            start = i + 1;
        }
        if (start < raw.len) try self.out.writeRawStr(raw[start..]);
    }

    fn emit_identifier_reference(self: *Self, id: *const ast.IdentifierReference) Error!void {
        try self.writeString(id.name);
    }

    fn emit_identifier_name(self: *Self, id: *const ast.IdentifierName) Error!void {
        try self.writeString(id.name);
    }

    fn emit_binding_identifier(self: *Self, id: *const ast.BindingIdentifier) Error!void {
        const definite = self.takeDefinite();
        if (!self.options.strip) try self.printDecorators(id.decorators);
        try self.writeString(id.name);
        if (id.optional) try self.emitInsideCommentsInline(self.current_idx);
        try self.printBindingSuffix(id.optional, definite, id.type_annotation);
    }

    fn emit_label_identifier(self: *Self, id: *const ast.LabelIdentifier) Error!void {
        try self.writeString(id.name);
    }

    fn emit_private_identifier(self: *Self, id: *const ast.PrivateIdentifier) Error!void {
        try self.out.writeByte('#');
        try self.writeString(id.name);
    }

    fn emit_assignment_pattern(self: *Self, p: *const ast.AssignmentPattern) Error!void {
        if (!self.options.strip) try self.printDecorators(p.decorators);
        try self.emit(p.left);
        if (!self.options.strip) if (p.optional) try self.out.writeByte('?');
        try self.emit(p.type_annotation);
        try self.printEq();
        try self.emitValue(p.right);
    }

    fn emit_binding_rest_element(self: *Self, r: *const ast.BindingRestElement) Error!void {
        if (!self.options.strip) try self.printDecorators(r.decorators);
        try self.out.writeStr("...");
        try self.emit(r.argument);
        if (!self.options.strip) if (r.optional) try self.out.writeByte('?');
        try self.emit(r.type_annotation);
    }

    fn emit_array_pattern(self: *Self, p: *const ast.ArrayPattern) Error!void {
        const definite = self.takeDefinite();
        if (!self.options.strip) try self.printDecorators(p.decorators);
        try self.out.writeByte('[');
        try self.emitInsideCommentsInline(self.current_idx);
        const list = self.tree.extra(p.elements);
        const depth = self.indent_depth;
        for (list, 0..) |x, i| {
            try self.emitAssignTarget(x, .{ .defer_trailing = true });
            try self.closeItem(i + 1 < list.len or p.rest != .null, depth);
        }
        if (p.rest != .null) {
            try self.emitExpr(p.rest, .{ .defer_trailing = true });
            try self.closeItem(false, depth);
        }
        try self.closeList(depth);
        if (p.rest == .null and list.len > 0 and list[list.len - 1] == .null) {
            // a trailing hole needs its own comma, else `[a,]` is one element
            try self.out.writeByte(',');
        }
        try self.out.writeByte(']');
        try self.printBindingSuffix(p.optional, definite, p.type_annotation);
    }

    fn emit_object_pattern(self: *Self, p: *const ast.ObjectPattern) Error!void {
        const definite = self.takeDefinite();
        if (!self.options.strip) try self.printDecorators(p.decorators);
        try self.out.writeByte('{');
        try self.emitInsideCommentsInline(self.current_idx);
        const list = self.tree.extra(p.properties);
        const has_any = list.len > 0 or p.rest != .null;
        if (has_any) try self.out.space();
        try self.emitItems(list, p.rest, .{});
        if (has_any and !self.out.atLineStart()) try self.out.space();
        try self.out.writeByte('}');
        try self.printBindingSuffix(p.optional, definite, p.type_annotation);
    }

    fn emit_binding_property(self: *Self, p: *const ast.BindingProperty) Error!void {
        if (p.shorthand and shorthandStillValid(self.tree, p.key, p.value)) {
            try self.emitAssignTarget(p.value, .{});
            return;
        }
        try self.printObjectKey(p.key, p.computed);
        try self.out.writeByte(':');
        try self.out.space();
        try self.emitAssignTarget(p.value, .{});
    }

    fn printPropertyKey(self: *Self, key: NodeIndex, computed: bool) Error!void {
        if (computed) {
            try self.out.writeByte('[');
            try self.emitValue(key);
            try self.out.writeByte(']');
        } else {
            try self.emit(key);
        }
    }

    // computed `["__proto__"]` defines an own property, bare `__proto__` sets the prototype
    fn printObjectKey(self: *Self, key: NodeIndex, computed: bool) Error!void {
        if (self.options.minify) {
            if (simpleStringKey(self.tree, key)) |s| {
                const proto_clash = computed and std.mem.eql(u8, s, "__proto__");
                if (!proto_clash) return self.writeNodeText(key, s);
            }
        }
        try self.printPropertyKey(key, computed);
    }

    // `["constructor"]` would become the constructor or a SyntaxError, `static ["prototype"]` too
    fn printClassKey(
        self: *Self,
        key: NodeIndex,
        computed: bool,
        static: bool,
        is_field: bool,
    ) Error!void {
        if (self.options.minify) {
            if (simpleStringKey(self.tree, key)) |s| {
                if (!computed) return self.writeNodeText(key, s);
                const ctor_clash = std.mem.eql(u8, s, "constructor") and (is_field or !static);
                const proto_clash = static and std.mem.eql(u8, s, "prototype");
                if (!ctor_clash and !proto_clash) return self.writeNodeText(key, s);
            }
        }
        try self.printPropertyKey(key, computed);
    }

    fn emit_function(self: *Self, f: *const ast.Function) Error!void {
        if (self.options.strip) {
            const is_ts_only = f.declare or
                f.type == .ts_declare_function or
                f.type == .ts_empty_body_function_expression;
            if (is_ts_only) return;
        }
        if (!self.options.strip) if (f.declare) try self.out.writeStr("declare ");
        if (f.async) try self.out.writeStr("async ");
        try self.out.writeStr("function");
        if (f.generator) try self.out.writeByte('*');
        if (f.id != .null) {
            try self.out.writeByte(' ');
            try self.emit(f.id);
        }
        try self.printFunctionAsMethod(f.*);
    }

    fn emitMethodValue(self: *Self, idx: NodeIndex) Error!void {
        const scope = try self.openComments(idx);
        try self.printFunctionAsMethod(self.nodeData(idx).function);
        try self.closeComments(idx, scope, false);
    }

    fn printFunctionAsMethod(self: *Self, f: ast.Function) Error!void {
        try self.emit(f.type_parameters);
        try self.printParams(f.params);
        try self.emit(f.return_type);
        if (f.body != .null) {
            try self.out.space();
            try self.emit(f.body);
        } else if (!self.options.strip) {
            try self.softSemi();
        }
    }

    fn emit_arrow_function_expression(
        self: *Self,
        a: *const ast.ArrowFunctionExpression,
        ctx: Ctx,
    ) Error!void {
        if (a.async) try self.out.writeStr("async ");
        try self.emitExpr(a.type_parameters, .{ .no_jsx_tag = true });
        try self.printParams(a.params);
        try self.emitExpr(a.return_type, .{ .defer_trailing = true });
        try self.out.space();
        try self.out.writeStr("=>");
        try self.emitOwedComments();
        if (!self.out.atLineStart()) try self.out.space();
        if (a.expression) {
            self.out.lead = .arrow;
            try self.emitExpr(a.body, .{ .prec = Precedence.Assignment, .no_in = ctx.no_in });
        } else {
            try self.emit(a.body);
        }
    }

    fn printParams(self: *Self, idx: NodeIndex) Error!void {
        std.debug.assert(self.tree.commentsOf(idx).len == 0);
        const params = self.tree.data(idx).formal_parameters;
        try self.out.writeByte('(');
        try self.emitKeptItems(self.tree.extra(params.items), params.rest);
        try self.emitInsideCommentsInline(self.current_idx);
        try self.out.writeByte(')');
    }

    fn emitKeptItems(self: *Self, items: []const NodeIndex, rest: NodeIndex) Error!void {
        var end = items.len;
        if (self.options.strip) {
            while (end > 0 and self.stripsToNothing(parameterOf(self.tree, items[end - 1]))) {
                end -= 1;
            }
        }
        const depth = self.indent_depth;
        for (items, 0..) |item, i| {
            const node = parameterOf(self.tree, item);
            if (self.options.strip and self.stripsToNothing(node)) {
                try self.emitNothing(node);
                continue;
            }
            try self.emitExpr(node, .{ .defer_trailing = true });
            try self.closeItem(i + 1 < end or rest != .null, depth);
        }
        if (rest != .null) {
            try self.emitExpr(rest, .{ .defer_trailing = true });
            try self.closeItem(false, depth);
        }
        try self.closeList(depth);
    }

    fn emit_class(self: *Self, c: *const ast.Class, ctx: Ctx) Error!void {
        if (self.options.strip) if (c.declare) return;
        if (!ctx.no_decorators) try self.printDecorators(c.decorators);
        if (!self.options.strip) {
            if (c.declare) try self.out.writeStr("declare ");
            if (c.abstract) try self.out.writeStr("abstract ");
        }
        try self.out.writeStr("class");
        if (c.id != .null) {
            try self.out.writeByte(' ');
            try self.emit(c.id);
        }
        try self.emit(c.type_parameters);
        if (c.super_class != .null) {
            try self.out.writeStr(" extends ");
            try self.emitExpr(c.super_class, .{
                .prec = Precedence.Call,
                .no_instantiation = true,
            });
            try self.emit(c.super_type_arguments);
        }
        if (!self.options.strip) {
            if (self.tree.extra(c.implements).len > 0) {
                try self.out.writeStr(" implements ");
                try self.emitList(c.implements);
            }
        }
        try self.out.space();
        try self.emit(c.body);
    }

    fn emit_class_body(self: *Self, b: *const ast.ClassBody) Error!void {
        try self.out.writeByte('{');
        self.indent_depth += 1;
        var any = false;
        for (self.tree.extra(b.body)) |m| {
            if (self.options.strip and self.stripsToNothing(m)) {
                try self.emitNothing(m);
                continue;
            }
            try self.flushSemi();
            try self.newline();
            try self.emit(m);
            any = true;
        }
        self.indent_depth -= 1;
        if (any) {
            self.pending_semi = false;
            try self.newline();
        } else if (self.options.comments != .none) {
            try self.emitInsideComments(self.current_idx);
        }
        try self.out.writeByte('}');
    }

    fn emit_method_definition(self: *Self, m: *const ast.MethodDefinition) Error!void {
        const fn_data = self.nodeData(m.value).function;
        if (self.options.strip) if (m.abstract or fn_data.body == .null) return;
        try self.printDecorators(m.decorators);
        try self.hoistKeyComments(m.key);
        defer self.skip_leading_of = .null;
        if (!self.options.strip) if (m.accessibility != .none) {
            try self.out.writeStr(m.accessibility.toString());
            try self.out.writeByte(' ');
        };
        if (m.static) try self.out.writeStr("static ");
        if (!self.options.strip) {
            if (m.abstract) try self.out.writeStr("abstract ");
            if (m.override) try self.out.writeStr("override ");
        }
        switch (m.kind) {
            .get => try self.out.writeStr("get "),
            .set => try self.out.writeStr("set "),
            .constructor, .method => {
                if (fn_data.async) try self.out.writeStr("async ");
                if (fn_data.generator) try self.out.writeByte('*');
            },
        }
        try self.printClassKey(m.key, m.computed, m.static, false);
        if (!self.options.strip) if (m.optional) try self.out.writeByte('?');
        try self.emitMethodValue(m.value);
    }

    fn emit_property_definition(self: *Self, p: *const ast.PropertyDefinition) Error!void {
        if (self.options.strip) if (p.declare or p.abstract) return;
        try self.printDecorators(p.decorators);
        try self.hoistKeyComments(p.key);
        defer self.skip_leading_of = .null;
        if (!self.options.strip) {
            if (p.declare) try self.out.writeStr("declare ");
            if (p.accessibility != .none) {
                try self.out.writeStr(p.accessibility.toString());
                try self.out.writeByte(' ');
            }
        }
        if (p.static) try self.out.writeStr("static ");
        if (!self.options.strip) {
            if (p.abstract) try self.out.writeStr("abstract ");
            if (p.override) try self.out.writeStr("override ");
            if (p.readonly) try self.out.writeStr("readonly ");
        }
        if (p.accessor) try self.out.writeStr("accessor ");
        if (!self.options.strip and p.definite) self.defer_trailing_of = p.key;
        try self.printClassKey(p.key, p.computed, p.static, true);
        self.defer_trailing_of = .null;
        if (!self.options.strip) {
            if (p.optional) try self.out.writeByte('?');
            if (p.definite) try self.out.writeByte('!');
        }
        try self.emitOwedComments();
        try self.emit(p.type_annotation);
        if (p.value != .null) {
            try self.printEq();
            try self.emitValue(p.value);
        }
        try self.softSemi();
    }

    fn emit_decorator(self: *Self, d: *const ast.Decorator) Error!void {
        try self.out.writeByte('@');
        const simple = self.decoratorIsSimple(d.expression);
        try self.emitExpr(d.expression, .{
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

    fn printDecorators(self: *Self, decs: IndexRange) Error!void {
        const list = self.tree.extra(decs);
        if (list.len == 0) return;
        const carry = self.out.map_start;
        for (list, 0..) |d, i| {
            try self.emit(d);
            // `@a.b class` would fuse to `@a.bclass` without a separator
            if (self.pretty()) {
                try self.newline();
            } else if (i + 1 == list.len) {
                try self.out.writeByte(' ');
            }
        }
        self.out.map_start = carry;
    }

    fn emit_import_declaration(self: *Self, d: *const ast.ImportDeclaration) Error!void {
        const list = self.tree.extra(d.specifiers);
        if (self.options.strip) {
            if (d.import_kind == .type) return;
            if (list.len > 0 and !hasValueImportSpecifier(self.tree, list)) return;
        }

        try self.out.writeStr("import");
        if (d.import_kind == .type) try self.out.writeStr(" type");
        if (d.phase) |ph| {
            try self.out.writeByte(' ');
            try self.out.writeStr(@tagName(ph));
        }

        if (list.len > 0) {
            try self.out.writeByte(' ');
            const depth = self.indent_depth;
            var i: usize = 0;
            if (self.nodeData(list[0]) == .import_default_specifier) {
                try self.emitExpr(list[0], .{ .defer_trailing = true });
                try self.closeItem(list.len > 1, depth);
                i = 1;
            }
            if (i < list.len) {
                if (self.nodeData(list[i]) == .import_namespace_specifier) {
                    try self.emit(list[i]);
                } else {
                    try self.out.writeByte('{');
                    try self.out.space();
                    try self.emitKeptItems(list[i..], .null);
                    if (!self.out.atLineStart()) try self.out.space();
                    try self.out.writeByte('}');
                }
            }
            try self.closeList(depth);
            try self.out.writeStr(" from ");
        } else {
            try self.out.writeByte(' ');
        }

        try self.emit(d.source);
        try self.printAttributes(d.attributes);
        try self.softSemi();
    }

    fn emit_import_specifier(self: *Self, s: *const ast.ImportSpecifier) Error!void {
        if (self.options.strip) std.debug.assert(s.import_kind != .type);
        if (s.import_kind == .type) try self.out.writeStr("type ");
        try self.emit(s.imported);
        const same = sameIdentifier(self.tree, s.imported, s.local);
        if (!same or self.hasPrintedComments(s.local)) {
            try self.out.writeStr(" as ");
            try self.emit(s.local);
        }
    }

    fn emit_import_default_specifier(self: *Self, s: *const ast.ImportDefaultSpecifier) Error!void {
        try self.emit(s.local);
    }

    fn emit_import_namespace_specifier(
        self: *Self,
        s: *const ast.ImportNamespaceSpecifier,
    ) Error!void {
        try self.out.writeStr("* as ");
        try self.emit(s.local);
    }

    fn emit_import_attribute(self: *Self, a: *const ast.ImportAttribute) Error!void {
        try self.emit(a.key);
        try self.out.writeByte(':');
        try self.out.space();
        try self.emit(a.value);
    }

    fn emit_import_expression(self: *Self, e: *const ast.ImportExpression) Error!void {
        try self.out.writeStr("import");
        if (e.phase) |ph| {
            try self.out.writeByte('.');
            try self.out.writeStr(@tagName(ph));
        }
        try self.out.writeByte('(');
        try self.emitValue(e.source);
        if (e.options != .null) {
            try self.out.writeByte(',');
            try self.out.space();
            try self.emitValue(e.options);
        }
        try self.out.writeByte(')');
    }

    fn emit_export_named_declaration(
        self: *Self,
        d: *const ast.ExportNamedDeclaration,
    ) Error!void {
        const list = self.tree.extra(d.specifiers);
        if (self.options.strip) {
            if (d.export_kind == .type) return;
            const no_value_specifiers = d.declaration == .null and list.len > 0 and
                !hasValueExportSpecifier(self.tree, list);
            if (no_value_specifiers) return;
        }

        if (self.options.strip and d.declaration != .null and self.stripsToNothing(d.declaration)) {
            return self.emitNothing(d.declaration);
        }
        const hoisted = self.decoratorsBeforeExport(d.declaration);
        if (hoisted) |decorators| try self.printDecorators(decorators);
        try self.out.writeStr("export");
        if (d.export_kind == .type and d.declaration == .null) try self.out.writeStr(" type");
        if (d.declaration != .null) {
            try self.out.writeByte(' ');
            return self.emitExpr(d.declaration, .{ .no_decorators = hoisted != null });
        }
        try self.out.space();
        try self.out.writeByte('{');
        try self.emitInsideCommentsInline(self.current_idx);
        if (list.len > 0) {
            try self.out.space();
            try self.emitKeptItems(list, .null);
            if (!self.out.atLineStart()) try self.out.space();
        }
        try self.out.writeByte('}');
        if (d.source != .null) {
            try self.out.writeStr(" from ");
            try self.emit(d.source);
        }
        try self.printAttributes(d.attributes);
        try self.softSemi();
    }

    fn emit_export_default_declaration(
        self: *Self,
        d: *const ast.ExportDefaultDeclaration,
    ) Error!void {
        if (self.nodeData(d.declaration).isDeclaration()) {
            if (self.options.strip and self.stripsToNothing(d.declaration)) {
                return self.emitNothing(d.declaration);
            }
            const hoisted = self.decoratorsBeforeExport(d.declaration);
            if (hoisted) |decorators| try self.printDecorators(decorators);
            try self.out.writeStr("export default ");
            return self.emitExpr(d.declaration, .{ .no_decorators = hoisted != null });
        }
        try self.out.writeStr("export default ");
        self.out.lead = .export_default;
        try self.emitExpr(d.declaration, .{ .prec = Precedence.Assignment });
        try self.softSemi();
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

    fn emit_export_all_declaration(self: *Self, d: *const ast.ExportAllDeclaration) Error!void {
        if (self.options.strip) if (d.export_kind == .type) return;
        try self.out.writeStr("export");
        if (d.export_kind == .type) try self.out.writeStr(" type");
        try self.out.writeStr(" *");
        if (d.exported != .null) {
            try self.out.writeStr(" as ");
            try self.emit(d.exported);
        }
        try self.out.writeStr(" from ");
        try self.emit(d.source);
        try self.printAttributes(d.attributes);
        try self.softSemi();
    }

    fn emit_export_specifier(self: *Self, s: *const ast.ExportSpecifier) Error!void {
        if (self.options.strip) std.debug.assert(s.export_kind != .type);
        if (s.export_kind == .type) try self.out.writeStr("type ");
        try self.emit(s.local);
        const same = sameIdentifier(self.tree, s.local, s.exported);
        if (!same or self.hasPrintedComments(s.exported)) {
            try self.out.writeStr(" as ");
            try self.emit(s.exported);
        }
    }

    fn printAttributes(self: *Self, attrs: IndexRange) Error!void {
        const list = self.tree.extra(attrs);
        if (list.len == 0) return;
        try self.out.writeStr(" with ");
        try self.out.writeByte('{');
        try self.out.space();
        try self.emitList(attrs);
        try self.out.space();
        try self.out.writeByte('}');
    }

    fn emit_ts_type_annotation(self: *Self, t: *const ast.TSTypeAnnotation) Error!void {
        try self.out.writeByte(':');
        try self.out.space();
        try self.emit(t.type_annotation);
    }

    fn emit_ts_type_reference(self: *Self, t: *const ast.TSTypeReference) Error!void {
        try self.emit(t.type_name);
        try self.emit(t.type_arguments);
    }

    fn emit_ts_qualified_name(self: *Self, q: *const ast.TSQualifiedName) Error!void {
        try self.emit(q.left);
        try self.out.writeByte('.');
        try self.emit(q.right);
    }

    fn emit_ts_type_query(self: *Self, q: *const ast.TSTypeQuery) Error!void {
        try self.out.writeStr("typeof ");
        try self.emit(q.expr_name);
        try self.emit(q.type_arguments);
    }

    fn emit_ts_import_type(self: *Self, t: *const ast.TSImportType) Error!void {
        try self.out.writeStr("import(");
        try self.emit(t.source);
        if (t.options != .null) {
            try self.out.writeByte(',');
            try self.out.space();
            try self.emit(t.options);
        }
        try self.out.writeByte(')');
        if (t.qualifier != .null) {
            try self.out.writeByte('.');
            try self.emit(t.qualifier);
        }
        try self.emit(t.type_arguments);
    }

    fn emit_ts_type_parameter(self: *Self, p: *const ast.TSTypeParameter) Error!void {
        if (p.@"const") try self.out.writeStr("const ");
        if (p.in) try self.out.writeStr("in ");
        if (p.out) try self.out.writeStr("out ");
        try self.emit(p.name);
        if (p.constraint != .null) {
            try self.out.writeStr(" extends ");
            try self.emit(p.constraint);
        }
        if (p.default != .null) {
            try self.printEq();
            try self.emit(p.default);
        }
    }

    fn emit_ts_type_parameter_declaration(
        self: *Self,
        d: *const ast.TSTypeParameterDeclaration,
        ctx: Ctx,
    ) Error!void {
        try self.out.writeByte('<');
        try self.emitList(d.params);
        // in TSX a lone `<T>` opens a JSX tag, and `<T,>` is valid TS too
        const params = self.tree.extra(d.params);
        if (ctx.no_jsx_tag and params.len == 1) {
            const param = self.tree.data(params[0]).ts_type_parameter;
            if (param.constraint == .null) try self.out.writeByte(',');
        }
        try self.out.writeByte('>');
    }

    fn emit_ts_type_parameter_instantiation(
        self: *Self,
        d: *const ast.TSTypeParameterInstantiation,
    ) Error!void {
        try self.out.writeByte('<');
        try self.emitList(d.params);
        try self.out.writeByte('>');
    }

    fn emit_ts_literal_type(self: *Self, t: *const ast.TSLiteralType) Error!void {
        // `!0` is no type
        switch (self.nodeData(t.literal)) {
            .boolean_literal => |b| try self.out.writeStr(if (b.value) "true" else "false"),
            else => try self.emit(t.literal),
        }
    }

    fn emit_ts_template_literal_type(self: *Self, t: *const ast.TSTemplateLiteralType) Error!void {
        try self.printTemplate(t.quasis, t.types, false);
    }

    fn emitType(self: *Self, idx: NodeIndex, floor: u8, ctx: Ctx) Error!void {
        try self.wrapIf(self.typePrec(idx) < floor, idx, ctx);
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

    fn emit_ts_array_type(self: *Self, t: *const ast.TSArrayType) Error!void {
        try self.emitType(t.element_type, TPrec.primary, .{ .defer_trailing = true });
        try self.out.writeByte('[');
        try self.emitOwedComments();
        try self.out.writeByte(']');
    }

    fn emit_ts_indexed_access_type(self: *Self, t: *const ast.TSIndexedAccessType) Error!void {
        try self.emitType(t.object_type, TPrec.primary, .{ .defer_trailing = true });
        try self.out.writeByte('[');
        try self.emitOwedComments();
        try self.emit(t.index_type);
        try self.out.writeByte(']');
    }

    fn emit_ts_tuple_type(self: *Self, t: *const ast.TSTupleType) Error!void {
        try self.out.writeByte('[');
        try self.emitInsideCommentsInline(self.current_idx);
        try self.emitList(t.element_types);
        try self.out.writeByte(']');
    }

    fn emit_ts_named_tuple_member(self: *Self, m: *const ast.TSNamedTupleMember) Error!void {
        try self.emit(m.label);
        if (m.optional) try self.out.writeByte('?');
        try self.out.writeByte(':');
        try self.out.space();
        try self.emit(m.element_type);
    }

    fn emit_ts_optional_type(self: *Self, t: *const ast.TSOptionalType) Error!void {
        try self.emitType(t.type_annotation, TPrec.primary, .{});
        try self.out.writeByte('?');
    }

    fn emit_ts_rest_type(self: *Self, t: *const ast.TSRestType) Error!void {
        try self.out.writeStr("...");
        try self.emit(t.type_annotation);
    }

    fn emit_ts_jsdoc_nullable_type(self: *Self, t: *const ast.TSJSDocNullableType) Error!void {
        try self.printJSDocNullability('?', t.type_annotation, t.postfix);
    }

    fn emit_ts_jsdoc_non_nullable_type(
        self: *Self,
        t: *const ast.TSJSDocNonNullableType,
    ) Error!void {
        try self.printJSDocNullability('!', t.type_annotation, t.postfix);
    }

    fn printJSDocNullability(self: *Self, marker: u8, idx: NodeIndex, postfix: bool) Error!void {
        if (!postfix) try self.out.writeByte(marker);
        try self.emit(idx);
        if (postfix) try self.out.writeByte(marker);
    }

    fn emit_ts_union_type(self: *Self, t: *const ast.TSUnionType) Error!void {
        try self.emitTypeList(t.types, '|');
    }

    fn emit_ts_intersection_type(self: *Self, t: *const ast.TSIntersectionType) Error!void {
        try self.emitTypeList(t.types, '&');
    }

    // a lone member keeps its leading operator (`type X = | A`) so reparse keeps the wrapper
    fn emitTypeList(self: *Self, types: IndexRange, comptime op: u8) Error!void {
        const list = self.tree.extra(types);
        if (list.len == 1) {
            try self.out.writeByte(op);
            try self.out.space();
        }
        const floor: u8 = if (op == '|') TPrec.intersection else TPrec.operator;
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.out.space();
                try self.out.writeByte(op);
                try self.out.space();
            }
            try self.emitType(x, floor, .{});
        }
    }

    fn emit_ts_conditional_type(self: *Self, t: *const ast.TSConditionalType) Error!void {
        try self.emitType(t.check_type, TPrec.@"union", .{ .defer_trailing = true });
        try self.writeKeywordThenOwed(" extends");
        try self.emitType(t.extends_type, TPrec.@"union", .{});
        try self.out.space();
        try self.out.writeByte('?');
        try self.out.space();
        try self.emit(t.true_type);
        try self.out.space();
        try self.out.writeByte(':');
        try self.out.space();
        try self.emit(t.false_type);
    }

    fn emit_ts_infer_type(self: *Self, t: *const ast.TSInferType) Error!void {
        try self.out.writeStr("infer ");
        try self.emit(t.type_parameter);
    }

    fn emit_ts_type_operator(self: *Self, t: *const ast.TSTypeOperator) Error!void {
        try self.out.writeStr(t.operator.toString());
        try self.out.writeByte(' ');
        try self.emitType(t.type_annotation, TPrec.operator, .{});
    }

    fn emit_ts_parenthesized_type(self: *Self, t: *const ast.TSParenthesizedType) Error!void {
        try self.out.writeByte('(');
        try self.emit(t.type_annotation);
        try self.out.writeByte(')');
    }

    fn emit_ts_function_type(self: *Self, t: *const ast.TSFunctionType) Error!void {
        try self.printArrowType(t.type_parameters, t.params, t.return_type);
    }

    fn emit_ts_constructor_type(self: *Self, t: *const ast.TSConstructorType) Error!void {
        if (t.abstract) try self.out.writeStr("abstract ");
        try self.out.writeStr("new ");
        try self.printArrowType(t.type_parameters, t.params, t.return_type);
    }

    fn printArrowType(
        self: *Self,
        type_parameters: NodeIndex,
        params: NodeIndex,
        return_type: NodeIndex,
    ) Error!void {
        try self.emit(type_parameters);
        try self.printParams(params);
        try self.out.space();
        try self.out.writeStr("=>");
        try self.out.space();
        try self.emitUnwrappedType(return_type);
    }

    fn emitUnwrappedType(self: *Self, idx: NodeIndex) Error!void {
        if (idx == .null) return;
        switch (self.nodeData(idx)) {
            .ts_type_annotation => |t| {
                const scope = try self.openComments(idx);
                try self.emit(t.type_annotation);
                try self.closeComments(idx, scope, false);
            },
            else => try self.emit(idx),
        }
    }

    fn emit_ts_type_predicate(self: *Self, t: *const ast.TSTypePredicate) Error!void {
        if (t.asserts) try self.out.writeStr("asserts ");
        if (t.type_annotation == .null) return self.emit(t.parameter_name);
        try self.emitExpr(t.parameter_name, .{ .defer_trailing = true });
        try self.writeKeywordThenOwed(" is");
        try self.emitUnwrappedType(t.type_annotation);
    }

    fn emit_ts_type_literal(self: *Self, t: *const ast.TSTypeLiteral) Error!void {
        try self.printSignatureBody(t.members);
    }

    fn emit_ts_mapped_type(self: *Self, t: *const ast.TSMappedType) Error!void {
        try self.out.writeByte('{');
        try self.out.space();
        try self.printMappedReadonly(t.readonly);
        try self.out.writeByte('[');
        try self.emit(t.key);
        try self.out.writeStr(" in ");
        try self.emit(t.constraint);
        if (t.name_type != .null) {
            try self.out.writeStr(" as ");
            try self.emit(t.name_type);
        }
        try self.out.writeByte(']');
        try self.printMappedOptional(t.optional);
        if (t.type_annotation != .null) {
            try self.out.writeByte(':');
            try self.out.space();
            try self.emit(t.type_annotation);
        }
        try self.softSemi();
        self.pending_semi = false;
        try self.out.space();
        try self.out.writeByte('}');
    }

    fn printMappedReadonly(self: *Self, m: ast.TSMappedTypeModifier) Error!void {
        switch (m) {
            .none => return,
            .true => {},
            .plus => try self.out.writeByte('+'),
            .minus => try self.out.writeByte('-'),
        }
        try self.out.writeStr("readonly ");
    }

    fn printMappedOptional(self: *Self, m: ast.TSMappedTypeModifier) Error!void {
        switch (m) {
            .none => {},
            .true => try self.out.writeByte('?'),
            .plus => try self.out.writeStr("+?"),
            .minus => try self.out.writeStr("-?"),
        }
    }

    fn emit_ts_property_signature(self: *Self, s: *const ast.TSPropertySignature) Error!void {
        if (s.readonly) try self.out.writeStr("readonly ");
        try self.printPropertyKey(s.key, s.computed);
        if (s.optional) try self.out.writeByte('?');
        try self.emit(s.type_annotation);
        try self.softSemi();
    }

    fn emit_ts_method_signature(self: *Self, s: *const ast.TSMethodSignature) Error!void {
        switch (s.kind) {
            .get => try self.out.writeStr("get "),
            .set => try self.out.writeStr("set "),
            .method => {},
        }
        try self.printPropertyKey(s.key, s.computed);
        if (s.optional) try self.out.writeByte('?');
        try self.printSignatureTail(s.type_parameters, s.params, s.return_type);
    }

    fn printSignatureTail(
        self: *Self,
        type_parameters: NodeIndex,
        params: NodeIndex,
        return_type: NodeIndex,
    ) Error!void {
        try self.emit(type_parameters);
        try self.printParams(params);
        try self.emit(return_type);
        try self.softSemi();
    }

    fn emit_ts_call_signature_declaration(
        self: *Self,
        s: *const ast.TSCallSignatureDeclaration,
    ) Error!void {
        try self.printSignatureTail(s.type_parameters, s.params, s.return_type);
    }

    fn emit_ts_construct_signature_declaration(
        self: *Self,
        s: *const ast.TSConstructSignatureDeclaration,
    ) Error!void {
        try self.out.writeStr("new ");
        try self.printSignatureTail(s.type_parameters, s.params, s.return_type);
    }

    fn emit_ts_index_signature(self: *Self, s: *const ast.TSIndexSignature) Error!void {
        if (s.static) try self.out.writeStr("static ");
        if (s.readonly) try self.out.writeStr("readonly ");
        try self.out.writeByte('[');
        try self.emitList(s.parameters);
        try self.out.writeByte(']');
        try self.emit(s.type_annotation);
        try self.softSemi();
    }

    fn printSignatureBody(self: *Self, items: IndexRange) Error!void {
        try self.out.writeByte('{');
        if (self.tree.extra(items).len > 0) {
            self.indent_depth += 1;
            for (self.tree.extra(items)) |s| {
                try self.flushSemi();
                try self.newline();
                try self.emit(s);
            }
            self.indent_depth -= 1;
            self.pending_semi = false;
            try self.newline();
        } else if (self.options.comments != .none) {
            try self.emitInsideComments(self.current_idx);
        }
        try self.out.writeByte('}');
    }

    fn emit_ts_type_alias_declaration(
        self: *Self,
        d: *const ast.TSTypeAliasDeclaration,
    ) Error!void {
        if (d.declare) try self.out.writeStr("declare ");
        try self.out.writeStr("type ");
        try self.emit(d.id);
        try self.emit(d.type_parameters);
        try self.printEq();
        // a leftmost bare `intrinsic` reference would reparse as the keyword
        const intrinsic = self.isLeftmostIntrinsicReference(d.type_annotation);
        try self.wrapIf(intrinsic, d.type_annotation, .{});
        try self.softSemi();
    }

    fn wrapIf(self: *Self, cond: bool, idx: NodeIndex, ctx: Ctx) Error!void {
        if (cond) try self.out.writeByte('(');
        try self.emitExpr(idx, ctx);
        if (cond) try self.out.writeByte(')');
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
        const list = self.tree.extra(types);
        return if (list.len > 0) list[0] else null;
    }

    fn emit_ts_interface_declaration(self: *Self, d: *const ast.TSInterfaceDeclaration) Error!void {
        if (d.declare) try self.out.writeStr("declare ");
        try self.out.writeStr("interface ");
        try self.emit(d.id);
        try self.emit(d.type_parameters);
        if (self.tree.extra(d.extends).len > 0) {
            try self.out.writeStr(" extends ");
            try self.emitList(d.extends);
        }
        try self.out.space();
        try self.emit(d.body);
    }

    fn emit_ts_interface_body(self: *Self, b: *const ast.TSInterfaceBody) Error!void {
        try self.printSignatureBody(b.body);
    }

    fn emit_ts_interface_heritage(self: *Self, h: *const ast.TSInterfaceHeritage) Error!void {
        try self.emit(h.expression);
        try self.emit(h.type_arguments);
    }

    fn emit_ts_class_implements(self: *Self, c: *const ast.TSClassImplements) Error!void {
        try self.emit(c.expression);
        try self.emit(c.type_arguments);
    }

    fn emit_ts_enum_declaration(self: *Self, d: *const ast.TSEnumDeclaration) Error!void {
        if (d.declare) try self.out.writeStr("declare ");
        if (d.is_const) try self.out.writeStr("const ");
        try self.out.writeStr("enum ");
        try self.emit(d.id);
        try self.out.space();
        try self.emit(d.body);
    }

    fn emit_ts_enum_body(self: *Self, b: *const ast.TSEnumBody) Error!void {
        try self.out.writeByte('{');
        const list = self.tree.extra(b.members);
        if (list.len > 0) {
            self.indent_depth += 1;
            for (list, 0..) |m, i| {
                try self.newline();
                try self.emitExpr(m, .{ .defer_trailing = true });
                try self.closeItem(i + 1 < list.len, null);
            }
            self.indent_depth -= 1;
            try self.newline();
        } else if (self.options.comments != .none) {
            try self.emitInsideComments(self.current_idx);
        }
        try self.out.writeByte('}');
    }

    fn emit_ts_enum_member(self: *Self, m: *const ast.TSEnumMember) Error!void {
        if (m.computed) {
            try self.out.writeByte('[');
            try self.emit(m.id);
            try self.out.writeByte(']');
        } else {
            try self.emit(m.id);
        }
        if (m.initializer != .null) {
            try self.printEq();
            try self.emit(m.initializer);
        }
    }

    fn emit_ts_module_declaration(self: *Self, d: *const ast.TSModuleDeclaration) Error!void {
        if (d.declare) try self.out.writeStr("declare ");
        try self.out.writeStr(d.kind.toString());
        try self.out.writeByte(' ');
        try self.emit(d.id);
        if (d.body != .null) {
            try self.out.space();
            try self.emit(d.body);
        } else {
            try self.softSemi();
        }
    }

    fn emit_ts_module_block(self: *Self, b: *const ast.TSModuleBlock) Error!void {
        try self.printBlock(b.body, false);
    }

    fn emit_ts_global_declaration(self: *Self, d: *const ast.TSGlobalDeclaration) Error!void {
        if (d.declare) try self.out.writeStr("declare ");
        try self.emit(d.id);
        try self.out.space();
        try self.emit(d.body);
    }

    fn emit_ts_type_assertion(self: *Self, e: *const ast.TSTypeAssertion, ctx: Ctx) Error!void {
        try self.out.writeByte('<');
        // `<<T>` would re-lex as `<<`
        if (typeStartsWithLeftAngle(self.tree, e.type_annotation)) try self.out.writeByte(' ');
        try self.emit(e.type_annotation);
        try self.out.writeByte('>');
        try self.emitExpr(e.expression, .{
            .prec = Precedence.Unary,
            .no_instantiation = ctx.no_instantiation,
        });
    }

    fn emit_ts_export_assignment(self: *Self, e: *const ast.TSExportAssignment) Error!void {
        try self.out.writeStr("export");
        try self.printEq();
        try self.emit(e.expression);
        try self.softSemi();
    }

    fn emit_ts_namespace_export_declaration(
        self: *Self,
        d: *const ast.TSNamespaceExportDeclaration,
    ) Error!void {
        try self.out.writeStr("export as namespace ");
        try self.emit(d.id);
        try self.softSemi();
    }

    fn emit_ts_import_equals_declaration(
        self: *Self,
        d: *const ast.TSImportEqualsDeclaration,
    ) Error!void {
        try self.out.writeStr("import ");
        if (d.import_kind == .type) try self.out.writeStr("type ");
        try self.emit(d.id);
        try self.printEq();
        try self.emit(d.module_reference);
        try self.softSemi();
    }

    fn emit_ts_external_module_reference(
        self: *Self,
        r: *const ast.TSExternalModuleReference,
    ) Error!void {
        try self.out.writeStr("require(");
        try self.emit(r.expression);
        try self.out.writeByte(')');
    }

    fn emit_ts_parameter_property(self: *Self, p: *const ast.TSParameterProperty) Error!void {
        try self.printDecorators(p.decorators);
        if (p.accessibility != .none) {
            try self.out.writeStr(p.accessibility.toString());
            try self.out.writeByte(' ');
        }
        if (p.override) try self.out.writeStr("override ");
        if (p.readonly) try self.out.writeStr("readonly ");
        try self.emit(p.parameter);
    }

    fn emit_ts_this_parameter(self: *Self, p: *const ast.TSThisParameter) Error!void {
        try self.out.writeStr("this");
        try self.emit(p.type_annotation);
    }

    fn emit_jsx_element(self: *Self, e: *const ast.JSXElement) Error!void {
        try self.emit(e.opening_element);
        for (self.tree.extra(e.children)) |c| try self.emit(c);
        try self.emit(e.closing_element);
    }

    fn emit_jsx_opening_element(self: *Self, o: *const ast.JSXOpeningElement) Error!void {
        try self.out.writeByte('<');
        try self.emit(o.name);
        try self.emit(o.type_arguments);
        for (self.tree.extra(o.attributes)) |a| {
            try self.out.writeByte(' ');
            try self.emit(a);
        }
        if (o.self_closing) {
            try self.out.space();
            try self.out.writeStr("/>");
        } else {
            try self.out.writeByte('>');
        }
    }

    fn emit_jsx_closing_element(self: *Self, c: *const ast.JSXClosingElement) Error!void {
        try self.out.writeStr("</");
        try self.emit(c.name);
        try self.out.writeByte('>');
    }

    fn emit_jsx_fragment(self: *Self, f: *const ast.JSXFragment) Error!void {
        try self.emit(f.opening_fragment);
        for (self.tree.extra(f.children)) |c| try self.emit(c);
        try self.emit(f.closing_fragment);
    }

    fn emit_jsx_identifier(self: *Self, id: *const ast.JSXIdentifier) Error!void {
        try self.writeString(id.name);
    }

    fn emit_jsx_namespaced_name(self: *Self, n: *const ast.JSXNamespacedName) Error!void {
        try self.emit(n.namespace);
        try self.out.writeByte(':');
        try self.emit(n.name);
    }

    fn emit_jsx_member_expression(self: *Self, m: *const ast.JSXMemberExpression) Error!void {
        try self.emit(m.object);
        try self.out.writeByte('.');
        try self.emit(m.property);
    }

    fn emit_jsx_attribute(self: *Self, a: *const ast.JSXAttribute) Error!void {
        try self.emit(a.name);
        if (a.value != .null) {
            try self.out.writeByte('=');
            // jsx attribute strings have no escapes, the raw lexeme is the value
            switch (self.nodeData(a.value)) {
                .string_literal => |lit| try self.writeNodeText(a.value, self.tree.string(lit.raw)),
                else => try self.emit(a.value),
            }
        }
    }

    fn emit_jsx_spread_attribute(self: *Self, a: *const ast.JSXSpreadAttribute) Error!void {
        try self.printJSXSpread(a.argument);
    }

    fn printJSXSpread(self: *Self, idx: NodeIndex) Error!void {
        try self.out.writeStr("{...");
        try self.emitValue(idx);
        try self.out.writeByte('}');
    }

    fn emit_jsx_expression_container(self: *Self, c: *const ast.JSXExpressionContainer) Error!void {
        try self.out.writeByte('{');
        try self.emitValue(c.expression);
        try self.out.writeByte('}');
    }

    fn emit_jsx_empty_expression(self: *Self, _: *const ast.JSXEmptyExpression) Error!void {
        try self.emitInsideCommentsInline(self.current_idx);
    }

    fn emit_jsx_opening_fragment(self: *Self, _: *const ast.JSXOpeningFragment) Error!void {
        try self.out.writeByte('<');
        try self.emitInsideCommentsInline(self.current_idx);
        try self.out.writeByte('>');
    }

    fn emit_jsx_closing_fragment(self: *Self, _: *const ast.JSXClosingFragment) Error!void {
        try self.out.writeStr("</");
        try self.emitInsideCommentsInline(self.current_idx);
        try self.out.writeByte('>');
    }

    fn emit_jsx_text(self: *Self, t: *const ast.JSXText) Error!void {
        const raw = self.tree.string(t.raw);
        try self.out.writeRawStr(if (raw.len != 0) raw else self.tree.string(t.value));
    }

    fn emit_jsx_spread_child(self: *Self, c: *const ast.JSXSpreadChild) Error!void {
        try self.printJSXSpread(c.expression);
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
        .formal_parameters, .formal_parameter, .template_element => true,
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

fn hasValueImportSpecifier(tree: *const Tree, list: []const NodeIndex) bool {
    for (list) |idx| {
        switch (tree.data(idx)) {
            .import_default_specifier, .import_namespace_specifier => return true,
            .import_specifier => |s| if (s.import_kind != .type) return true,
            else => {},
        }
    }
    return false;
}

fn hasValueExportSpecifier(tree: *const Tree, list: []const NodeIndex) bool {
    for (list) |idx| {
        switch (tree.data(idx)) {
            .export_specifier => |s| if (s.export_kind != .type) return true,
            else => {},
        }
    }
    return false;
}

fn isNamed(tree: *const Tree, idx: NodeIndex, name: []const u8) bool {
    const id = identifierStringOrNull(tree, idx) orelse return false;
    return std.mem.eql(u8, tree.string(id), name);
}

// an uninitialized `const` is ambient, as in a `.d.ts`
fn isAmbient(tree: *const Tree, d: ast.VariableDeclaration) bool {
    if (d.declare) return true;
    if (d.kind != .@"const") return false;
    for (tree.extra(d.declarators)) |x| {
        if (tree.data(x).variable_declarator.init == .null) return true;
    }
    return false;
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

// `??` mixed with `&&`/`||` must be parenthesized
fn logicalMismatch(tree: *const Tree, parent: ast.LogicalOperator, child: NodeIndex) bool {
    const child_op = switch (tree.data(child)) {
        .logical_expression => |l| l.operator,
        else => return false,
    };
    return (parent == .nullish_coalescing) != (child_op == .nullish_coalescing);
}

fn simpleStringKey(tree: *const Tree, idx: NodeIndex) ?[]const u8 {
    const lit = switch (tree.data(idx)) {
        .string_literal => |l| l,
        else => return null,
    };
    const s = tree.string(lit.value);
    return if (utils.isIdentifierName(s)) s else null;
}

fn isBareInteger(text: []const u8) bool {
    if (text.len == 0 or !std.ascii.isDigit(text[0])) return false;
    for (text[1..]) |c| if (!std.ascii.isDigit(c) and c != '_') return false;
    return true;
}

fn needsSpaceBeforeInlineComment(last: u8) bool {
    return switch (last) {
        0, ' ', '\n', '(', '[', '{', '<' => false,
        else => true,
    };
}

fn strippedOperand(tree: *const Tree, idx: NodeIndex) NodeIndex {
    return switch (tree.data(idx)) {
        .ts_as_expression => |e| e.expression,
        .ts_satisfies_expression => |e| e.expression,
        .ts_non_null_expression => |e| e.expression,
        .ts_instantiation_expression => |e| e.expression,
        .ts_type_assertion => |e| e.expression,
        .ts_parameter_property => |p| p.parameter,
        else => unreachable,
    };
}

fn hasLineTerminator(text: []const u8) bool {
    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (util.Utf.lineBreakLen(text, i) > 0) return true;
    }
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
    const p: u8 = e.operator.toToken().precedence();
    if (e.operator == .exponent) return Precedence.Postfix;
    const op = e.operator.toString();
    if (op[0] == '<' and endsWithTsCast(tree, e.left)) return Precedence.Grouping;
    return p;
}

// TypeScript reads `f<T>` as comparisons before `<`, `>`, `+`, or `-`, and its scanner starts
// `>=` and `>>` with `>`
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

const chain_stack_bytes_max = 256 * 1024;

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

// decided per node by `precedenceOf`
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
    const trailing: u8 = 1; // function, constructor, conditional, infer
    const @"union": u8 = 2;
    const intersection: u8 = 3;
    const operator: u8 = 4; // keyof, typeof, readonly, unique
    const primary: u8 = 5;
};
