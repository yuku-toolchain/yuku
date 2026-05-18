const std = @import("std");
const util = @import("util");
const ast = @import("../ast.zig");
const sourcemap = @import("sourcemap.zig");

const Allocator = std.mem.Allocator;
const Tree = ast.Tree;
const NodeIndex = ast.NodeIndex;
const NodeData = ast.NodeData;
const IndexRange = ast.IndexRange;

pub const SourceMap = sourcemap.SourceMap;
pub const SourceMapOptions = sourcemap.Options;

/// Whitespace mode for the output.
pub const Format = enum {
    /// Indented, with spaces around operators and after commas.
    pretty,
    /// No discretionary whitespace; only what the grammar requires.
    compact,
};

/// Quote style for emitted string literals.
pub const Quotes = enum { double, single };

/// Codegen options.
pub const Options = struct {
    format: Format = .pretty,
    /// Spaces per indentation level. Used only when `format = .pretty`.
    indent: u8 = 2,
    quotes: Quotes = .double,
    /// Set to enable Source Map V3 output alongside the code.
    source_maps: ?SourceMapOptions = null,
};

/// A codegen-detected problem in the input AST.
pub const Diagnostic = struct {
    message: []const u8,
    /// Byte offset where the problem starts.
    start: u32,
    /// Byte offset where the problem ends.
    end: u32,
};

/// Output of a codegen run. All buffers are owned by the allocator passed
/// to `print`/`strip`/`minify`; `deinit` with the same allocator frees
/// them.
pub const Result = struct {
    code: []const u8,
    /// Empty when codegen succeeded cleanly.
    errors: []const Diagnostic,
    /// Populated when `Options.source_maps` was set.
    map: ?SourceMap = null,

    pub fn deinit(self: Result, allocator: Allocator) void {
        allocator.free(self.code);
        allocator.free(self.errors);
        if (self.map) |m| m.deinit(allocator);
    }
};

pub const Error = error{OutOfMemory};

/// comptime configuration
pub const Config = struct {
    strip_ts: bool = false,
    /// when true, applies size-reducing substitutions during emit
    minify: bool = false,
};

/// Renders a `Tree` to source code.
pub fn print(allocator: Allocator, tree: *Tree, options: Options) Error!Result {
    return printImpl(.{}, allocator, tree, options);
}

/// Render TypeScript `Tree` to JavaScript output, excluding TypeScript-specific syntax.
pub fn strip(allocator: Allocator, tree: *Tree, options: Options) Error!Result {
    return printImpl(.{ .strip_ts = true }, allocator, tree, options);
}

/// render `Tree` with print-time minification substitutions enabled.
/// combine with `Options.format = .compact` for fully minified output.
pub fn minify(allocator: Allocator, tree: *Tree, options: Options) Error!Result {
    return printImpl(.{ .minify = true }, allocator, tree, options);
}

pub fn printImpl(comptime cfg: Config, allocator: Allocator, tree: *Tree, options: Options) Error!Result {
    var p = try Printer(cfg).init(allocator, tree, options);
    defer p.deinit();
    try p.printRoot();

    const code = try p.code.toOwnedSlice(allocator);
    errdefer allocator.free(code);
    const errors = try p.errors.toOwnedSlice(allocator);
    errdefer allocator.free(errors);
    const map = if (p.sm) |*sm| try sm.build(allocator) else null;
    return .{ .code = code, .errors = errors, .map = map };
}

fn Printer(comptime cfg: Config) type {
    return struct {
        const Self = @This();
        const strip_ts = cfg.strip_ts;
        const minify_mode = cfg.minify;

        tree: *Tree,
        code: std.ArrayList(u8) = .empty,
        errors: std.ArrayList(Diagnostic) = .empty,
        options: Options,
        arena: std.heap.ArenaAllocator,
        allocator: Allocator,
        indent_depth: u32 = 0,
        /// set while emitting a destructuring/assignment target so nested
        /// array/object literals know to re-paren ts `as`/`satisfies`/
        /// type-assertion leaves the parser elided.
        in_assign_target: bool = false,
        /// In compact mode, statement terminators are deferred via this flag
        /// rather than emitted immediately. The next statement flushes it,
        /// a closing `}` clears it, eliminating the trailing `;` for free.
        pending_semi: bool = false,
        /// Source-map state. Present iff `options.source_maps != null`.
        sm: ?sourcemap.State = null,

    fn init(allocator: Allocator, tree: *Tree, options: Options) Error!Self {
        var p = Self{
            .tree = tree,
            .options = options,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .allocator = allocator,
        };
        try p.code.ensureTotalCapacity(allocator, tree.source.len);
        if (options.source_maps) |sm_opts| {
            p.sm = try sourcemap.State.init(allocator, sm_opts);
            // pre-size to ~one segment per node; avoids reallocations.
            try p.sm.?.mappings.ensureTotalCapacity(allocator, tree.nodes.len);
        }
        return p;
    }

    fn deinit(self: *Self) void {
        self.arena.deinit();
        self.code.deinit(self.allocator);
        self.errors.deinit(self.allocator);
        if (self.sm) |*sm| sm.deinit(self.allocator);
    }

    inline fn pretty(self: *const Self) bool {
        return self.options.format == .pretty;
    }

    inline fn writeByte(self: *Self, b: u8) Error!void {
        self.dropPendingKeywordSpace(b);
        try self.code.append(self.allocator, b);
        if (self.sm) |*sm| {
            if (b == '\n') {
                sm.gen_line += 1;
                sm.gen_col = 0;
            } else sm.gen_col += 1;
        }
    }

    inline fn writeStr(self: *Self, s: []const u8) Error!void {
        if (s.len == 0) return;
        self.dropPendingKeywordSpace(s[0]);
        try self.code.appendSlice(self.allocator, s);
        if (self.sm) |*sm| sm.advance(s);
    }

    /// In compact mode, drop the trailing ` ` from a just-written `keyword `
    /// when the upcoming byte is punctuation (`else { … }` → `else{…}`,
    /// `return"x"` → `return"x"`). Preserved when the next byte extends an
    /// identifier (`return foo`, `case 5:`).
    inline fn dropPendingKeywordSpace(self: *Self, next: u8) void {
        if (self.pretty()) return;
        const items = self.code.items;
        if (items.len < 2 or items[items.len - 1] != ' ') return;
        if (isIdCont(items[items.len - 2]) and !isIdCont(next)) {
            _ = self.code.pop();
            if (self.sm) |*sm| if (sm.gen_col > 0) {
                sm.gen_col -= 1;
            };
        }
    }

    inline fn writeString(self: *Self, id: ast.String) Error!void {
        try self.writeStr(self.tree.string(id));
    }

    inline fn space(self: *Self) Error!void {
        if (self.pretty()) try self.writeByte(' ');
    }

    fn newline(self: *Self) Error!void {
        if (!self.pretty()) return;
        try self.writeByte('\n');
        const n = self.indent_depth * self.options.indent;
        try self.code.appendNTimes(self.allocator, ' ', n);
        if (self.sm) |*sm| sm.gen_col = n;
    }

    inline fn mark(self: *const Self) usize {
        return self.code.items.len;
    }

    inline fn rewindTo(self: *Self, pos: usize) void {
        self.code.shrinkRetainingCapacity(pos);
    }

    /// Snapshot of the state needed to undo a speculative `tryEmit`.
    const Cursor = struct {
        code: usize,
        sm: ?sourcemap.State.Snapshot,
    };

    inline fn cursor(self: *const Self) Cursor {
        return .{
            .code = self.code.items.len,
            .sm = if (self.sm) |sm| sm.snapshot() else null,
        };
    }

    inline fn restore(self: *Self, c: Cursor) void {
        self.code.shrinkRetainingCapacity(c.code);
        if (c.sm) |snap| if (self.sm) |*sm| sm.restore(snap);
    }

    fn tryEmit(self: *Self, idx: NodeIndex) Error!bool {
        const start = self.mark();
        const tail: u8 = if (start > 0) self.code.items[start - 1] else 0;
        try self.emit(idx);
        if (self.code.items.len > start) return true;
        return start > 0 and self.code.items[start - 1] != tail;
    }

    /// emits `idx` in a position where a statement is grammatically required;
    /// substitutes an empty statement if the emit produces no output.
    fn emitStmt(self: *Self, idx: NodeIndex) Error!void {
        if (!try self.tryEmit(idx)) try self.writeByte(';');
    }

    fn printRoot(self: *Self) Error!void {
        try self.emit(self.tree.root);
    }

    fn emit(self: *Self, idx: NodeIndex) Error!void {
        if (idx == .null) return;
        const data = self.tree.data(idx);

        if (comptime strip_ts) {
            if (data.isTypeContext()) return;

            switch (data) {
                .ts_type_alias_declaration,
                .ts_interface_declaration,
                .ts_global_declaration,
                .ts_namespace_export_declaration,
                .ts_this_parameter,
                => return,
                .ts_as_expression => |e| return self.emit(e.expression),
                .ts_satisfies_expression => |e| return self.emit(e.expression),
                .ts_type_assertion => |e| return self.emit(e.expression),
                .ts_non_null_expression => |e| return self.emit(e.expression),
                .ts_instantiation_expression => |e| return self.emit(e.expression),
                .ts_enum_declaration => |e| {
                    if (e.declare) return;
                    return self.diagnose(idx, "TypeScript enums cannot be stripped to JavaScript");
                },
                .ts_module_declaration => |m| {
                    if (m.declare) return;
                    return self.diagnose(idx, "TypeScript namespaces cannot be stripped to JavaScript");
                },
                .ts_import_equals_declaration => |i| {
                    if (i.import_kind == .type) return;
                    return self.diagnose(idx, "`import = require()` cannot be stripped to JavaScript");
                },
                .ts_export_assignment => return self.diagnose(idx, "`export =` cannot be stripped to JavaScript"),
                .ts_parameter_property => |pp| {
                    try self.diagnose(idx, "parameter properties cannot be stripped to JavaScript");
                    return self.emit(pp.parameter);
                },
                else => {},
            }
        }

        if (self.sm != null) try self.recordMapping(idx, data);

        switch (data) {
            inline else => |node, tag| {
                const fn_name = "emit_" ++ @tagName(tag);
                if (comptime @hasDecl(Self, fn_name)) {
                    try @field(Self, fn_name)(self, node);
                } else {
                    std.debug.panic("codegen: not implemented for {s}", .{@tagName(tag)});
                }
            },
        }
    }

    /// Records a mapping for `idx`. Skips synthetic spans (0, 0).
    fn recordMapping(self: *Self, idx: NodeIndex, data: NodeData) Error!void {
        var sm = &self.sm.?;
        const span = self.tree.span(idx);
        if (span.start == 0 and span.end == 0) return;

        const orig = sm.locate(span.start);
        const name_idx: i32 = switch (data) {
            inline .identifier_reference,
            .identifier_name,
            .binding_identifier,
            .label_identifier,
            .private_identifier,
            .jsx_identifier,
            => |id| @intCast(try sm.intern(self.allocator, self.tree.string(id.name))),
            else => -1,
        };
        try sm.record(self.allocator, orig.line, orig.col, name_idx);
    }

    fn diagnose(self: *Self, idx: NodeIndex, message: []const u8) Error!void {
        const span = self.tree.span(idx);
        try self.errors.append(self.allocator, .{
            .message = message,
            .start = span.start,
            .end = span.end,
        });
    }

    fn emit_program(self: *Self, p: ast.Program) Error!void {
        if (p.hashbang) |h| {
            try self.writeStr("#!");
            try self.writeString(h.value);
            try self.newline();
        }
        try self.printStmtList(p.body);
        self.pending_semi = false;
    }

    /// Emits a list of statements, flushing the deferred `;` between each.
    /// On strip-to-nothing, rewinds buffer and restores `pending_semi` so the
    /// preceding statement's terminator isn't lost.
    fn printStmtList(self: *Self, items: IndexRange) Error!void {
        var first = true;
        for (self.tree.extra(items)) |s| {
            const cur = self.cursor();
            const saved_semi = self.pending_semi;
            if (!first) try self.newline();
            try self.flushSemi();
            if (try self.tryEmit(s)) {
                first = false;
            } else {
                self.restore(cur);
                self.pending_semi = saved_semi;
            }
        }
    }

    fn printBlock(self: *Self, items: IndexRange) Error!void {
        try self.writeByte('{');
        if (self.tree.extra(items).len > 0) {
            const cur = self.cursor();
            self.indent_depth += 1;
            try self.newline();
            const after_indent = self.mark();
            try self.printStmtList(items);
            self.indent_depth -= 1;
            if (self.mark() == after_indent) {
                self.restore(cur);
            } else {
                self.pending_semi = false;
                try self.newline();
            }
        }
        try self.writeByte('}');
    }

    /// Statement-terminator `;`. In compact mode, defers via `pending_semi`
    /// so the next `flushSemi` writes it or a closing `}` drops it for free.
    inline fn softSemi(self: *Self) Error!void {
        if (self.pretty()) try self.writeByte(';') else self.pending_semi = true;
    }

    /// Emits any deferred `;` and clears the flag.
    inline fn flushSemi(self: *Self) Error!void {
        if (self.pending_semi) {
            self.pending_semi = false;
            try self.writeByte(';');
        }
    }

    fn emit_block_statement(self: *Self, s: ast.BlockStatement) Error!void {
        try self.printBlock(s.body);
    }

    fn emit_function_body(self: *Self, b: ast.FunctionBody) Error!void {
        try self.printBlock(b.body);
    }

    fn emit_static_block(self: *Self, b: ast.StaticBlock) Error!void {
        try self.writeStr("static");
        try self.space();
        try self.printBlock(b.body);
    }

    fn emit_directive(self: *Self, d: ast.Directive) Error!void {
        try self.emitStringLit(d.value);
        try self.softSemi();
    }

    fn emit_empty_statement(self: *Self, _: ast.EmptyStatement) Error!void {
        // Direct `;` (not deferred): when this serves as the body of an outer
        // control-flow construct (`if(x);`, `for(;;);`, `lbl:;`), the parser
        // requires the `;` to materialize the body.
        try self.writeByte(';');
    }

    fn emit_debugger_statement(self: *Self, _: ast.DebuggerStatement) Error!void {
        try self.writeStr("debugger");
        try self.softSemi();
    }

    fn emit_expression_statement(self: *Self, s: ast.ExpressionStatement) Error!void {
        try self.emit(s.expression);
        try self.softSemi();
    }

    fn emit_if_statement(self: *Self, s: ast.IfStatement) Error!void {
        try self.writeStr("if");
        try self.space();
        try self.writeByte('(');
        try self.emit(s.@"test");
        try self.writeByte(')');
        try self.space();
        try self.emitStmt(s.consequent);
        if (s.alternate != .null) {
            try self.flushSemi();
            try self.space();
            try self.writeStr("else ");
            try self.emitStmt(s.alternate);
        }
    }

    fn emit_return_statement(self: *Self, s: ast.ReturnStatement) Error!void {
        try self.writeStr("return");
        if (s.argument != .null) {
            try self.writeByte(' ');
            try self.emit(s.argument);
        }
        try self.softSemi();
    }

    fn emit_throw_statement(self: *Self, s: ast.ThrowStatement) Error!void {
        try self.writeStr("throw ");
        try self.emit(s.argument);
        try self.softSemi();
    }

    fn emit_break_statement(self: *Self, s: ast.BreakStatement) Error!void {
        try self.writeStr("break");
        if (s.label != .null) {
            try self.writeByte(' ');
            try self.emit(s.label);
        }
        try self.softSemi();
    }

    fn emit_continue_statement(self: *Self, s: ast.ContinueStatement) Error!void {
        try self.writeStr("continue");
        if (s.label != .null) {
            try self.writeByte(' ');
            try self.emit(s.label);
        }
        try self.softSemi();
    }

    fn emit_labeled_statement(self: *Self, s: ast.LabeledStatement) Error!void {
        try self.emit(s.label);
        try self.writeByte(':');
        try self.space();
        try self.emitStmt(s.body);
    }

    fn emit_with_statement(self: *Self, s: ast.WithStatement) Error!void {
        try self.writeStr("with");
        try self.space();
        try self.writeByte('(');
        try self.emit(s.object);
        try self.writeByte(')');
        try self.space();
        try self.emitStmt(s.body);
    }

    fn emit_while_statement(self: *Self, s: ast.WhileStatement) Error!void {
        try self.writeStr("while");
        try self.space();
        try self.writeByte('(');
        try self.emit(s.@"test");
        try self.writeByte(')');
        try self.space();
        try self.emitStmt(s.body);
    }

    fn emit_do_while_statement(self: *Self, s: ast.DoWhileStatement) Error!void {
        try self.writeStr("do ");
        try self.emitStmt(s.body);
        try self.flushSemi();
        try self.space();
        try self.writeStr("while");
        try self.space();
        try self.writeByte('(');
        try self.emit(s.@"test");
        try self.writeStr(");");
    }

    fn emit_for_statement(self: *Self, s: ast.ForStatement) Error!void {
        try self.writeStr("for");
        try self.space();
        try self.writeByte('(');
        if (s.init != .null) try self.printForLeft(s.init);
        try self.writeByte(';');
        if (s.@"test" != .null) {
            try self.space();
            try self.emit(s.@"test");
        }
        try self.writeByte(';');
        if (s.update != .null) {
            try self.space();
            try self.emit(s.update);
        }
        try self.writeByte(')');
        try self.space();
        try self.emitStmt(s.body);
    }

    fn emit_for_in_statement(self: *Self, s: ast.ForInStatement) Error!void {
        try self.writeStr("for");
        try self.space();
        try self.writeByte('(');
        try self.printForLeft(s.left);
        try self.writeStr(" in ");
        try self.emit(s.right);
        try self.writeByte(')');
        try self.space();
        try self.emitStmt(s.body);
    }

    fn emit_for_of_statement(self: *Self, s: ast.ForOfStatement) Error!void {
        try self.writeStr("for");
        if (s.@"await") try self.writeStr(" await");
        try self.space();
        try self.writeByte('(');
        // `for (async of …)` is forbidden, disambiguates from `for await`.
        const wrap_async = !s.@"await" and isBareAsyncIdentifier(self.tree, s.left);
        if (wrap_async) try self.writeByte('(');
        try self.printForLeft(s.left);
        if (wrap_async) try self.writeByte(')');
        try self.writeStr(" of ");
        try self.emit(s.right);
        try self.writeByte(')');
        try self.space();
        try self.emitStmt(s.body);
    }

    fn printForLeft(self: *Self, idx: NodeIndex) Error!void {
        switch (self.tree.data(idx)) {
            .variable_declaration => |d| try self.printVariableDecl(d, false),
            else => try self.emitAssignTarget(idx),
        }
    }

    fn emit_switch_statement(self: *Self, s: ast.SwitchStatement) Error!void {
        try self.writeStr("switch");
        try self.space();
        try self.writeByte('(');
        try self.emit(s.discriminant);
        try self.writeByte(')');
        try self.space();
        try self.writeByte('{');
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
        try self.writeByte('}');
    }

    fn emit_switch_case(self: *Self, c: ast.SwitchCase) Error!void {
        if (c.@"test" != .null) {
            try self.writeStr("case ");
            try self.emit(c.@"test");
            try self.writeByte(':');
        } else {
            try self.writeStr("default:");
        }
        if (self.tree.extra(c.consequent).len == 0) return;
        self.indent_depth += 1;
        defer self.indent_depth -= 1;
        try self.printStmtList(c.consequent);
    }

    fn emit_try_statement(self: *Self, s: ast.TryStatement) Error!void {
        try self.writeStr("try ");
        try self.emit(s.block);
        if (s.handler != .null) {
            try self.space();
            try self.emit(s.handler);
        }
        if (s.finalizer != .null) {
            try self.space();
            try self.writeStr("finally ");
            try self.emit(s.finalizer);
        }
    }

    fn emit_catch_clause(self: *Self, c: ast.CatchClause) Error!void {
        try self.writeStr("catch");
        if (c.param != .null) {
            try self.space();
            try self.writeByte('(');
            try self.emit(c.param);
            try self.writeByte(')');
        }
        try self.space();
        try self.emit(c.body);
    }

    fn emit_variable_declaration(self: *Self, d: ast.VariableDeclaration) Error!void {
        if (comptime strip_ts) if (d.declare) return;
        try self.printVariableDecl(d, true);
    }

    fn printVariableDecl(self: *Self, d: ast.VariableDeclaration, with_semicolon: bool) Error!void {
        if (comptime !strip_ts) if (d.declare) try self.writeStr("declare ");
        try self.writeStr(d.kind.toString());
        try self.writeByte(' ');
        const list = self.tree.extra(d.declarators);
        for (list, 0..) |dx, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(dx);
        }
        if (with_semicolon) try self.softSemi();
    }

    fn emit_variable_declarator(self: *Self, d: ast.VariableDeclarator) Error!void {
        try self.emit(d.id);
        if (comptime !strip_ts) if (d.definite) try self.writeByte('!');
        if (d.init != .null) {
            try self.separateBangFromAssign();
            try self.space();
            try self.writeByte('=');
            try self.space();
            try self.emit(d.init);
        }
    }

    fn emit_sequence_expression(self: *Self, e: ast.SequenceExpression) Error!void {
        const list = self.tree.extra(e.expressions);
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(x);
        }
    }

    fn emit_parenthesized_expression(self: *Self, e: ast.ParenthesizedExpression) Error!void {
        try self.writeByte('(');
        try self.emit(e.expression);
        try self.writeByte(')');
    }

    fn emit_binary_expression(self: *Self, e: ast.BinaryExpression) Error!void {
        const op = e.operator.toString();
        if (comptime minify_mode) {
            // `**` requires its left operand to be an UpdateExpression, an
            // unparenthesized unary is a SyntaxError. our minify substitutions
            // (`true`/`false`/`undefined` -> unary) can produce such a left.
            const is_pow = op.len == 2 and op[0] == '*' and op[1] == '*';
            if (is_pow and minifiesToUnary(self.tree, e.left)) {
                try self.writeByte('(');
                try self.emit(e.left);
                try self.writeByte(')');
            } else {
                try self.emit(e.left);
            }
        } else {
            try self.emit(e.left);
        }
        if (isWordOp(op)) {
            try self.writeByte(' ');
            try self.writeStr(op);
            try self.writeByte(' ');
        } else {
            // `x! == y` would re-lex as `!==`.
            if (op[0] == '=') try self.separateBangFromAssign();
            try self.space();
            try self.writeStr(op);
            try self.space();
            // guard against token merges: `++`/`--`, `//` regex/comment,
            // `<!--` annex B html-like line comment.
            if (!self.pretty() and op.len == 1) {
                switch (op[0]) {
                    '+', '-', '/' => if (leftmostByteIs(self.tree, e.right, op[0])) try self.writeByte(' '),
                    '<' => if (leftmostByteIs(self.tree, e.right, '!')) try self.writeByte(' '),
                    else => {},
                }
            }
        }
        try self.emit(e.right);
    }

    fn emit_logical_expression(self: *Self, e: ast.LogicalExpression) Error!void {
        try self.emit(e.left);
        try self.space();
        try self.writeStr(e.operator.toString());
        try self.space();
        try self.emit(e.right);
    }

    fn emit_conditional_expression(self: *Self, e: ast.ConditionalExpression) Error!void {
        try self.emit(e.@"test");
        try self.space();
        try self.writeByte('?');
        try self.space();
        try self.emit(e.consequent);
        try self.space();
        try self.writeByte(':');
        try self.space();
        try self.emit(e.alternate);
    }

    fn emit_unary_expression(self: *Self, e: ast.UnaryExpression) Error!void {
        const op = e.operator.toString();
        try self.writeStr(op);
        if (isWordOp(op)) try self.writeByte(' ');
        // `+ +x` would print as `++x` and re-lex as a prefix update.
        if (op.len == 1 and (op[0] == '+' or op[0] == '-')) {
            if (leftmostByteIs(self.tree, e.argument, op[0])) try self.writeByte(' ');
        }
        try self.emit(e.argument);
    }

    fn emit_update_expression(self: *Self, e: ast.UpdateExpression) Error!void {
        const op = e.operator.toString();
        if (e.prefix) {
            try self.writeStr(op);
            try self.emitAssignTarget(e.argument);
        } else {
            try self.emitAssignTarget(e.argument);
            try self.writeStr(op);
        }
    }

    fn emit_assignment_expression(self: *Self, e: ast.AssignmentExpression) Error!void {
        try self.emitAssignTarget(e.left);
        try self.separateBangFromAssign();
        try self.space();
        try self.writeStr(e.operator.toString());
        try self.space();
        try self.emit(e.right);
    }

    /// `x!=…` would re-lex `!` (non-null assertion) into `!=`. pad if needed.
    inline fn separateBangFromAssign(self: *Self) Error!void {
        if (self.pretty()) return;
        const items = self.code.items;
        if (items.len > 0 and items[items.len - 1] == '!') try self.writeByte(' ');
    }

    /// emits `idx` as an assignment/destructuring target, wrapping a bare
    /// ts `as`/`satisfies`/type-assertion in parens (the parser elides
    /// them, the grammar doesn't).
    fn emitAssignTarget(self: *Self, idx: NodeIndex) Error!void {
        if (idx == .null) return;
        const prev = self.in_assign_target;
        defer self.in_assign_target = prev;

        if (needsParensAsAssignTarget(self.tree, idx)) {
            self.in_assign_target = false;
            try self.writeByte('(');
            try self.emit(idx);
            try self.writeByte(')');
        } else {
            self.in_assign_target = true;
            try self.emit(idx);
        }
    }

    /// routes a child of an array/object literal: if the literal is itself
    /// a target, the child is too.
    inline fn emitChildOfAssignTarget(self: *Self, idx: NodeIndex) Error!void {
        if (self.in_assign_target) try self.emitAssignTarget(idx) else try self.emit(idx);
    }

    fn emit_array_expression(self: *Self, e: ast.ArrayExpression) Error!void {
        const in_target = self.in_assign_target;
        self.in_assign_target = false;
        defer self.in_assign_target = in_target;

        try self.writeByte('[');
        for (self.tree.extra(e.elements), 0..) |x, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            if (in_target) try self.emitAssignTarget(x) else try self.emit(x);
        }
        try self.writeByte(']');
    }

    fn emit_object_expression(self: *Self, e: ast.ObjectExpression) Error!void {
        // re-set per-property so a value's recursion can't strip the flag
        // from later siblings.
        const in_target = self.in_assign_target;
        defer self.in_assign_target = in_target;

        try self.writeByte('{');
        const list = self.tree.extra(e.properties);
        if (list.len > 0) {
            try self.space();
            for (list, 0..) |x, i| {
                if (i > 0) {
                    try self.writeByte(',');
                    try self.space();
                }
                self.in_assign_target = in_target;
                try self.emit(x);
            }
            try self.space();
        }
        try self.writeByte('}');
    }

    fn emit_object_property(self: *Self, p: ast.ObjectProperty) Error!void {
        if (p.method or p.kind == .get or p.kind == .set) {
            const fn_data = self.tree.data(p.value).function;
            if (p.kind == .get) {
                try self.writeStr("get ");
            } else if (p.kind == .set) {
                try self.writeStr("set ");
            } else {
                if (fn_data.@"async") try self.writeStr("async ");
                if (fn_data.generator) try self.writeByte('*');
            }
            try self.printObjectKey(p.key, p.computed);
            try self.printFunctionAsMethod(fn_data);
            return;
        }

        if (p.shorthand and shorthandStillValid(self.tree, p.key, p.value)) {
            try self.emitChildOfAssignTarget(p.value);
            return;
        }

        try self.printObjectKey(p.key, p.computed);
        try self.writeByte(':');
        try self.space();
        try self.emitChildOfAssignTarget(p.value);
    }

    fn emit_spread_element(self: *Self, s: ast.SpreadElement) Error!void {
        try self.writeStr("...");
        try self.emit(s.argument);
    }

    fn emit_member_expression(self: *Self, e: ast.MemberExpression) Error!void {
        const head_decimal = isDecimalLiteral(self.tree, e.object);
        const head_start = self.mark();
        try self.emitAsHead(e.object);

        // `obj["foo"]` → `obj.foo` when the key names a valid identifier.
        const static_key: ?[]const u8 = if (comptime minify_mode)
            (if (e.computed) simpleStringKey(self.tree, e.property) else null)
        else
            null;

        if (e.computed and static_key == null) {
            if (e.optional) try self.writeStr("?.");
            try self.writeByte('[');
            try self.emit(e.property);
            try self.writeByte(']');
        } else {
            // `1.0` shortens to `1`, the member `.` would then be absorbed
            // as the literal's trailing dot. close the number with `.` so
            // `1..toString()` lexes as number-dot-name.
            if (comptime minify_mode) {
                const head = self.code.items[head_start..];
                if (head_decimal and !e.optional and std.mem.indexOfAny(u8, head, ".eE") == null)
                    try self.writeByte('.');
            }
            try self.writeStr(if (e.optional) "?." else ".");
            if (static_key) |k| try self.writeStr(k) else try self.emit(e.property);
        }
    }

    fn emit_call_expression(self: *Self, e: ast.CallExpression) Error!void {
        try self.emitAsHead(e.callee);
        if (e.optional) try self.writeStr("?.");
        try self.emit(e.type_arguments);
        try self.printArgList(e.arguments);
    }

    fn emit_chain_expression(self: *Self, e: ast.ChainExpression) Error!void {
        try self.emit(e.expression);
    }

    fn emit_new_expression(self: *Self, e: ast.NewExpression) Error!void {
        try self.writeStr("new ");
        try self.emitAsHead(e.callee);
        try self.emit(e.type_arguments);
        try self.printArgList(e.arguments);
    }

    fn emit_tagged_template_expression(self: *Self, e: ast.TaggedTemplateExpression) Error!void {
        try self.emitAsHead(e.tag);
        try self.emit(e.type_arguments);
        try self.emit(e.quasi);
    }

    /// emits `idx` as the head of a member/call/new/tagged-template form.
    /// in minify mode, `true`/`undefined`/`Infinity` rewrite to non-primary
    /// expressions (`!0`, `void 0`, `1/0`), so wrap them in parens here,
    /// otherwise `true.x` would emit `!0.x`, parsed as `!(0.x)`.
    inline fn emitAsHead(self: *Self, idx: NodeIndex) Error!void {
        if (comptime minify_mode) {
            if (idx != .null and isMinifiedToNonPrimary(self.tree, idx)) {
                try self.writeByte('(');
                try self.emit(idx);
                try self.writeByte(')');
                return;
            }
        }
        try self.emit(idx);
    }

    fn emit_await_expression(self: *Self, e: ast.AwaitExpression) Error!void {
        try self.writeStr("await ");
        try self.emit(e.argument);
    }

    fn emit_yield_expression(self: *Self, e: ast.YieldExpression) Error!void {
        try self.writeStr("yield");
        if (e.delegate) try self.writeByte('*');
        if (e.argument != .null) {
            try self.writeByte(' ');
            try self.emit(e.argument);
        }
    }

    fn emit_meta_property(self: *Self, p: ast.MetaProperty) Error!void {
        try self.emit(p.meta);
        try self.writeByte('.');
        try self.emit(p.property);
    }

    fn emit_super(self: *Self, _: ast.Super) Error!void {
        try self.writeStr("super");
    }

    fn emit_this_expression(self: *Self, _: ast.ThisExpression) Error!void {
        try self.writeStr("this");
    }

    fn printArgList(self: *Self, args: IndexRange) Error!void {
        try self.writeByte('(');
        const list = self.tree.extra(args);
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(x);
        }
        try self.writeByte(')');
    }

    fn emit_string_literal(self: *Self, lit: ast.StringLiteral) Error!void {
        try self.emitStringLit(lit.value);
    }

    fn emitStringLit(self: *Self, value: ast.String) Error!void {
        const s = self.tree.string(value);
        const q = self.pickQuote(s);
        try self.writeByte(q);
        try self.writeEscapedString(s, q);
        try self.writeByte(q);
    }

    /// in minify mode, picks the quote character that needs fewer escapes
    /// for `s`. outside minify mode, always honors `Options.quotes`.
    inline fn pickQuote(self: *const Self, s: []const u8) u8 {
        const default: u8 = if (self.options.quotes == .single) '\'' else '"';
        if (comptime !minify_mode) return default;
        const single = std.mem.count(u8, s, "'");
        const double = std.mem.count(u8, s, "\"");
        if (single == double) return default;
        return if (double < single) '"' else '\'';
    }

    fn writeEscapedString(self: *Self, s: []const u8, quote: u8) Error!void {
        var start: usize = 0;
        for (s, 0..) |c, i| {
            const esc: ?[]const u8 = switch (c) {
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
                if (i > start) try self.writeStr(s[start..i]);
                try self.writeStr(e);
                start = i + 1;
            }
        }
        if (start < s.len) try self.writeStr(s[start..]);
    }

    fn emit_numeric_literal(self: *Self, lit: ast.NumericLiteral) Error!void {
        if (comptime minify_mode) {
            try self.writeShortestNumber(lit);
        } else {
            try self.writeString(lit.raw);
        }
    }

    fn writeShortestNumber(self: *Self, lit: ast.NumericLiteral) Error!void {
        const raw = self.tree.string(lit.raw);
        var src_buf: [128]u8 = undefined;
        const cleaned = stripUnderscores(raw, &src_buf) orelse return self.writeStr(raw);
        if (lit.kind != .decimal) return self.writeStr(cleaned);

        var dst_buf: [128]u8 = undefined;
        try self.writeStr(shortestDecimal(cleaned, &dst_buf));
    }

    fn emit_bigint_literal(self: *Self, lit: ast.BigIntLiteral) Error!void {
        try self.writeString(lit.raw);
        try self.writeByte('n');
    }

    fn emit_boolean_literal(self: *Self, lit: ast.BooleanLiteral) Error!void {
        if (comptime minify_mode) {
            try self.writeStr(if (lit.value) "!0" else "!1");
        } else {
            try self.writeStr(if (lit.value) "true" else "false");
        }
    }

    fn emit_null_literal(self: *Self, _: ast.NullLiteral) Error!void {
        try self.writeStr("null");
    }

    fn emit_regexp_literal(self: *Self, lit: ast.RegExpLiteral) Error!void {
        try self.writeByte('/');
        try self.writeString(lit.pattern);
        try self.writeByte('/');
        try self.writeString(lit.flags);
    }

    fn emit_template_literal(self: *Self, lit: ast.TemplateLiteral) Error!void {
        try self.writeByte('`');
        const quasis = self.tree.extra(lit.quasis);
        const exprs = self.tree.extra(lit.expressions);
        for (quasis, 0..) |q, i| {
            try self.emit(q);
            if (i < exprs.len) {
                try self.writeStr("${");
                try self.emit(exprs[i]);
                try self.writeByte('}');
            }
        }
        try self.writeByte('`');
    }

    fn emit_template_element(self: *Self, el: ast.TemplateElement) Error!void {
        const s = self.tree.string(el.cooked);
        var i: usize = 0;
        var start: usize = 0;
        while (i < s.len) : (i += 1) {
            const c = s[i];
            const esc: ?[]const u8 = switch (c) {
                '\\' => "\\\\",
                '`' => "\\`",
                '$' => if (i + 1 < s.len and s[i + 1] == '{') "\\$" else null,
                '\r' => "\\r",
                0 => if (i + 1 < s.len and std.ascii.isDigit(s[i + 1])) "\\x00" else "\\0",
                else => null,
            };
            if (esc) |e| {
                if (i > start) try self.writeStr(s[start..i]);
                try self.writeStr(e);
                start = i + 1;
            }
        }
        if (start < s.len) try self.writeStr(s[start..]);
    }

    fn emit_identifier_reference(self: *Self, id: ast.IdentifierReference) Error!void {
        if (comptime minify_mode) {
            if (!self.in_assign_target) {
                const name = self.tree.string(id.name);
                if (std.mem.eql(u8, name, "undefined")) return self.writeStr("void 0");
                if (std.mem.eql(u8, name, "Infinity")) return self.writeStr("1/0");
            }
        }
        try self.writeString(id.name);
    }

    fn emit_identifier_name(self: *Self, id: ast.IdentifierName) Error!void {
        try self.writeString(id.name);
    }

    fn emit_binding_identifier(self: *Self, id: ast.BindingIdentifier) Error!void {
        if (comptime !strip_ts) try self.printDecorators(id.decorators);
        try self.writeString(id.name);
        if (comptime !strip_ts) if (id.optional) try self.writeByte('?');
        try self.emit(id.type_annotation);
    }

    fn emit_label_identifier(self: *Self, id: ast.LabelIdentifier) Error!void {
        try self.writeString(id.name);
    }

    fn emit_private_identifier(self: *Self, id: ast.PrivateIdentifier) Error!void {
        try self.writeByte('#');
        try self.writeString(id.name);
    }

    fn emit_assignment_pattern(self: *Self, p: ast.AssignmentPattern) Error!void {
        if (comptime !strip_ts) try self.printDecorators(p.decorators);
        try self.emit(p.left);
        if (comptime !strip_ts) if (p.optional) try self.writeByte('?');
        try self.emit(p.type_annotation);
        try self.separateBangFromAssign();
        try self.space();
        try self.writeByte('=');
        try self.space();
        try self.emit(p.right);
    }

    fn emit_binding_rest_element(self: *Self, r: ast.BindingRestElement) Error!void {
        if (comptime !strip_ts) try self.printDecorators(r.decorators);
        try self.writeStr("...");
        try self.emit(r.argument);
        if (comptime !strip_ts) if (r.optional) try self.writeByte('?');
        try self.emit(r.type_annotation);
    }

    fn emit_array_pattern(self: *Self, p: ast.ArrayPattern) Error!void {
        if (comptime !strip_ts) try self.printDecorators(p.decorators);
        try self.writeByte('[');
        const list = self.tree.extra(p.elements);
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emitAssignTarget(x);
        }
        if (p.rest != .null) {
            if (list.len > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(p.rest);
        }
        try self.writeByte(']');
        if (comptime !strip_ts) if (p.optional) try self.writeByte('?');
        try self.emit(p.type_annotation);
    }

    fn emit_object_pattern(self: *Self, p: ast.ObjectPattern) Error!void {
        if (comptime !strip_ts) try self.printDecorators(p.decorators);
        try self.writeByte('{');
        const list = self.tree.extra(p.properties);
        const has_any = list.len > 0 or p.rest != .null;
        if (has_any) try self.space();
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(x);
        }
        if (p.rest != .null) {
            if (list.len > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(p.rest);
        }
        if (has_any) try self.space();
        try self.writeByte('}');
        if (comptime !strip_ts) if (p.optional) try self.writeByte('?');
        try self.emit(p.type_annotation);
    }

    fn emit_binding_property(self: *Self, p: ast.BindingProperty) Error!void {
        if (p.shorthand and shorthandStillValid(self.tree, p.key, p.value)) {
            try self.emitAssignTarget(p.value);
            return;
        }
        try self.printObjectKey(p.key, p.computed);
        try self.writeByte(':');
        try self.space();
        try self.emitAssignTarget(p.value);
    }

    fn printPropertyKey(self: *Self, key: NodeIndex, computed: bool) Error!void {
        if (computed) {
            try self.writeByte('[');
            try self.emit(key);
            try self.writeByte(']');
        } else {
            try self.emit(key);
        }
    }

    fn printObjectKey(self: *Self, key: NodeIndex, computed: bool) Error!void {
        if (comptime minify_mode) {
            if (simpleStringKey(self.tree, key)) |s| return self.writeStr(s);
        }
        try self.printPropertyKey(key, computed);
    }

    /// class member key. non-computed string keys collapse to bare
    /// identifiers the same way object keys do. computed `["…"]` collapses
    /// are gated to avoid the cases ECMA reinterprets or rejects.
    ///   `["constructor"]` on a non-static method becomes the actual
    ///     constructor (different `kind`)
    ///   `["constructor"]` on a non-static get/set is a SyntaxError per ECMA
    ///   `["constructor"]` on a field is a SyntaxError per ECMA
    ///   `["prototype"]` on any static member is a SyntaxError per ECMA
    fn printClassKey(self: *Self, key: NodeIndex, computed: bool, static: bool, is_field: bool) Error!void {
        if (comptime minify_mode) {
            if (simpleStringKey(self.tree, key)) |s| {
                if (!computed) return self.writeStr(s);
                const ctor_clash = std.mem.eql(u8, s, "constructor") and (is_field or !static);
                const proto_clash = static and std.mem.eql(u8, s, "prototype");
                if (!ctor_clash and !proto_clash) return self.writeStr(s);
            }
        }
        try self.printPropertyKey(key, computed);
    }

    fn emit_function(self: *Self, f: ast.Function) Error!void {
        if (comptime strip_ts) if (f.declare or f.type == .ts_declare_function or f.type == .ts_empty_body_function_expression) return;
        if (comptime !strip_ts) if (f.declare) try self.writeStr("declare ");
        if (f.@"async") try self.writeStr("async ");
        try self.writeStr("function");
        if (f.generator) try self.writeByte('*');
        if (f.id != .null) {
            try self.writeByte(' ');
            try self.emit(f.id);
        }
        try self.emit(f.type_parameters);
        try self.emit(f.params);
        try self.emit(f.return_type);
        if (f.body != .null) {
            try self.space();
            try self.emit(f.body);
        } else if (comptime !strip_ts) {
            try self.softSemi();
        }
    }

    fn printFunctionAsMethod(self: *Self, f: ast.Function) Error!void {
        try self.emit(f.type_parameters);
        try self.emit(f.params);
        try self.emit(f.return_type);
        if (f.body != .null) {
            try self.space();
            try self.emit(f.body);
        } else if (comptime !strip_ts) {
            try self.softSemi();
        }
    }

    fn emit_arrow_function_expression(self: *Self, a: ast.ArrowFunctionExpression) Error!void {
        if (a.@"async") try self.writeStr("async ");
        try self.emit(a.type_parameters);
        try self.emit(a.params);
        try self.emit(a.return_type);
        try self.space();
        try self.writeStr("=>");
        try self.space();
        try self.emit(a.body);
    }

    fn emit_formal_parameters(self: *Self, params: ast.FormalParameters) Error!void {
        try self.writeByte('(');
        const list = self.tree.extra(params.items);
        var first = true;
        for (list) |p| first = (try self.emitSeparated(p, first));
        if (params.rest != .null) _ = try self.emitSeparated(params.rest, first);
        try self.writeByte(')');
    }

    /// writes `, ` then emits `idx`. rolls back the separator if emit
    /// produced no output. returns the new value of `first`.
    fn emitSeparated(self: *Self, idx: NodeIndex, first: bool) Error!bool {
        const cur = self.cursor();
        if (!first) {
            try self.writeByte(',');
            try self.space();
        }
        if (try self.tryEmit(idx)) return false;
        self.restore(cur);
        return first;
    }

    fn emit_formal_parameter(self: *Self, p: ast.FormalParameter) Error!void {
        try self.emit(p.pattern);
    }

    fn emit_class(self: *Self, c: ast.Class) Error!void {
        if (comptime strip_ts) if (c.declare) return;
        try self.printDecorators(c.decorators);
        if (comptime !strip_ts) {
            if (c.declare) try self.writeStr("declare ");
            if (c.abstract) try self.writeStr("abstract ");
        }
        try self.writeStr("class");
        if (c.id != .null) {
            try self.writeByte(' ');
            try self.emit(c.id);
        }
        try self.emit(c.type_parameters);
        if (c.super_class != .null) {
            try self.writeStr(" extends ");
            try self.emitAsHead(c.super_class);
            try self.emit(c.super_type_arguments);
        }
        if (comptime !strip_ts) {
            const impl = self.tree.extra(c.implements);
            if (impl.len > 0) {
                try self.writeStr(" implements ");
                for (impl, 0..) |i, idx| {
                    if (idx > 0) {
                        try self.writeByte(',');
                        try self.space();
                    }
                    try self.emit(i);
                }
            }
        }
        try self.space();
        try self.emit(c.body);
    }

    fn emit_class_body(self: *Self, b: ast.ClassBody) Error!void {
        try self.writeByte('{');
        self.indent_depth += 1;
        var any = false;
        for (self.tree.extra(b.body)) |m| {
            const cur = self.cursor();
            const saved_semi = self.pending_semi;
            try self.flushSemi();
            try self.newline();
            if (try self.tryEmit(m)) {
                any = true;
            } else {
                self.restore(cur);
                self.pending_semi = saved_semi;
            }
        }
        self.indent_depth -= 1;
        if (any) {
            self.pending_semi = false;
            try self.newline();
        }
        try self.writeByte('}');
    }

    fn emit_method_definition(self: *Self, m: ast.MethodDefinition) Error!void {
        const fn_data = self.tree.data(m.value).function;
        if (comptime strip_ts) if (m.abstract or fn_data.body == .null) return;
        try self.printDecorators(m.decorators);
        if (comptime !strip_ts) if (m.accessibility != .none) {
            try self.writeStr(m.accessibility.toString());
            try self.writeByte(' ');
        };
        if (m.static) try self.writeStr("static ");
        if (comptime !strip_ts) {
            if (m.abstract) try self.writeStr("abstract ");
            if (m.override) try self.writeStr("override ");
        }
        switch (m.kind) {
            .get => try self.writeStr("get "),
            .set => try self.writeStr("set "),
            .constructor, .method => {
                if (fn_data.@"async") try self.writeStr("async ");
                if (fn_data.generator) try self.writeByte('*');
            },
        }
        try self.printClassKey(m.key, m.computed, m.static, false);
        if (comptime !strip_ts) if (m.optional) try self.writeByte('?');
        try self.printFunctionAsMethod(fn_data);
    }

    fn emit_property_definition(self: *Self, p: ast.PropertyDefinition) Error!void {
        if (comptime strip_ts) if (p.declare or p.abstract) return;
        try self.printDecorators(p.decorators);
        if (comptime !strip_ts) {
            if (p.declare) try self.writeStr("declare ");
            if (p.accessibility != .none) {
                try self.writeStr(p.accessibility.toString());
                try self.writeByte(' ');
            }
        }
        if (p.static) try self.writeStr("static ");
        if (comptime !strip_ts) {
            if (p.abstract) try self.writeStr("abstract ");
            if (p.override) try self.writeStr("override ");
            if (p.readonly) try self.writeStr("readonly ");
        }
        if (p.accessor) try self.writeStr("accessor ");
        try self.printClassKey(p.key, p.computed, p.static, true);
        if (comptime !strip_ts) {
            if (p.optional) try self.writeByte('?');
            if (p.definite) try self.writeByte('!');
        }
        try self.emit(p.type_annotation);
        if (p.value != .null) {
            try self.space();
            try self.writeByte('=');
            try self.space();
            try self.emit(p.value);
        }
        try self.softSemi();
    }

    fn emit_decorator(self: *Self, d: ast.Decorator) Error!void {
        try self.writeByte('@');
        try self.emit(d.expression);
    }

    fn printDecorators(self: *Self, decs: IndexRange) Error!void {
        const list = self.tree.extra(decs);
        for (list, 0..) |d, i| {
            try self.emit(d);
            // `@a.b class` would fuse to `@a.bclass` without a separator.
            if (self.pretty()) try self.newline() else if (i + 1 == list.len) try self.writeByte(' ');
        }
    }

    fn emit_import_declaration(self: *Self, d: ast.ImportDeclaration) Error!void {
        const list = self.tree.extra(d.specifiers);
        if (comptime strip_ts) {
            if (d.import_kind == .type) return;
            if (list.len > 0 and !hasValueImportSpecifier(self.tree, list)) return;
        }

        try self.writeStr("import");
        if (d.import_kind == .type) try self.writeStr(" type");
        if (d.phase) |ph| {
            try self.writeByte(' ');
            try self.writeStr(switch (ph) {
                .source => "source",
                .@"defer" => "defer",
            });
        }

        if (list.len > 0) {
            try self.writeByte(' ');
            var i: usize = 0;
            if (self.tree.data(list[0]) == .import_default_specifier) {
                try self.emit(list[0]);
                i = 1;
            }
            if (i < list.len) {
                if (i > 0) {
                    try self.writeByte(',');
                    try self.space();
                }
                if (self.tree.data(list[i]) == .import_namespace_specifier) {
                    try self.emit(list[i]);
                } else {
                    try self.writeByte('{');
                    try self.space();
                    var first = true;
                    while (i < list.len) : (i += 1) first = try self.emitSeparated(list[i], first);
                    try self.space();
                    try self.writeByte('}');
                }
            }
            try self.writeStr(" from ");
        } else {
            try self.writeByte(' ');
        }

        try self.emit(d.source);
        try self.printAttributes(d.attributes);
        try self.softSemi();
    }

    fn emit_import_specifier(self: *Self, s: ast.ImportSpecifier) Error!void {
        if (comptime strip_ts) if (s.import_kind == .type) return;
        if (s.import_kind == .type) try self.writeStr("type ");
        try self.emit(s.imported);
        if (!sameIdentifier(self.tree, s.imported, s.local)) {
            try self.writeStr(" as ");
            try self.emit(s.local);
        }
    }

    fn emit_import_default_specifier(self: *Self, s: ast.ImportDefaultSpecifier) Error!void {
        try self.emit(s.local);
    }

    fn emit_import_namespace_specifier(self: *Self, s: ast.ImportNamespaceSpecifier) Error!void {
        try self.writeStr("* as ");
        try self.emit(s.local);
    }

    fn emit_import_attribute(self: *Self, a: ast.ImportAttribute) Error!void {
        try self.emit(a.key);
        try self.writeByte(':');
        try self.space();
        try self.emit(a.value);
    }

    fn emit_import_expression(self: *Self, e: ast.ImportExpression) Error!void {
        try self.writeStr("import");
        if (e.phase) |ph| {
            try self.writeByte('.');
            try self.writeStr(switch (ph) {
                .source => "source",
                .@"defer" => "defer",
            });
        }
        try self.writeByte('(');
        try self.emit(e.source);
        if (e.options != .null) {
            try self.writeByte(',');
            try self.space();
            try self.emit(e.options);
        }
        try self.writeByte(')');
    }

    fn emit_export_named_declaration(self: *Self, d: ast.ExportNamedDeclaration) Error!void {
        if (comptime strip_ts) if (d.export_kind == .type) return;
        const list = self.tree.extra(d.specifiers);
        if (comptime strip_ts) if (d.declaration == .null and list.len > 0 and !hasValueExportSpecifier(self.tree, list)) return;

        const cur = self.cursor();
        try self.writeStr("export");
        // for `export type Foo = …`, the declaration emits its own `type`.
        if (d.export_kind == .type and d.declaration == .null) try self.writeStr(" type");
        if (d.declaration != .null) {
            try self.writeByte(' ');
            if (!try self.tryEmit(d.declaration)) self.restore(cur);
            return;
        }
        try self.space();
        try self.writeByte('{');
        if (list.len > 0) {
            try self.space();
            var first = true;
            for (list) |s| first = try self.emitSeparated(s, first);
            try self.space();
        }
        try self.writeByte('}');
        if (d.source != .null) {
            try self.writeStr(" from ");
            try self.emit(d.source);
        }
        try self.printAttributes(d.attributes);
        try self.softSemi();
    }

    fn emit_export_default_declaration(self: *Self, d: ast.ExportDefaultDeclaration) Error!void {
        const cur = self.cursor();
        try self.writeStr("export default ");
        // strip can erase the inner declaration (`export default interface ...`);
        // back out the `export default ` prefix if so
        if (!try self.tryEmit(d.declaration)) {
            self.restore(cur);
            return;
        }
        // expression default needs ';'; function/class declaration default does not
        switch (self.tree.data(d.declaration)) {
            .function, .class => {},
            else => try self.softSemi(),
        }
    }

    fn emit_export_all_declaration(self: *Self, d: ast.ExportAllDeclaration) Error!void {
        if (comptime strip_ts) if (d.export_kind == .type) return;
        try self.writeStr("export");
        if (d.export_kind == .type) try self.writeStr(" type");
        try self.writeStr(" *");
        if (d.exported != .null) {
            try self.writeStr(" as ");
            try self.emit(d.exported);
        }
        try self.writeStr(" from ");
        try self.emit(d.source);
        try self.printAttributes(d.attributes);
        try self.softSemi();
    }

    fn emit_export_specifier(self: *Self, s: ast.ExportSpecifier) Error!void {
        if (comptime strip_ts) if (s.export_kind == .type) return;
        if (s.export_kind == .type) try self.writeStr("type ");
        try self.emit(s.local);
        if (!sameIdentifier(self.tree, s.local, s.exported)) {
            try self.writeStr(" as ");
            try self.emit(s.exported);
        }
    }

    fn printAttributes(self: *Self, attrs: IndexRange) Error!void {
        const list = self.tree.extra(attrs);
        if (list.len == 0) return;
        try self.writeStr(" with ");
        try self.writeByte('{');
        try self.space();
        for (list, 0..) |a, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(a);
        }
        try self.space();
        try self.writeByte('}');
    }

    fn emit_ts_type_annotation(self: *Self, t: ast.TSTypeAnnotation) Error!void {
        try self.writeByte(':');
        try self.space();
        try self.emit(t.type_annotation);
    }

    fn emit_ts_any_keyword(self: *Self, _: ast.TSAnyKeyword) Error!void { try self.writeStr("any"); }
    fn emit_ts_unknown_keyword(self: *Self, _: ast.TSUnknownKeyword) Error!void { try self.writeStr("unknown"); }
    fn emit_ts_never_keyword(self: *Self, _: ast.TSNeverKeyword) Error!void { try self.writeStr("never"); }
    fn emit_ts_void_keyword(self: *Self, _: ast.TSVoidKeyword) Error!void { try self.writeStr("void"); }
    fn emit_ts_null_keyword(self: *Self, _: ast.TSNullKeyword) Error!void { try self.writeStr("null"); }
    fn emit_ts_undefined_keyword(self: *Self, _: ast.TSUndefinedKeyword) Error!void { try self.writeStr("undefined"); }
    fn emit_ts_string_keyword(self: *Self, _: ast.TSStringKeyword) Error!void { try self.writeStr("string"); }
    fn emit_ts_number_keyword(self: *Self, _: ast.TSNumberKeyword) Error!void { try self.writeStr("number"); }
    fn emit_ts_bigint_keyword(self: *Self, _: ast.TSBigIntKeyword) Error!void { try self.writeStr("bigint"); }
    fn emit_ts_boolean_keyword(self: *Self, _: ast.TSBooleanKeyword) Error!void { try self.writeStr("boolean"); }
    fn emit_ts_symbol_keyword(self: *Self, _: ast.TSSymbolKeyword) Error!void { try self.writeStr("symbol"); }
    fn emit_ts_object_keyword(self: *Self, _: ast.TSObjectKeyword) Error!void { try self.writeStr("object"); }
    fn emit_ts_intrinsic_keyword(self: *Self, _: ast.TSIntrinsicKeyword) Error!void { try self.writeStr("intrinsic"); }
    fn emit_ts_this_type(self: *Self, _: ast.TSThisType) Error!void { try self.writeStr("this"); }

    fn emit_ts_type_reference(self: *Self, t: ast.TSTypeReference) Error!void {
        try self.emitEntityName(t.type_name);
        try self.emit(t.type_arguments);
    }

    fn emit_ts_qualified_name(self: *Self, q: ast.TSQualifiedName) Error!void {
        try self.emitEntityName(q.left);
        try self.writeByte('.');
        try self.emit(q.right);
    }

    fn emit_ts_type_query(self: *Self, q: ast.TSTypeQuery) Error!void {
        try self.writeStr("typeof ");
        try self.emitEntityName(q.expr_name);
        try self.emit(q.type_arguments);
    }

    fn emit_ts_import_type(self: *Self, t: ast.TSImportType) Error!void {
        try self.writeStr("import(");
        try self.emit(t.source);
        if (t.options != .null) {
            try self.writeByte(',');
            try self.space();
            try self.emit(t.options);
        }
        try self.writeByte(')');
        if (t.qualifier != .null) {
            try self.writeByte('.');
            try self.emitEntityName(t.qualifier);
        }
        try self.emit(t.type_arguments);
    }

    /// emits `idx` as a TS entity name (Identifier | QualifiedName | this |
    /// ImportType). Suppresses `undefined`/`Infinity` rewrites that the
    /// expression-context emitter applies, since type queries and type
    /// references require a bare entity name.
    fn emitEntityName(self: *Self, idx: NodeIndex) Error!void {
        if (idx == .null) return;
        switch (self.tree.data(idx)) {
            .identifier_reference => |id| try self.writeString(id.name),
            else => try self.emit(idx),
        }
    }

    fn emit_ts_type_parameter(self: *Self, p: ast.TSTypeParameter) Error!void {
        if (p.@"const") try self.writeStr("const ");
        if (p.in) try self.writeStr("in ");
        if (p.out) try self.writeStr("out ");
        try self.emit(p.name);
        if (p.constraint != .null) {
            try self.writeStr(" extends ");
            try self.emit(p.constraint);
        }
        if (p.default != .null) {
            try self.space();
            try self.writeByte('=');
            try self.space();
            try self.emit(p.default);
        }
    }

    fn emit_ts_type_parameter_declaration(self: *Self, d: ast.TSTypeParameterDeclaration) Error!void {
        try self.writeByte('<');
        const list = self.tree.extra(d.params);
        for (list, 0..) |p, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(p);
        }
        try self.writeByte('>');
    }

    fn emit_ts_type_parameter_instantiation(self: *Self, d: ast.TSTypeParameterInstantiation) Error!void {
        try self.writeByte('<');
        const list = self.tree.extra(d.params);
        for (list, 0..) |p, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(p);
        }
        try self.writeByte('>');
    }

    fn emit_ts_literal_type(self: *Self, t: ast.TSLiteralType) Error!void {
        // In a type position `true`/`false` are literal types, not expressions:
        // the minify-mode `!0`/`!1` shorthand is invalid syntax here. Emit the
        // keyword form directly and let everything else (string/number/bigint/
        // template) fall through to its normal emitter.
        switch (self.tree.data(t.literal)) {
            .boolean_literal => |b| try self.writeStr(if (b.value) "true" else "false"),
            else => try self.emit(t.literal),
        }
    }

    fn emit_ts_template_literal_type(self: *Self, t: ast.TSTemplateLiteralType) Error!void {
        try self.writeByte('`');
        const quasis = self.tree.extra(t.quasis);
        const types = self.tree.extra(t.types);
        for (quasis, 0..) |q, i| {
            try self.emit(q);
            if (i < types.len) {
                try self.writeStr("${");
                try self.emit(types[i]);
                try self.writeByte('}');
            }
        }
        try self.writeByte('`');
    }

    fn emit_ts_array_type(self: *Self, t: ast.TSArrayType) Error!void {
        try self.emit(t.element_type);
        try self.writeStr("[]");
    }

    fn emit_ts_indexed_access_type(self: *Self, t: ast.TSIndexedAccessType) Error!void {
        try self.emit(t.object_type);
        try self.writeByte('[');
        try self.emit(t.index_type);
        try self.writeByte(']');
    }

    fn emit_ts_tuple_type(self: *Self, t: ast.TSTupleType) Error!void {
        try self.writeByte('[');
        const list = self.tree.extra(t.element_types);
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(x);
        }
        try self.writeByte(']');
    }

    fn emit_ts_named_tuple_member(self: *Self, m: ast.TSNamedTupleMember) Error!void {
        try self.emit(m.label);
        if (m.optional) try self.writeByte('?');
        try self.writeByte(':');
        try self.space();
        try self.emit(m.element_type);
    }

    fn emit_ts_optional_type(self: *Self, t: ast.TSOptionalType) Error!void {
        try self.emit(t.type_annotation);
        try self.writeByte('?');
    }

    fn emit_ts_rest_type(self: *Self, t: ast.TSRestType) Error!void {
        try self.writeStr("...");
        try self.emit(t.type_annotation);
    }

    fn emit_ts_jsdoc_nullable_type(self: *Self, t: ast.TSJSDocNullableType) Error!void {
        if (t.postfix) {
            try self.emit(t.type_annotation);
            try self.writeByte('?');
        } else {
            try self.writeByte('?');
            try self.emit(t.type_annotation);
        }
    }

    fn emit_ts_jsdoc_non_nullable_type(self: *Self, t: ast.TSJSDocNonNullableType) Error!void {
        if (t.postfix) {
            try self.emit(t.type_annotation);
            try self.writeByte('!');
        } else {
            try self.writeByte('!');
            try self.emit(t.type_annotation);
        }
    }

    fn emit_ts_jsdoc_unknown_type(self: *Self, _: ast.TSJSDocUnknownType) Error!void {
        try self.writeByte('?');
    }

    fn emit_ts_union_type(self: *Self, t: ast.TSUnionType) Error!void {
        const list = self.tree.extra(t.types);
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.space();
                try self.writeByte('|');
                try self.space();
            }
            try self.emit(x);
        }
    }

    fn emit_ts_intersection_type(self: *Self, t: ast.TSIntersectionType) Error!void {
        const list = self.tree.extra(t.types);
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.space();
                try self.writeByte('&');
                try self.space();
            }
            try self.emit(x);
        }
    }

    fn emit_ts_conditional_type(self: *Self, t: ast.TSConditionalType) Error!void {
        try self.emit(t.check_type);
        try self.writeStr(" extends ");
        try self.emit(t.extends_type);
        try self.space();
        try self.writeByte('?');
        try self.space();
        try self.emit(t.true_type);
        try self.space();
        try self.writeByte(':');
        try self.space();
        try self.emit(t.false_type);
    }

    fn emit_ts_infer_type(self: *Self, t: ast.TSInferType) Error!void {
        try self.writeStr("infer ");
        try self.emit(t.type_parameter);
    }

    fn emit_ts_type_operator(self: *Self, t: ast.TSTypeOperator) Error!void {
        try self.writeStr(t.operator.toString());
        try self.writeByte(' ');
        try self.emit(t.type_annotation);
    }

    fn emit_ts_parenthesized_type(self: *Self, t: ast.TSParenthesizedType) Error!void {
        try self.writeByte('(');
        try self.emit(t.type_annotation);
        try self.writeByte(')');
    }

    fn emit_ts_function_type(self: *Self, t: ast.TSFunctionType) Error!void {
        try self.emit(t.type_parameters);
        try self.emit(t.params);
        try self.space();
        try self.writeStr("=>");
        try self.space();
        try self.emitUnwrappedType(t.return_type);
    }

    fn emit_ts_constructor_type(self: *Self, t: ast.TSConstructorType) Error!void {
        if (t.abstract) try self.writeStr("abstract ");
        try self.writeStr("new ");
        try self.emit(t.type_parameters);
        try self.emit(t.params);
        try self.space();
        try self.writeStr("=>");
        try self.space();
        try self.emitUnwrappedType(t.return_type);
    }

    /// strips the `ts_type_annotation` wrapper before emitting. used for
    /// arrow-form return types and `is`-predicates where the wrapper's
    /// leading `:` would be wrong.
    fn emitUnwrappedType(self: *Self, idx: NodeIndex) Error!void {
        if (idx == .null) return;
        const data = self.tree.data(idx);
        if (data == .ts_type_annotation) {
            try self.emit(data.ts_type_annotation.type_annotation);
        } else {
            try self.emit(idx);
        }
    }

    fn emit_ts_type_predicate(self: *Self, t: ast.TSTypePredicate) Error!void {
        if (t.asserts) try self.writeStr("asserts ");
        try self.emit(t.parameter_name);
        if (t.type_annotation != .null) {
            try self.writeStr(" is ");
            try self.emitUnwrappedType(t.type_annotation);
        }
    }

    fn emit_ts_type_literal(self: *Self, t: ast.TSTypeLiteral) Error!void {
        try self.printSignatureBody(t.members);
    }

    fn emit_ts_mapped_type(self: *Self, t: ast.TSMappedType) Error!void {
        try self.writeByte('{');
        try self.space();
        try self.printMappedModifier(t.readonly, "readonly");
        try self.writeByte('[');
        try self.emit(t.key);
        try self.writeStr(" in ");
        try self.emit(t.constraint);
        if (t.name_type != .null) {
            try self.writeStr(" as ");
            try self.emit(t.name_type);
        }
        try self.writeByte(']');
        try self.printMappedOptional(t.optional);
        if (t.type_annotation != .null) {
            try self.writeByte(':');
            try self.space();
            try self.emit(t.type_annotation);
        }
        try self.softSemi();
        self.pending_semi = false;
        try self.space();
        try self.writeByte('}');
    }

    fn printMappedModifier(self: *Self, m: ast.TSMappedTypeModifier, keyword: []const u8) Error!void {
        switch (m) {
            .none => {},
            .true => {
                try self.writeStr(keyword);
                try self.writeByte(' ');
            },
            .plus => {
                try self.writeByte('+');
                try self.writeStr(keyword);
                try self.writeByte(' ');
            },
            .minus => {
                try self.writeByte('-');
                try self.writeStr(keyword);
                try self.writeByte(' ');
            },
        }
    }

    fn printMappedOptional(self: *Self, m: ast.TSMappedTypeModifier) Error!void {
        switch (m) {
            .none => {},
            .true => try self.writeByte('?'),
            .plus => try self.writeStr("+?"),
            .minus => try self.writeStr("-?"),
        }
    }

    fn emit_ts_property_signature(self: *Self, s: ast.TSPropertySignature) Error!void {
        if (s.readonly) try self.writeStr("readonly ");
        try self.printPropertyKey(s.key, s.computed);
        if (s.optional) try self.writeByte('?');
        if (s.type_annotation != .null) try self.emit(s.type_annotation);
        try self.softSemi();
    }

    fn emit_ts_method_signature(self: *Self, s: ast.TSMethodSignature) Error!void {
        switch (s.kind) {
            .get => try self.writeStr("get "),
            .set => try self.writeStr("set "),
            .method => {},
        }
        try self.printPropertyKey(s.key, s.computed);
        if (s.optional) try self.writeByte('?');
        try self.emit(s.type_parameters);
        try self.emit(s.params);
        if (s.return_type != .null) try self.emit(s.return_type);
        try self.softSemi();
    }

    fn emit_ts_call_signature_declaration(self: *Self, s: ast.TSCallSignatureDeclaration) Error!void {
        try self.emit(s.type_parameters);
        try self.emit(s.params);
        if (s.return_type != .null) try self.emit(s.return_type);
        try self.softSemi();
    }

    fn emit_ts_construct_signature_declaration(self: *Self, s: ast.TSConstructSignatureDeclaration) Error!void {
        try self.writeStr("new ");
        try self.emit(s.type_parameters);
        try self.emit(s.params);
        if (s.return_type != .null) try self.emit(s.return_type);
        try self.softSemi();
    }

    fn emit_ts_index_signature(self: *Self, s: ast.TSIndexSignature) Error!void {
        if (s.static) try self.writeStr("static ");
        if (s.readonly) try self.writeStr("readonly ");
        try self.writeByte('[');
        const params = self.tree.extra(s.parameters);
        for (params, 0..) |p, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(p);
        }
        try self.writeByte(']');
        try self.emit(s.type_annotation);
        try self.softSemi();
    }

    /// emits a `{ … }` block of ts signatures (type literal or interface body).
    fn printSignatureBody(self: *Self, items: IndexRange) Error!void {
        try self.writeByte('{');
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
        }
        try self.writeByte('}');
    }

    fn emit_ts_type_alias_declaration(self: *Self, d: ast.TSTypeAliasDeclaration) Error!void {
        if (d.declare) try self.writeStr("declare ");
        try self.writeStr("type ");
        try self.emit(d.id);
        try self.emit(d.type_parameters);
        try self.space();
        try self.writeByte('=');
        try self.space();
        try self.emit(d.type_annotation);
        try self.softSemi();
    }

    fn emit_ts_interface_declaration(self: *Self, d: ast.TSInterfaceDeclaration) Error!void {
        if (d.declare) try self.writeStr("declare ");
        try self.writeStr("interface ");
        try self.emit(d.id);
        try self.emit(d.type_parameters);
        const heritage = self.tree.extra(d.extends);
        if (heritage.len > 0) {
            try self.writeStr(" extends ");
            for (heritage, 0..) |h, i| {
                if (i > 0) {
                    try self.writeByte(',');
                    try self.space();
                }
                try self.emit(h);
            }
        }
        try self.space();
        try self.emit(d.body);
    }

    fn emit_ts_interface_body(self: *Self, b: ast.TSInterfaceBody) Error!void {
        try self.printSignatureBody(b.body);
    }

    fn emit_ts_interface_heritage(self: *Self, h: ast.TSInterfaceHeritage) Error!void {
        try self.emit(h.expression);
        try self.emit(h.type_arguments);
    }

    fn emit_ts_class_implements(self: *Self, c: ast.TSClassImplements) Error!void {
        try self.emit(c.expression);
        try self.emit(c.type_arguments);
    }

    fn emit_ts_enum_declaration(self: *Self, d: ast.TSEnumDeclaration) Error!void {
        if (d.declare) try self.writeStr("declare ");
        if (d.is_const) try self.writeStr("const ");
        try self.writeStr("enum ");
        try self.emit(d.id);
        try self.space();
        try self.emit(d.body);
    }

    fn emit_ts_enum_body(self: *Self, b: ast.TSEnumBody) Error!void {
        try self.writeByte('{');
        const list = self.tree.extra(b.members);
        if (list.len > 0) {
            self.indent_depth += 1;
            for (list, 0..) |m, i| {
                try self.newline();
                try self.emit(m);
                if (i < list.len - 1) try self.writeByte(',');
            }
            self.indent_depth -= 1;
            try self.newline();
        }
        try self.writeByte('}');
    }

    fn emit_ts_enum_member(self: *Self, m: ast.TSEnumMember) Error!void {
        if (m.computed) {
            try self.writeByte('[');
            try self.emit(m.id);
            try self.writeByte(']');
        } else {
            try self.emit(m.id);
        }
        if (m.initializer != .null) {
            try self.space();
            try self.writeByte('=');
            try self.space();
            try self.emit(m.initializer);
        }
    }

    fn emit_ts_module_declaration(self: *Self, d: ast.TSModuleDeclaration) Error!void {
        if (d.declare) try self.writeStr("declare ");
        try self.writeStr(d.kind.toString());
        try self.writeByte(' ');
        try self.emit(d.id);
        if (d.body != .null) {
            try self.space();
            try self.emit(d.body);
        } else {
            try self.softSemi();
        }
    }

    fn emit_ts_module_block(self: *Self, b: ast.TSModuleBlock) Error!void {
        try self.printBlock(b.body);
    }

    fn emit_ts_global_declaration(self: *Self, d: ast.TSGlobalDeclaration) Error!void {
        if (d.declare) try self.writeStr("declare ");
        try self.emit(d.id);
        try self.space();
        try self.emit(d.body);
    }

    fn emit_ts_as_expression(self: *Self, e: ast.TSAsExpression) Error!void {
        try self.emit(e.expression);
        try self.writeStr(" as ");
        try self.emit(e.type_annotation);
    }

    fn emit_ts_satisfies_expression(self: *Self, e: ast.TSSatisfiesExpression) Error!void {
        try self.emit(e.expression);
        try self.writeStr(" satisfies ");
        try self.emit(e.type_annotation);
    }

    fn emit_ts_type_assertion(self: *Self, e: ast.TSTypeAssertion) Error!void {
        try self.writeByte('<');
        // `<<T>` would re-lex as `<<` (left shift).
        if (typeStartsWithLeftAngle(self.tree, e.type_annotation)) try self.writeByte(' ');
        try self.emit(e.type_annotation);
        try self.writeByte('>');
        try self.emit(e.expression);
    }

    fn emit_ts_non_null_expression(self: *Self, e: ast.TSNonNullExpression) Error!void {
        try self.emit(e.expression);
        try self.writeByte('!');
    }

    fn emit_ts_instantiation_expression(self: *Self, e: ast.TSInstantiationExpression) Error!void {
        try self.emit(e.expression);
        try self.emit(e.type_arguments);
    }

    fn emit_ts_export_assignment(self: *Self, e: ast.TSExportAssignment) Error!void {
        try self.writeStr("export");
        try self.space();
        try self.writeByte('=');
        try self.space();
        try self.emit(e.expression);
        try self.softSemi();
    }

    fn emit_ts_namespace_export_declaration(self: *Self, d: ast.TSNamespaceExportDeclaration) Error!void {
        try self.writeStr("export as namespace ");
        try self.emit(d.id);
        try self.softSemi();
    }

    fn emit_ts_import_equals_declaration(self: *Self, d: ast.TSImportEqualsDeclaration) Error!void {
        try self.writeStr("import ");
        if (d.import_kind == .type) try self.writeStr("type ");
        try self.emit(d.id);
        try self.space();
        try self.writeByte('=');
        try self.space();
        try self.emit(d.module_reference);
        try self.softSemi();
    }

    fn emit_ts_external_module_reference(self: *Self, r: ast.TSExternalModuleReference) Error!void {
        try self.writeStr("require(");
        try self.emit(r.expression);
        try self.writeByte(')');
    }

    fn emit_ts_parameter_property(self: *Self, p: ast.TSParameterProperty) Error!void {
        try self.printDecorators(p.decorators);
        if (p.accessibility != .none) {
            try self.writeStr(p.accessibility.toString());
            try self.writeByte(' ');
        }
        if (p.override) try self.writeStr("override ");
        if (p.readonly) try self.writeStr("readonly ");
        try self.emit(p.parameter);
    }

    fn emit_ts_this_parameter(self: *Self, p: ast.TSThisParameter) Error!void {
        try self.writeStr("this");
        if (p.type_annotation != .null) try self.emit(p.type_annotation);
    }

    fn emit_jsx_element(self: *Self, e: ast.JSXElement) Error!void {
        try self.emit(e.opening_element);
        for (self.tree.extra(e.children)) |c| try self.emit(c);
        if (e.closing_element != .null) try self.emit(e.closing_element);
    }

    fn emit_jsx_opening_element(self: *Self, o: ast.JSXOpeningElement) Error!void {
        try self.writeByte('<');
        try self.emit(o.name);
        try self.emit(o.type_arguments);
        for (self.tree.extra(o.attributes)) |a| {
            try self.writeByte(' ');
            try self.emit(a);
        }
        if (o.self_closing) {
            try self.space();
            try self.writeStr("/>");
        } else {
            try self.writeByte('>');
        }
    }

    fn emit_jsx_closing_element(self: *Self, c: ast.JSXClosingElement) Error!void {
        try self.writeStr("</");
        try self.emit(c.name);
        try self.writeByte('>');
    }

    fn emit_jsx_fragment(self: *Self, f: ast.JSXFragment) Error!void {
        try self.emit(f.opening_fragment);
        for (self.tree.extra(f.children)) |c| try self.emit(c);
        try self.emit(f.closing_fragment);
    }

    fn emit_jsx_opening_fragment(self: *Self, _: ast.JSXOpeningFragment) Error!void {
        try self.writeStr("<>");
    }

    fn emit_jsx_closing_fragment(self: *Self, _: ast.JSXClosingFragment) Error!void {
        try self.writeStr("</>");
    }

    fn emit_jsx_identifier(self: *Self, id: ast.JSXIdentifier) Error!void {
        try self.writeString(id.name);
    }

    fn emit_jsx_namespaced_name(self: *Self, n: ast.JSXNamespacedName) Error!void {
        try self.emit(n.namespace);
        try self.writeByte(':');
        try self.emit(n.name);
    }

    fn emit_jsx_member_expression(self: *Self, m: ast.JSXMemberExpression) Error!void {
        try self.emit(m.object);
        try self.writeByte('.');
        try self.emit(m.property);
    }

    fn emit_jsx_attribute(self: *Self, a: ast.JSXAttribute) Error!void {
        try self.emit(a.name);
        if (a.value != .null) {
            try self.writeByte('=');
            try self.emit(a.value);
        }
    }

    fn emit_jsx_spread_attribute(self: *Self, a: ast.JSXSpreadAttribute) Error!void {
        try self.writeByte('{');
        try self.writeStr("...");
        try self.emit(a.argument);
        try self.writeByte('}');
    }

    fn emit_jsx_expression_container(self: *Self, c: ast.JSXExpressionContainer) Error!void {
        try self.writeByte('{');
        try self.emit(c.expression);
        try self.writeByte('}');
    }

    fn emit_jsx_empty_expression(_: *Self, _: ast.JSXEmptyExpression) Error!void {}

    fn emit_jsx_text(self: *Self, t: ast.JSXText) Error!void {
        try self.writeString(t.value);
    }

    fn emit_jsx_spread_child(self: *Self, c: ast.JSXSpreadChild) Error!void {
        try self.writeByte('{');
        try self.writeStr("...");
        try self.emit(c.expression);
        try self.writeByte('}');
    }
    };
}

fn isWordOp(op: []const u8) bool {
    return std.mem.eql(u8, op, "in") or std.mem.eql(u8, op, "instanceof") or
        std.mem.eql(u8, op, "typeof") or std.mem.eql(u8, op, "void") or
        std.mem.eql(u8, op, "delete");
}

fn leftmostByteIs(tree: *const Tree, idx: NodeIndex, byte: u8) bool {
    if (idx == .null) return false;
    return switch (tree.data(idx)) {
        .unary_expression => |u| switch (u.operator) {
            .positive => byte == '+',
            .negate => byte == '-',
            .logical_not => byte == '!',
            .bitwise_not => byte == '~',
            else => false, // typeof/void/delete start with a letter
        },
        .update_expression => |u| if (u.prefix) switch (u.operator) {
            .increment => byte == '+',
            .decrement => byte == '-',
        } else leftmostByteIs(tree, u.argument, byte),
        .regexp_literal => byte == '/',
        .member_expression => |m| leftmostByteIs(tree, m.object, byte),
        .call_expression => |c| leftmostByteIs(tree, c.callee, byte),
        .chain_expression => |c| leftmostByteIs(tree, c.expression, byte),
        .tagged_template_expression => |tt| leftmostByteIs(tree, tt.tag, byte),
        .binary_expression => |b| leftmostByteIs(tree, b.left, byte),
        .logical_expression => |l| leftmostByteIs(tree, l.left, byte),
        .conditional_expression => |c| leftmostByteIs(tree, c.@"test", byte),
        .assignment_expression => |a| leftmostByteIs(tree, a.left, byte),
        .sequence_expression => |s| blk: {
            const list = tree.extra(s.expressions);
            break :blk list.len > 0 and leftmostByteIs(tree, list[0], byte);
        },
        .ts_as_expression => |e| leftmostByteIs(tree, e.expression, byte),
        .ts_satisfies_expression => |e| leftmostByteIs(tree, e.expression, byte),
        .ts_non_null_expression => |e| leftmostByteIs(tree, e.expression, byte),
        .ts_instantiation_expression => |e| leftmostByteIs(tree, e.expression, byte),
        else => false,
    };
}

fn sameIdentifier(tree: *const Tree, a: NodeIndex, b: NodeIndex) bool {
    if (a == .null or b == .null) return false;
    const an = identifierStringOrNull(tree, a) orelse return false;
    const bn = identifierStringOrNull(tree, b) orelse return false;
    return std.mem.eql(u8, tree.string(an), tree.string(bn));
}

// Whether an object property / binding property can stay in shorthand form
// (`{ name }`) at emit time. The parser sets `shorthand = true` when the
// source was written that way, but a later rename pass (the mangler) may
// have changed the value-side binding without touching the key. At that
// point `{ name: a }` is required to preserve which property is being
// read/destructured. Peels through `assignment_pattern` so `{ name = 1 }`
// is still considered shorthand when the binding kept the same name.
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

fn isBareAsyncIdentifier(tree: *const Tree, idx: NodeIndex) bool {
    const name = identifierStringOrNull(tree, idx) orelse return false;
    return std.mem.eql(u8, tree.string(name), "async");
}

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

fn isMinifiedToNonPrimary(tree: *const Tree, idx: NodeIndex) bool {
    return switch (tree.data(idx)) {
        .boolean_literal => true,
        .identifier_reference => |id| blk: {
            const name = tree.string(id.name);
            break :blk std.mem.eql(u8, name, "undefined") or std.mem.eql(u8, name, "Infinity");
        },
        else => false,
    };
}

/// callers gate this on minify mode. returns true when `idx` would be
/// emitted as an unparenthesized UnaryExpression, i.e. `true`/`false` ->
/// `!0`/`!1` and `undefined` -> `void 0`. (`Infinity` -> `1/0` is a
/// BinaryExpression, not a unary, so it doesn't count here)
fn minifiesToUnary(tree: *const Tree, idx: NodeIndex) bool {
    return switch (tree.data(idx)) {
        .boolean_literal => true,
        .identifier_reference => |id| std.mem.eql(u8, tree.string(id.name), "undefined"),
        else => false,
    };
}

/// returns the decoded value of `idx` if it's a string literal whose
/// contents form a valid IdentifierName. used by the `obj["foo"]` to
/// `obj.foo` and `{"foo": x}` to `{foo: x}` rewrites.
fn simpleStringKey(tree: *const Tree, idx: NodeIndex) ?[]const u8 {
    const lit = switch (tree.data(idx)) {
        .string_literal => |l| l,
        else => return null,
    };
    const s = tree.string(lit.value);
    return if (isIdentifierName(s)) s else null;
}

inline fn isIdCont(c: u8) bool {
    return c == '$' or c >= 0x80 or util.UnicodeId.canContinueId(c);
}

fn isIdentifierName(s: []const u8) bool {
    if (s.len == 0) return false;
    var i: usize = 0;
    while (i < s.len) {
        const cp = util.Utf.codePointAt(s, i) catch return false;
        const ok = if (i == 0) util.UnicodeId.canStartId(cp.value) else util.UnicodeId.canContinueId(cp.value);
        if (!ok) return false;
        i += cp.len;
    }
    return true;
}

fn isDecimalLiteral(tree: *const Tree, idx: NodeIndex) bool {
    return switch (tree.data(idx)) {
        .numeric_literal => |lit| lit.kind == .decimal,
        else => false,
    };
}

fn stripUnderscores(raw: []const u8, buf: []u8) ?[]const u8 {
    if (std.mem.indexOfScalar(u8, raw, '_') == null) return raw;
    if (raw.len > buf.len) return null;
    var len: usize = 0;
    for (raw) |c| if (c != '_') {
        buf[len] = c;
        len += 1;
    };
    return buf[0..len];
}

/// returns the shortest semantically-equivalent spelling of decimal `s`.
/// falls back to `s` whenever no rewrite is shorter. `scratch` and `s` must
/// not alias each other.
fn shortestDecimal(s: []const u8, scratch: []u8) []const u8 {
    const exp_at = std.mem.indexOfAny(u8, s, "eE") orelse s.len;
    const mantissa = s[0..exp_at];
    const exp_suffix = s[exp_at..]; // empty or `e<n>` / `E<n>`

    const dot_at = std.mem.indexOfScalar(u8, mantissa, '.') orelse mantissa.len;
    var int_part = mantissa[0..dot_at];
    const frac_raw = if (dot_at < mantissa.len) mantissa[dot_at + 1 ..] else "";
    const frac_part = std.mem.trimEnd(u8, frac_raw, "0");

    // `0.5` shortens to `.5` once the leading zero is bare.
    if (frac_part.len > 0 and std.mem.eql(u8, int_part, "0")) int_part = "";

    // `0.0`, `.0`, `0e10`, and similar all collapse to `0`.
    if (int_part.len == 0 and frac_part.len == 0) return "0";

    // pure integer with three or more trailing zeros, the `Ne<k>` form shortens.
    if (frac_part.len == 0 and exp_suffix.len == 0) {
        const head = std.mem.trimEnd(u8, int_part, "0");
        const trail = int_part.len - head.len;
        if (trail >= 3 and head.len > 0) {
            const out = std.fmt.bufPrint(scratch, "{s}e{d}", .{ head, trail }) catch return s;
            if (out.len < s.len) return out;
        }
    }

    const dot = if (frac_part.len > 0) "." else "";
    const out = std.fmt.bufPrint(scratch, "{s}{s}{s}{s}", .{ int_part, dot, frac_part, exp_suffix }) catch return s;
    return if (out.len < s.len) out else s;
}
