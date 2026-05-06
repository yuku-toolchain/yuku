const std = @import("std");
const ast = @import("../ast.zig");

const Allocator = std.mem.Allocator;
const Tree = ast.Tree;
const NodeIndex = ast.NodeIndex;
const NodeData = ast.NodeData;
const IndexRange = ast.IndexRange;

/// Whitespace mode for the output.
pub const Format = enum {
    /// Indented, with spaces around operators and after commas.
    pretty,
    /// No discretionary whitespace; only the separators the JS grammar requires.
    compact,
};

/// Source map output mode. `v3` is reserved for a later phase.
pub const SourceMap = enum { none, v3 };

/// Quote style for string literals.
pub const Quotes = enum { double, single };

/// Codegen options.
pub const Options = struct {
    /// Whitespace mode. See `Format`.
    format: Format = .pretty,
    /// Spaces per indentation level (used only when `format == .pretty`).
    indent: u8 = 2,
    /// Source map output mode.
    sourcemap: SourceMap = .none,
    /// Quote style for emitted string literals.
    quotes: Quotes = .double,
    /// Append a trailing newline to the output if missing.
    final_newline: bool = true,
};

/// A codegen-detected problem in the input tree.
pub const Diagnostic = struct {
    /// A human-readable description of the problem.
    message: []const u8,
    /// The start position of the problem in the source.
    start: u32,
    /// The end position of the problem in the source.
    end: u32,
};

/// Output of a codegen run.
///
/// All buffers are allocated from the caller's allocator, call `deinit` with
/// the same allocator to free them.
pub const Result = struct {
    /// Generated source code.
    code: []const u8,
    /// Source map, when `Options.sourcemap != .none`.
    map: ?[]const u8,
    /// Codegen-detected problems. Empty when codegen succeeded cleanly.
    errors: []const Diagnostic,

    pub fn deinit(self: Result, allocator: Allocator) void {
        allocator.free(self.code);
        if (self.map) |m| allocator.free(m);
        allocator.free(self.errors);
    }
};

/// All return-value errors are allocation errors. Codegen-detected
/// user-code problems are reported in `Result.errors`.
pub const Error = error{OutOfMemory};

/// Renders a `Tree` to source code.
pub fn print(allocator: Allocator, tree: *Tree, options: Options) Error!Result {
    return printImpl(false, allocator, tree, options);
}

pub fn printImpl(comptime strip_ts: bool, allocator: Allocator, tree: *Tree, options: Options) Error!Result {
    var p = try Printer(strip_ts).init(allocator, tree, options);
    defer p.deinit();
    try p.printRoot();
    return .{
        .code = try p.code.toOwnedSlice(allocator),
        .map = null,
        .errors = try p.errors.toOwnedSlice(allocator),
    };
}

fn Printer(comptime strip_ts: bool) type {
    return struct {
        const Self = @This();

        tree: *Tree,
        code: std.ArrayList(u8) = .empty,
        errors: std.ArrayList(Diagnostic) = .empty,
        options: Options,
        arena: std.heap.ArenaAllocator,
        allocator: Allocator,
        indent_depth: u32 = 0,

    fn init(allocator: Allocator, tree: *Tree, options: Options) Error!Self {
        var p = Self{
            .tree = tree,
            .options = options,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .allocator = allocator,
        };
        try p.code.ensureTotalCapacity(allocator, tree.source.len);
        return p;
    }

    fn deinit(self: *Self) void {
        self.arena.deinit();
        self.code.deinit(self.allocator);
        self.errors.deinit(self.allocator);
    }

    inline fn pretty(self: *const Self) bool {
        return self.options.format == .pretty;
    }

    inline fn writeByte(self: *Self, b: u8) Error!void {
        try self.code.append(self.allocator, b);
    }

    inline fn writeStr(self: *Self, s: []const u8) Error!void {
        try self.code.appendSlice(self.allocator, s);
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
    }

    fn printRoot(self: *Self) Error!void {
        try self.emit(self.tree.root);
        if (self.options.final_newline and (self.code.items.len == 0 or self.code.items[self.code.items.len - 1] != '\n')) {
            try self.writeByte('\n');
        }
    }

    fn emit(self: *Self, idx: NodeIndex) Error!void {
        if (idx == .null) return;
        const data = self.tree.data(idx);

        if (comptime strip_ts) {
            switch (data) {
                // type-only declarations and signatures, erased silently
                .ts_type_alias_declaration,
                .ts_interface_declaration,
                .ts_global_declaration,
                .ts_namespace_export_declaration,
                .ts_index_signature,
                .ts_this_parameter,
                => return,
                // expression wrappers, pass through to the inner expression
                .ts_as_expression => |e| return self.emit(e.expression),
                .ts_satisfies_expression => |e| return self.emit(e.expression),
                .ts_type_assertion => |e| return self.emit(e.expression),
                .ts_non_null_expression => |e| return self.emit(e.expression),
                .ts_instantiation_expression => |e| return self.emit(e.expression),
                // not strippable, report as error and skip
                .ts_enum_declaration => return self.diagnose(idx, "TypeScript enums cannot be stripped to JavaScript"),
                .ts_module_declaration => return self.diagnose(idx, "TypeScript namespaces cannot be stripped to JavaScript"),
                .ts_export_assignment => return self.diagnose(idx, "`export =` cannot be stripped to JavaScript"),
                .ts_import_equals_declaration => return self.diagnose(idx, "`import = require()` cannot be stripped to JavaScript"),
                // parameter property, report and emit the inner pattern so call arity is preserved
                .ts_parameter_property => |pp| {
                    try self.diagnose(idx, "parameter properties cannot be stripped to JavaScript");
                    return self.emit(pp.parameter);
                },
                else => {},
            }
        }

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
    }

    fn printStmtList(self: *Self, items: IndexRange) Error!void {
        const list = self.tree.extra(items);
        var first = true;
        for (list) |s| {
            const before = self.code.items.len;
            if (!first) try self.newline();
            const after_sep = self.code.items.len;
            try self.emit(s);
            if (self.code.items.len == after_sep) {
                self.code.shrinkRetainingCapacity(before);
            } else {
                first = false;
            }
        }
    }

    fn printBlock(self: *Self, items: IndexRange) Error!void {
        try self.writeByte('{');
        const list = self.tree.extra(items);
        if (list.len > 0) {
            const before = self.code.items.len;
            self.indent_depth += 1;
            try self.newline();
            const after_indent = self.code.items.len;
            try self.printStmtList(items);
            self.indent_depth -= 1;
            if (self.code.items.len == after_indent) {
                self.code.shrinkRetainingCapacity(before);
            } else {
                try self.newline();
            }
        }
        try self.writeByte('}');
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
        try self.writeByte(';');
    }

    fn emit_empty_statement(self: *Self, _: ast.EmptyStatement) Error!void {
        try self.writeByte(';');
    }

    fn emit_debugger_statement(self: *Self, _: ast.DebuggerStatement) Error!void {
        try self.writeStr("debugger;");
    }

    fn emit_expression_statement(self: *Self, s: ast.ExpressionStatement) Error!void {
        try self.emit(s.expression);
        try self.writeByte(';');
    }

    fn emit_if_statement(self: *Self, s: ast.IfStatement) Error!void {
        try self.writeStr("if");
        try self.space();
        try self.writeByte('(');
        try self.emit(s.@"test");
        try self.writeByte(')');
        try self.space();
        try self.emit(s.consequent);
        if (s.alternate != .null) {
            try self.space();
            try self.writeStr("else ");
            try self.emit(s.alternate);
        }
    }

    fn emit_return_statement(self: *Self, s: ast.ReturnStatement) Error!void {
        try self.writeStr("return");
        if (s.argument != .null) {
            try self.writeByte(' ');
            try self.emit(s.argument);
        }
        try self.writeByte(';');
    }

    fn emit_throw_statement(self: *Self, s: ast.ThrowStatement) Error!void {
        try self.writeStr("throw ");
        try self.emit(s.argument);
        try self.writeByte(';');
    }

    fn emit_break_statement(self: *Self, s: ast.BreakStatement) Error!void {
        try self.writeStr("break");
        if (s.label != .null) {
            try self.writeByte(' ');
            try self.emit(s.label);
        }
        try self.writeByte(';');
    }

    fn emit_continue_statement(self: *Self, s: ast.ContinueStatement) Error!void {
        try self.writeStr("continue");
        if (s.label != .null) {
            try self.writeByte(' ');
            try self.emit(s.label);
        }
        try self.writeByte(';');
    }

    fn emit_labeled_statement(self: *Self, s: ast.LabeledStatement) Error!void {
        try self.emit(s.label);
        try self.writeByte(':');
        try self.space();
        try self.emit(s.body);
    }

    fn emit_with_statement(self: *Self, s: ast.WithStatement) Error!void {
        try self.writeStr("with");
        try self.space();
        try self.writeByte('(');
        try self.emit(s.object);
        try self.writeByte(')');
        try self.space();
        try self.emit(s.body);
    }

    fn emit_while_statement(self: *Self, s: ast.WhileStatement) Error!void {
        try self.writeStr("while");
        try self.space();
        try self.writeByte('(');
        try self.emit(s.@"test");
        try self.writeByte(')');
        try self.space();
        try self.emit(s.body);
    }

    fn emit_do_while_statement(self: *Self, s: ast.DoWhileStatement) Error!void {
        try self.writeStr("do ");
        try self.emit(s.body);
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
        try self.emit(s.body);
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
        try self.emit(s.body);
    }

    fn emit_for_of_statement(self: *Self, s: ast.ForOfStatement) Error!void {
        try self.writeStr("for");
        if (s.@"await") try self.writeStr(" await");
        try self.space();
        try self.writeByte('(');
        try self.printForLeft(s.left);
        try self.writeStr(" of ");
        try self.emit(s.right);
        try self.writeByte(')');
        try self.space();
        try self.emit(s.body);
    }

    fn printForLeft(self: *Self, idx: NodeIndex) Error!void {
        switch (self.tree.data(idx)) {
            .variable_declaration => |d| try self.printVariableDecl(d, false),
            else => try self.emit(idx),
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
                try self.newline();
                try self.emit(c);
            }
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
        const list = self.tree.extra(c.consequent);
        if (list.len > 0) {
            self.indent_depth += 1;
            for (list) |s| {
                try self.newline();
                try self.emit(s);
            }
            self.indent_depth -= 1;
        }
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
        if (with_semicolon) try self.writeByte(';');
    }

    fn emit_variable_declarator(self: *Self, d: ast.VariableDeclarator) Error!void {
        try self.emit(d.id);
        if (d.init != .null) {
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
        try self.emit(e.left);
        const op = e.operator.toString();
        if (isWordOp(op)) {
            try self.writeByte(' ');
            try self.writeStr(op);
            try self.writeByte(' ');
        } else {
            try self.space();
            try self.writeStr(op);
            try self.space();
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
        try self.emit(e.argument);
    }

    fn emit_update_expression(self: *Self, e: ast.UpdateExpression) Error!void {
        const op = e.operator.toString();
        if (e.prefix) {
            try self.writeStr(op);
            try self.emit(e.argument);
        } else {
            try self.emit(e.argument);
            try self.writeStr(op);
        }
    }

    fn emit_assignment_expression(self: *Self, e: ast.AssignmentExpression) Error!void {
        try self.emit(e.left);
        try self.space();
        try self.writeStr(e.operator.toString());
        try self.space();
        try self.emit(e.right);
    }

    fn emit_array_expression(self: *Self, e: ast.ArrayExpression) Error!void {
        try self.writeByte('[');
        const list = self.tree.extra(e.elements);
        for (list, 0..) |x, i| {
            if (i > 0) {
                try self.writeByte(',');
                try self.space();
            }
            try self.emit(x);
        }
        try self.writeByte(']');
    }

    fn emit_object_expression(self: *Self, e: ast.ObjectExpression) Error!void {
        try self.writeByte('{');
        const list = self.tree.extra(e.properties);
        if (list.len > 0) {
            try self.space();
            for (list, 0..) |x, i| {
                if (i > 0) {
                    try self.writeByte(',');
                    try self.space();
                }
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
            try self.printPropertyKey(p.key, p.computed);
            try self.printFunctionAsMethod(fn_data);
            return;
        }

        if (p.shorthand) {
            try self.emit(p.value);
            return;
        }

        try self.printPropertyKey(p.key, p.computed);
        try self.writeByte(':');
        try self.space();
        try self.emit(p.value);
    }

    fn emit_spread_element(self: *Self, s: ast.SpreadElement) Error!void {
        try self.writeStr("...");
        try self.emit(s.argument);
    }

    fn emit_member_expression(self: *Self, e: ast.MemberExpression) Error!void {
        try self.emit(e.object);
        if (e.computed) {
            if (e.optional) try self.writeStr("?.");
            try self.writeByte('[');
            try self.emit(e.property);
            try self.writeByte(']');
        } else {
            try self.writeStr(if (e.optional) "?." else ".");
            try self.emit(e.property);
        }
    }

    fn emit_call_expression(self: *Self, e: ast.CallExpression) Error!void {
        try self.emit(e.callee);
        if (e.optional) try self.writeStr("?.");
        try self.printArgList(e.arguments);
    }

    fn emit_chain_expression(self: *Self, e: ast.ChainExpression) Error!void {
        try self.emit(e.expression);
    }

    fn emit_new_expression(self: *Self, e: ast.NewExpression) Error!void {
        try self.writeStr("new ");
        try self.emit(e.callee);
        try self.printArgList(e.arguments);
    }

    fn emit_tagged_template_expression(self: *Self, e: ast.TaggedTemplateExpression) Error!void {
        try self.emit(e.tag);
        try self.emit(e.quasi);
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
        const q: u8 = if (self.options.quotes == .single) '\'' else '"';
        try self.writeByte(q);
        try self.writeEscapedString(self.tree.string(value), q);
        try self.writeByte(q);
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
                0 => "\\0",
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
        try self.writeString(lit.raw);
    }

    fn emit_bigint_literal(self: *Self, lit: ast.BigIntLiteral) Error!void {
        try self.writeString(lit.raw);
        try self.writeByte('n');
    }

    fn emit_boolean_literal(self: *Self, lit: ast.BooleanLiteral) Error!void {
        try self.writeStr(if (lit.value) "true" else "false");
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
        try self.writeString(id.name);
    }

    fn emit_identifier_name(self: *Self, id: ast.IdentifierName) Error!void {
        try self.writeString(id.name);
    }

    fn emit_binding_identifier(self: *Self, id: ast.BindingIdentifier) Error!void {
        try self.writeString(id.name);
    }

    fn emit_label_identifier(self: *Self, id: ast.LabelIdentifier) Error!void {
        try self.writeString(id.name);
    }

    fn emit_private_identifier(self: *Self, id: ast.PrivateIdentifier) Error!void {
        try self.writeByte('#');
        try self.writeString(id.name);
    }

    fn emit_assignment_pattern(self: *Self, p: ast.AssignmentPattern) Error!void {
        try self.emit(p.left);
        try self.space();
        try self.writeByte('=');
        try self.space();
        try self.emit(p.right);
    }

    fn emit_binding_rest_element(self: *Self, r: ast.BindingRestElement) Error!void {
        try self.writeStr("...");
        try self.emit(r.argument);
    }

    fn emit_array_pattern(self: *Self, p: ast.ArrayPattern) Error!void {
        try self.writeByte('[');
        const list = self.tree.extra(p.elements);
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
        try self.writeByte(']');
    }

    fn emit_object_pattern(self: *Self, p: ast.ObjectPattern) Error!void {
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
    }

    fn emit_binding_property(self: *Self, p: ast.BindingProperty) Error!void {
        if (p.shorthand) {
            try self.emit(p.value);
            return;
        }
        try self.printPropertyKey(p.key, p.computed);
        try self.writeByte(':');
        try self.space();
        try self.emit(p.value);
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

    fn emit_function(self: *Self, f: ast.Function) Error!void {
        if (comptime strip_ts) if (f.declare or f.type == .ts_declare_function or f.type == .ts_empty_body_function_expression) return;
        if (f.@"async") try self.writeStr("async ");
        try self.writeStr("function");
        if (f.generator) try self.writeByte('*');
        if (f.id != .null) {
            try self.writeByte(' ');
            try self.emit(f.id);
        }
        try self.emit(f.params);
        if (f.body != .null) {
            try self.space();
            try self.emit(f.body);
        }
    }

    fn printFunctionAsMethod(self: *Self, f: ast.Function) Error!void {
        try self.emit(f.params);
        if (f.body != .null) {
            try self.space();
            try self.emit(f.body);
        }
    }

    fn emit_arrow_function_expression(self: *Self, a: ast.ArrowFunctionExpression) Error!void {
        if (a.@"async") try self.writeStr("async ");
        try self.emit(a.params);
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

    /// writes `, ` then emits `idx`. rolls back the separator if emit produced
    /// no output. returns the new value of `first`.
    fn emitSeparated(self: *Self, idx: NodeIndex, first: bool) Error!bool {
        const before = self.code.items.len;
        if (!first) {
            try self.writeByte(',');
            try self.space();
        }
        const after_sep = self.code.items.len;
        try self.emit(idx);
        if (self.code.items.len == after_sep) {
            self.code.shrinkRetainingCapacity(before);
            return first;
        }
        return false;
    }

    fn emit_formal_parameter(self: *Self, p: ast.FormalParameter) Error!void {
        try self.emit(p.pattern);
    }

    fn emit_class(self: *Self, c: ast.Class) Error!void {
        if (comptime strip_ts) if (c.declare) return;
        try self.printDecorators(c.decorators);
        try self.writeStr("class");
        if (c.id != .null) {
            try self.writeByte(' ');
            try self.emit(c.id);
        }
        if (c.super_class != .null) {
            try self.writeStr(" extends ");
            try self.emit(c.super_class);
        }
        try self.space();
        try self.emit(c.body);
    }

    fn emit_class_body(self: *Self, b: ast.ClassBody) Error!void {
        try self.writeByte('{');
        const list = self.tree.extra(b.body);
        self.indent_depth += 1;
        var emitted = false;
        for (list) |m| {
            const before = self.code.items.len;
            try self.newline();
            const after_nl = self.code.items.len;
            try self.emit(m);
            if (self.code.items.len == after_nl) {
                self.code.shrinkRetainingCapacity(before);
            } else {
                emitted = true;
            }
        }
        self.indent_depth -= 1;
        if (emitted) try self.newline();
        try self.writeByte('}');
    }

    fn emit_method_definition(self: *Self, m: ast.MethodDefinition) Error!void {
        const fn_data = self.tree.data(m.value).function;
        if (comptime strip_ts) if (m.abstract or fn_data.body == .null) return;
        try self.printDecorators(m.decorators);
        if (m.static) try self.writeStr("static ");
        switch (m.kind) {
            .get => try self.writeStr("get "),
            .set => try self.writeStr("set "),
            .constructor, .method => {
                if (fn_data.@"async") try self.writeStr("async ");
                if (fn_data.generator) try self.writeByte('*');
            },
        }
        try self.printPropertyKey(m.key, m.computed);
        try self.printFunctionAsMethod(fn_data);
    }

    fn emit_property_definition(self: *Self, p: ast.PropertyDefinition) Error!void {
        if (comptime strip_ts) if (p.declare or p.abstract) return;
        try self.printDecorators(p.decorators);
        if (p.static) try self.writeStr("static ");
        if (p.accessor) try self.writeStr("accessor ");
        try self.printPropertyKey(p.key, p.computed);
        if (p.value != .null) {
            try self.space();
            try self.writeByte('=');
            try self.space();
            try self.emit(p.value);
        }
        try self.writeByte(';');
    }

    fn emit_decorator(self: *Self, d: ast.Decorator) Error!void {
        try self.writeByte('@');
        try self.emit(d.expression);
    }

    fn printDecorators(self: *Self, decs: IndexRange) Error!void {
        const list = self.tree.extra(decs);
        for (list) |d| {
            try self.emit(d);
            try self.newline();
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
                    while (i < list.len) : (i += 1) {
                        const spec = self.tree.data(list[i]);
                        if (comptime strip_ts) if (spec == .import_specifier and spec.import_specifier.import_kind == .type) continue;
                        if (!first) {
                            try self.writeByte(',');
                            try self.space();
                        }
                        first = false;
                        try self.emit(list[i]);
                    }
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
        try self.writeByte(';');
    }

    fn emit_import_specifier(self: *Self, s: ast.ImportSpecifier) Error!void {
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

        try self.writeStr("export");
        if (d.export_kind == .type) try self.writeStr(" type");
        if (d.declaration != .null) {
            // a stripped declaration (`export declare …`, `export interface …`)
            // produces nothing, so back out the `export` keyword to avoid a stray prefix
            const before_decl = self.code.items.len;
            try self.writeByte(' ');
            const after_sep = self.code.items.len;
            try self.emit(d.declaration);
            if (self.code.items.len == after_sep) {
                self.code.shrinkRetainingCapacity(before_decl - "export".len);
            }
            return;
        }
        try self.space();
        try self.writeByte('{');
        if (list.len > 0) {
            try self.space();
            var first = true;
            for (list) |s| {
                const sd = self.tree.data(s);
                if (comptime strip_ts) if (sd == .export_specifier and sd.export_specifier.export_kind == .type) continue;
                if (!first) {
                    try self.writeByte(',');
                    try self.space();
                }
                first = false;
                try self.emit(s);
            }
            try self.space();
        }
        try self.writeByte('}');
        if (d.source != .null) {
            try self.writeStr(" from ");
            try self.emit(d.source);
        }
        try self.printAttributes(d.attributes);
        try self.writeByte(';');
    }

    fn emit_export_default_declaration(self: *Self, d: ast.ExportDefaultDeclaration) Error!void {
        try self.writeStr("export default ");
        try self.emit(d.declaration);
        // expression default needs ';'; function/class declaration default does not
        switch (self.tree.data(d.declaration)) {
            .function, .class => {},
            else => try self.writeByte(';'),
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
        try self.writeByte(';');
    }

    fn emit_export_specifier(self: *Self, s: ast.ExportSpecifier) Error!void {
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
    };
}

fn isWordOp(op: []const u8) bool {
    return std.mem.eql(u8, op, "in") or std.mem.eql(u8, op, "instanceof") or
        std.mem.eql(u8, op, "typeof") or std.mem.eql(u8, op, "void") or
        std.mem.eql(u8, op, "delete");
}

fn sameIdentifier(tree: *const Tree, a: NodeIndex, b: NodeIndex) bool {
    if (a == .null or b == .null) return false;
    const an = identifierStringOrNull(tree, a) orelse return false;
    const bn = identifierStringOrNull(tree, b) orelse return false;
    return std.mem.eql(u8, tree.string(an), tree.string(bn));
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
