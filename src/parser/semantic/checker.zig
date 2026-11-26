const std = @import("std");
const spec = @import("spec");
const traverser = @import("../traverser/root.zig");
const ast = @import("../ast.zig");
const lexical = @import("../lexical.zig");
const binder = @import("binder.zig");
const scope = @import("scope.zig");

const Allocator = std.mem.Allocator;
const static_semantics = spec.static_semantics;
const Symbol = binder.Symbol;
const Ctx = traverser.semantic.Ctx;
const NodeIndex = ast.NodeIndex;
const eql = std.mem.eql;

const Error = Allocator.Error;

pub const Checker = struct {
    tree: *ast.Tree,
    allocator: Allocator,

    exported_names: std.array_hash_map.String(ExportedName) = .empty,
    local_kinds: std.StringHashMapUnmanaged(Symbol.Flags) = .empty,
    export_specifiers: std.ArrayList(ExportSpecifierInfo) = .empty,
    plain_functions: std.AutoHashMapUnmanaged(NodeIndex, void) = .empty,

    const ExportedName = struct {
        first: NodeIndex,
        last: NodeIndex,
        // every export but interfaces and overload signatures, as tsc counts
        counted: u32 = 0,
        flags: Symbol.Flags = .{},
        alias: bool = false,
        local: ?[]const u8 = null,
        // a conflict the order of the exports decides
        conflict: bool = false,
    };

    const ExportForm = union(enum) {
        alias: ?[]const u8,
        // an `export default` expression merges with no earlier export
        default_expression,
        declaration: struct { flags: Symbol.Flags, counted: bool },
    };

    const ExportSpecifierInfo = struct { local_name: []const u8, node: NodeIndex };

    const Self = @This();

    pub fn enter_binding_identifier(
        self: *Self,
        id: ast.BindingIdentifier,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        const name = ctx.tree.string(id.name);
        const flags = ctx.symbols.pending.flags;

        // type-position identifiers are labels unless they are type parameters
        if (ctx.inTypePosition() and !flags.type_parameter) return;

        // https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
        if (isStrict(ctx) and !flags.ambient) {
            try self.checkStrictReserved(name, node, ctx, "a binding identifier");
            if (lexical.isEvalOrArguments(name)) {
                const message = "'{s}' cannot be used as a binding identifier in strict mode";
                try self.report(node, try self.fmt(message, .{name}), .{});
            }
        } else if (flags.block_scoped_var and name.len == "let".len and eql(u8, name, "let")) {
            const message =
                "'let' is not allowed to be used as a name in 'let' or 'const' declarations";
            try self.report(node, message, .{});
        }

        try self.checkRedeclaration(node, name, flags, ctx);

        switch (ctx.symbols.export_state) {
            .named => try self.recordExportedName(name, node, ctx, exportForm(flags, ctx)),
            .none => try self.recordLocalKinds(name, flags, ctx),
            .default => {},
        }
    }

    fn checkRedeclaration(
        self: *Self,
        node: NodeIndex,
        name: []const u8,
        flags: Symbol.Flags,
        ctx: *Ctx,
    ) Error!void {
        const symbols = &ctx.symbols;
        const pending = symbols.pending;
        const target = pending.scope;

        if (symbols.prior(name, ctx.scope.scopes.items)) |sym| {
            const existing = symbols.symbol(sym).flags;
            // annex B lets a sloppy block declare a plain function twice (B.3.2.4)
            if (pending.conflicts(existing) and !(self.plain_functions.contains(node) and
                self.plain_functions.contains(symbols.firstDeclOf(sym))))
            {
                return self.reportRedeclaration(name, node, sym, ctx);
            }

            // duplicate params are legal only in sloppy simple-parameter-list functions (15.1.1)
            // https://tc39.es/ecma262/#sec-parameter-lists-static-semantics-early-errors
            if (existing.parameter and flags.parameter) {
                if (enclosingParameters(ctx)) |params| {
                    const must_be_unique = isStrict(ctx) or
                        params.kind == .unique_formal_parameters or
                        params.kind == .arrow_formal_parameters or
                        static_semantics.findNonSimpleParameter(ctx.tree, params) != null;
                    if (must_be_unique) try self.reportRedeclaration(name, node, sym, ctx);
                }
            }
        }

        // a body's lexical names may not duplicate the signature one scope up (14.15.1, 15.2.1)
        if (!flags.function_scoped_var and !flags.function) {
            const target_scope = ctx.scope.get(target);
            const outer = target_scope.parent;
            const outer_holds_signature = outer != .none and switch (target_scope.kind) {
                .function_body => true,
                .block => switch (ctx.tree.data(ctx.scope.get(outer).node)) {
                    .catch_clause => |clause| clause.body == target_scope.node,
                    else => false,
                },
                else => false,
            };
            if (outer_holds_signature) {
                if (symbols.ownBinding(outer, name)) |sym| {
                    if (pending.conflicts(symbols.symbol(sym).flags)) {
                        try self.reportRedeclaration(name, node, sym, ctx);
                    }
                }
            }
        }

        // a hoisting name conflicts with lexical names in every scope it
        // passes through (14.2.1, 15.2.1)
        if (flags.isHoistingVar() or flags.function) {
            var iter = ctx.scope.ancestors(ctx.scope.current);
            while (iter.next()) |scope_id| {
                if (scope_id == target) break;
                const sym = symbols.ownBinding(scope_id, name) orelse continue;
                const existing = symbols.symbol(sym).flags;
                // a var may collide with a simple catch parameter, the annex B.3.4 carve-out
                if (existing.isBlockScopedLike() or (existing.catch_var and
                    !binder.isSimpleCatch(ctx.tree, ctx.scope.get(scope_id))))
                {
                    try self.reportRedeclaration(name, node, sym, ctx);
                    break;
                }
            }
        }
    }

    /// https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
    pub inline fn enter_identifier_reference(
        self: *Self,
        id: ast.IdentifierReference,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (ctx.inTypePosition()) return;

        const name = ctx.tree.string(id.name);
        if (lexical.mayBeStrictReserved(name) or lexical.isArguments(name)) {
            try self.checkIdentifierReference(name, node, ctx);
        }
    }

    fn checkIdentifierReference(
        self: *Self,
        name: []const u8,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        try self.checkStrictReserved(name, node, ctx, "an identifier");

        // https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
        if (!lexical.isArguments(name)) return;
        const holder = enclosing(ctx, &.{ .function, .property_definition, .static_block });
        if (holder != null and holder.? != .function) {
            const message = "'arguments' cannot be referenced in property initializers or class" ++
                " static initialization blocks";
            try self.report(node, message, .{});
        }
    }

    /// https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
    pub fn enter_label_identifier(
        self: *Self,
        id: ast.LabelIdentifier,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        try self.checkStrictReserved(ctx.tree.string(id.name), node, ctx, "a label");
    }

    /// https://tc39.es/ecma262/#sec-string-literals-early-errors
    pub inline fn enter_string_literal(
        self: *Self,
        literal: ast.StringLiteral,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (!isStrict(ctx)) return;
        const verbatim = literal.value.start == literal.raw.start + 1 and
            literal.value.end + 1 == literal.raw.end;
        if (!verbatim) try self.checkStringEscapes(node);
    }

    fn checkStringEscapes(self: *Self, node: NodeIndex) Error!void {
        const span = self.tree.span(node);
        if (spec.escapes.hasOctalEscape(self.tree.source[span.start..span.end])) {
            try self.report(node, "Octal escape sequences are not allowed in strict mode", .{
                .help = "Use a hexadecimal escape instead, such as '\\x07'",
            });
        }
    }

    /// https://tc39.es/ecma262/#sec-additional-syntax-numeric-literals
    pub inline fn enter_numeric_literal(
        self: *Self,
        _: ast.NumericLiteral,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (!isStrict(ctx)) return;

        const span = ctx.tree.span(node);
        const raw = ctx.tree.source[span.start..span.end];
        const legacy = raw.len >= 2 and raw[0] == '0' and raw[1] >= '0' and raw[1] <= '9';
        if (legacy) try self.reportLegacyNumericLiteral(node, raw);
    }

    fn reportLegacyNumericLiteral(self: *Self, node: NodeIndex, raw: []const u8) Error!void {
        @branchHint(.cold);
        if (std.mem.findAny(u8, raw, "89") == null) {
            const trimmed = std.mem.trimStart(u8, raw, "0");
            const digits = if (trimmed.len == 0) "0" else trimmed;
            try self.report(node, "Octal literals are not allowed in strict mode", .{
                .help = try self.fmt("Use the syntax '0o{s}'", .{digits}),
            });
        } else {
            try self.report(node, "Decimals with leading zeros are not allowed in strict mode", .{
                .help = "Remove the leading zeros, or use a '0o', '0x', or '0b' prefix",
            });
        }
    }

    /// https://tc39.es/ecma262/#sec-function-definitions-static-semantics-early-errors
    pub fn enter_directive(
        self: *Self,
        directive: ast.Directive,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (!eql(u8, ctx.tree.string(directive.value), "use strict")) return;

        const function = enclosing(ctx, &.{ .function, .arrow_function_expression });
        const params_index = switch (function orelse return) {
            inline .function, .arrow_function_expression => |f| f.params,
            else => unreachable,
        };
        if (params_index == .null) return;

        const params = ctx.tree.data(params_index).formal_parameters;
        if (static_semantics.findNonSimpleParameter(ctx.tree, params)) |param| {
            const message = "'use strict' directive cannot be used with non-simple parameter list";
            try self.report(node, message, .{
                .labels = try self.labels(&.{self.label(param, "non-simple parameter")}),
            });
        }
    }

    // where a function declaration may stand, labelled (14.13.1, B.3.1) or as an if body (B.3.3)
    pub fn enter_function(
        self: *Self,
        func: ast.Function,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (func.id != .null and !func.async and !func.generator) {
            if (!ctx.scope.get(ctx.symbols.pending.scope).flags.strict) {
                try self.plain_functions.put(self.allocator, func.id, {});
            }
        }
        if (func.type != .function_declaration) return;

        const is_strict = isStrict(ctx);
        var through_labels = false;
        var iter = ctx.path.ancestors();
        _ = iter.next(); // skip self

        const parent = while (iter.next()) |idx| {
            const data = ctx.tree.data(idx);
            if (data != .labeled_statement) break data;
            through_labels = true;
        } else return;

        const is_valid = switch (parent) {
            .program,
            .function_body,
            .class_body,
            .static_block,
            .block_statement,
            .switch_case,
            .export_named_declaration,
            .export_default_declaration,
            .ts_module_block,
            => !is_strict or !through_labels,
            .if_statement => !is_strict and !through_labels,
            else => false,
        };
        if (!is_valid) {
            const message = if (is_strict)
                "In strict mode code, functions can only be declared at the top level or" ++
                    " inside a block"
            else
                "In non-strict mode code, functions can only be declared at the top level," ++
                    " inside a block, or as the body of an 'if' statement";
            try self.report(node, message, .{});
        }
    }

    pub fn enter_yield_expression(
        self: *Self,
        _: ast.YieldExpression,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (enclosingParameters(ctx) != null) {
            try self.report(node, "'yield' expressions cannot be used in a parameter list", .{});
        }
    }

    pub fn enter_await_expression(
        self: *Self,
        _: ast.AwaitExpression,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        const holder = enclosing(ctx, &.{
            .formal_parameters,
            .static_block,
            .function,
            .arrow_function_expression,
        });
        const message = switch (holder orelse return) {
            .formal_parameters => "'await' expressions cannot be used in a parameter list",
            // ClassStaticBlockBody uses [~Await]
            .static_block => "'await' expression cannot be used inside a class static block",
            else => return,
        };
        try self.report(node, message, .{});
    }

    /// https://tc39.es/ecma262/#sec-update-expressions-static-semantics-early-errors
    pub fn enter_update_expression(
        self: *Self,
        expr: ast.UpdateExpression,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (isStrict(ctx)) try self.checkEvalArguments(expr.argument, node, ctx);
    }

    /// https://tc39.es/ecma262/#sec-assignment-operators-static-semantics-early-errors
    pub fn enter_assignment_expression(
        self: *Self,
        expr: ast.AssignmentExpression,
        _: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (isStrict(ctx)) try self.checkAssignTargetEvalArguments(expr.left, ctx);
    }

    /// https://tc39.es/ecma262/#sec-static-semantics-allprivateidentifiersvalid
    pub fn enter_binary_expression(
        self: *Self,
        expr: ast.BinaryExpression,
        _: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (expr.operator == .in and ctx.tree.data(expr.left) == .private_identifier) {
            try self.checkPrivateName(expr.left, ctx);
        }
    }

    pub fn enter_unary_expression(
        self: *Self,
        expr: ast.UnaryExpression,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (expr.operator != .delete) return;
        var target = static_semantics.unwrapParens(ctx.tree, expr.argument);
        if (ctx.tree.data(target) == .chain_expression) {
            const chain = ctx.tree.data(target).chain_expression;
            target = static_semantics.unwrapParens(ctx.tree, chain.expression);
        }

        switch (ctx.tree.data(target)) {
            // https://tc39.es/ecma262/#sec-delete-operator-static-semantics-early-errors
            .identifier_reference => if (isStrict(ctx)) {
                const message = "'delete' cannot be called on an identifier in strict mode";
                try self.report(node, message, .{});
            },
            .member_expression => |m| {
                if (!m.computed and ctx.tree.data(m.property) == .private_identifier) {
                    const message =
                        "The operand of a 'delete' operator cannot be a private identifier";
                    try self.report(node, message, .{});
                }
            },
            else => {},
        }
    }

    /// https://tc39.es/ecma262/#sec-__proto__-property-names-in-object-initializers
    pub fn enter_object_expression(
        self: *Self,
        obj: ast.ObjectExpression,
        _: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        var first_proto: ?NodeIndex = null;
        for (ctx.tree.extra(obj.properties)) |child| {
            if (ctx.tree.data(child) != .object_property) continue;
            const prop = ctx.tree.data(child).object_property;
            if (prop.computed or prop.method or prop.shorthand or prop.kind != .init) continue;
            const pn = static_semantics.propName(ctx.tree, prop.key) orelse continue;
            if (!pn.eql("__proto__")) continue;

            const first = first_proto orelse {
                first_proto = child;
                continue;
            };
            try self.report(child, "Duplicate '__proto__' property in an object literal", .{
                .help = "Set the prototype only once",
                .labels = try self.labels(&.{self.label(first, "first defined here")}),
            });
        }
    }

    /// https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub inline fn enter_call_expression(
        self: *Self,
        expr: ast.CallExpression,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (ctx.tree.data(expr.callee) == .super) try self.checkSuperCall(node, ctx);
    }

    fn checkSuperCall(self: *Self, node: NodeIndex, ctx: *Ctx) Error!void {
        var above = ctx.path.ancestors();
        const in_constructor = switch (superHome(ctx, &above)) {
            .function => |parent| parent != null and switch (ctx.tree.data(parent.?)) {
                .method_definition => |method| method.kind == .constructor,
                else => false,
            },
            else => false,
        };
        const class: ?ast.Class = if (in_constructor) while (above.next()) |i| {
            if (ctx.tree.data(i) == .class) break ctx.tree.data(i).class;
        } else null else null;

        if (class == null) {
            const message = "Super calls are not permitted outside constructors or in nested" ++
                " functions inside constructors";
            try self.report(node, message, .{
                .help = "Use an arrow function to keep the constructor's 'super' binding",
            });
        } else if (class.?.super_class == .null) {
            try self.report(node, "'super' can only be referenced in a derived class", .{
                .help = "Add an 'extends' clause, or remove the 'super()' call",
            });
        }
    }

    pub inline fn enter_member_expression(
        self: *Self,
        expr: ast.MemberExpression,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        const on_super = ctx.tree.data(expr.object) == .super;
        const private = !expr.computed and ctx.tree.data(expr.property) == .private_identifier;
        if (on_super or private) try self.checkMemberExpression(expr, node, on_super, private, ctx);
    }

    fn checkMemberExpression(
        self: *Self,
        expr: ast.MemberExpression,
        node: NodeIndex,
        on_super: bool,
        private: bool,
        ctx: *Ctx,
    ) Error!void {
        if (on_super and !isSuperPropertyValid(ctx)) {
            const message = "'super' can only be referenced in members of derived classes or" ++
                " object literal expressions";
            try self.report(node, message, .{
                .help = "Use an arrow function to keep the method's 'super' binding",
            });
        }
        // 13.3.7 SuperProperty admits no PrivateIdentifier, and 15.7.7
        // AllPrivateIdentifiersValid requires a declaring class
        if (!private) return;
        if (on_super) {
            try self.report(node, "Private identifiers cannot be accessed on 'super'", .{});
        } else {
            try self.checkPrivateName(expr.property, ctx);
        }
    }

    /// https://tc39.es/ecma262/#sec-left-hand-side-expressions-static-semantics-early-errors
    pub fn enter_meta_property(
        self: *Self,
        prop: ast.MetaProperty,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        const meta = ctx.tree.data(prop.meta);
        if (meta != .identifier_name) return;
        const name = ctx.tree.string(meta.identifier_name.name);

        if (eql(u8, name, "import") and !ctx.tree.isModule()) {
            try self.report(node, "Cannot use 'import.meta' outside a module", .{});
        }

        // https://tc39.es/ecma262/#sec-scripts-static-semantics-early-errors
        // https://tc39.es/ecma262/#sec-module-semantics-static-semantics-early-errors
        // commonjs top level is a function body
        if (eql(u8, name, "new") and ctx.tree.source_type != .commonjs and
            enclosing(ctx, &.{ .function, .static_block, .property_definition }) == null)
        {
            const message = "'new.target' is only allowed in functions, class field" ++
                " initializers, and static blocks";
            try self.report(node, message, .{});
        }
    }

    pub fn enter_import_declaration(
        self: *Self,
        decl: ast.ImportDeclaration,
        _: NodeIndex,
        _: *Ctx,
    ) Error!void {
        try self.checkDuplicateWithAttributes(decl.attributes);
    }

    pub fn enter_export_named_declaration(
        self: *Self,
        decl: ast.ExportNamedDeclaration,
        _: NodeIndex,
        _: *Ctx,
    ) Error!void {
        try self.checkDuplicateWithAttributes(decl.attributes);
    }

    pub fn enter_export_default_declaration(
        self: *Self,
        decl: ast.ExportDefaultDeclaration,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        const form = defaultExportForm(ctx.tree, decl.declaration);
        try self.recordExportedName("default", node, ctx, form);
    }

    pub fn enter_export_all_declaration(
        self: *Self,
        decl: ast.ExportAllDeclaration,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (decl.exported != .null) {
            const name = moduleExportName(ctx.tree, decl.exported);
            try self.recordExportedName(name, node, ctx, .{ .alias = null });
        }
        try self.checkDuplicateWithAttributes(decl.attributes);
    }

    /// https://tc39.es/ecma262/#sec-exports-static-semantics-early-errors
    pub fn enter_export_specifier(
        self: *Self,
        specifier: ast.ExportSpecifier,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        const declaration = ctx.tree.data(ctx.path.parent().?).export_named_declaration;
        const local = if (declaration.source == .null)
            moduleExportName(ctx.tree, specifier.local)
        else
            null;
        const exported_name = moduleExportName(ctx.tree, specifier.exported);
        try self.recordExportedName(exported_name, node, ctx, .{ .alias = local });

        // only local value exports are resolved, as the checker tracks the value scope alone
        if (specifier.export_kind == .type or declaration.export_kind == .type) return;
        if (ctx.inTsNamespace()) return;
        if (local) |local_name| {
            const info: ExportSpecifierInfo = .{ .local_name = local_name, .node = node };
            try self.export_specifiers.append(self.allocator, info);
        }
    }

    /// https://tc39.es/ecma262/#sec-with-statement-static-semantics-early-errors
    pub fn enter_with_statement(
        self: *Self,
        _: ast.WithStatement,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (isStrict(ctx)) {
            try self.report(node, "'with' statements are not allowed in strict mode", .{});
        }
    }

    /// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements-static-semantics-early-errors
    pub fn enter_for_of_statement(
        self: *Self,
        stmt: ast.ForOfStatement,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (stmt.await and !ctx.tree.isTs()) {
            const tags = [_]ast.NodeTag{ .static_block, .function, .arrow_function_expression };
            const holder = enclosing(ctx, &tags);
            if (holder != null and holder.? == .static_block) {
                const message = "'for await' loops cannot be used inside a class static block";
                try self.report(node, message, .{});
            }
        }
        try self.checkForInOfInitializer(ctx, stmt.left, false);
        if (isStrict(ctx)) try self.checkAssignTargetEvalArguments(stmt.left, ctx);
    }

    /// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements-static-semantics-early-errors
    pub fn enter_for_in_statement(
        self: *Self,
        stmt: ast.ForInStatement,
        _: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        try self.checkForInOfInitializer(ctx, stmt.left, true);
        if (isStrict(ctx)) try self.checkAssignTargetEvalArguments(stmt.left, ctx);
    }

    pub fn enter_variable_declaration(
        self: *Self,
        decl: ast.VariableDeclaration,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if ((decl.kind == .using or decl.kind == .await_using) and
            ctx.tree.source_type == .script and isAtProgramLevel(ctx))
        {
            const message = "'{s}' declarations are not allowed at the top level of a script";
            try self.report(node, try self.fmt(message, .{decl.kind.toString()}), .{});
        }
    }

    pub fn enter_break_statement(
        self: *Self,
        stmt: ast.BreakStatement,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (stmt.label != .null) {
            _ = try self.jumpTarget(stmt.label, node, ctx, "break");
        } else if (!isInsideJumpTarget(ctx, true)) {
            const message = "A 'break' statement can only be used within an enclosing iteration" ++
                " or switch statement";
            try self.report(node, message, .{});
        }
    }

    pub fn enter_continue_statement(
        self: *Self,
        stmt: ast.ContinueStatement,
        node: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        if (stmt.label != .null) {
            const body = try self.jumpTarget(stmt.label, node, ctx, "continue") orelse
                return;
            const target = ctx.tree.data(body);
            if (!target.isIteration() and target != .labeled_statement) {
                const name = labelName(ctx.tree, stmt.label);
                try self.report(node, continue_label_message, .{
                    .help = try self.fmt("Label '{s}' does not mark a loop", .{name}),
                });
            }
        } else if (!isInsideJumpTarget(ctx, false)) {
            const message = "A 'continue' statement can only be used within an enclosing" ++
                " iteration statement";
            try self.report(node, message, .{});
        }
    }

    const continue_label_message = "A 'continue' statement can only jump to a label of an" ++
        " enclosing iteration statement";

    /// https://tc39.es/ecma262/#sec-labelled-statements-static-semantics-early-errors
    pub fn enter_labeled_statement(
        self: *Self,
        stmt: ast.LabeledStatement,
        _: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        const name = labelName(ctx.tree, stmt.label);
        var iter = ctx.path.ancestors();
        _ = iter.next(); // skip self
        while (iter.next()) |i| {
            const data = ctx.tree.data(i);
            if (data == .labeled_statement) {
                if (eql(u8, labelName(ctx.tree, data.labeled_statement.label), name)) {
                    const message = try self.fmt("Duplicate label '{s}'", .{name});
                    try self.report(stmt.label, message, .{});
                }
            }
            if (isFunctionBoundary(data)) break;
        }
    }

    /// Per section 14.12, the CaseBlock grammar permits at most one DefaultClause.
    pub fn enter_switch_statement(
        self: *Self,
        stmt: ast.SwitchStatement,
        _: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        var first_default: ?NodeIndex = null;
        for (ctx.tree.extra(stmt.cases)) |child| {
            if (ctx.tree.data(child).switch_case.@"test" != .null) continue;
            const first = first_default orelse {
                first_default = child;
                continue;
            };
            const message =
                "A 'default' clause cannot appear more than once in a 'switch' statement";
            try self.report(child, message, .{
                .labels = try self.labels(&.{self.label(first, "first 'default' clause here")}),
            });
        }
    }

    /// https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn enter_class_body(
        self: *Self,
        body: ast.ClassBody,
        _: NodeIndex,
        ctx: *Ctx,
    ) Error!void {
        const children = ctx.tree.extra(body.body);
        var first_constructor: ?NodeIndex = null;

        for (children, 0..) |child, j| {
            const child_data = ctx.tree.data(child);

            // a body-less method is a ts overload, abstract, or ambient signature
            if (child_data == .method_definition) {
                const method = child_data.method_definition;
                const has_body = ctx.tree.data(method.value).function.body != .null;
                if (method.kind == .constructor and has_body) {
                    if (first_constructor) |first| {
                        const first_label = self.label(first, "first constructor here");
                        const message = "Multiple constructor implementations are not allowed";
                        try self.report(method.key, message, .{
                            .labels = try self.labels(&.{first_label}),
                        });
                    } else {
                        first_constructor = method.key;
                    }
                }
            }

            const name_j = privateElementName(ctx.tree, child_data) orelse continue;
            const kind_j = privateElementKind(ctx.tree, child_data);

            for (children[0..j]) |prev| {
                const prev_data = ctx.tree.data(prev);
                const name_i = privateElementName(ctx.tree, prev_data) orelse continue;
                if (!eql(u8, name_i, name_j)) continue;

                const kind_i = privateElementKind(ctx.tree, prev_data);
                const same_static =
                    privateElementIsStatic(prev_data) == privateElementIsStatic(child_data);
                if (privateElementsPair(kind_i, kind_j) and same_static) continue;

                try self.report(child, try self.fmt("Duplicate private name '#{s}'", .{name_j}), .{
                    .labels = try self.labels(&.{self.label(prev, "first declared here")}),
                });
                break;
            }
        }
    }

    const PrivateElementKind = enum { field, method, signature, getter, setter };

    fn privateElementName(tree: *const ast.Tree, data: ast.NodeData) ?[]const u8 {
        const key = switch (data) {
            inline .property_definition, .method_definition => |element| element.key,
            else => return null,
        };
        return switch (tree.data(key)) {
            .private_identifier => |id| tree.string(id.name),
            else => null,
        };
    }

    fn privateElementKind(tree: *const ast.Tree, data: ast.NodeData) PrivateElementKind {
        return switch (data) {
            .method_definition => |method| switch (method.kind) {
                .get => .getter,
                .set => .setter,
                else => if (tree.data(method.value).function.body == .null)
                    .signature
                else
                    .method,
            },
            else => .field,
        };
    }

    fn privateElementsPair(a: PrivateElementKind, b: PrivateElementKind) bool {
        return switch (a) {
            .getter => b == .setter,
            .setter => b == .getter,
            .signature => b == .signature or b == .method,
            .method => b == .signature,
            .field => false,
        };
    }

    fn privateElementIsStatic(data: ast.NodeData) bool {
        return switch (data) {
            inline .property_definition, .method_definition => |element| element.static,
            else => false,
        };
    }

    // 15.7.7 AllPrivateIdentifiersValid
    fn checkPrivateName(self: *Self, node: NodeIndex, ctx: *Ctx) Error!void {
        const name = ctx.tree.string(ctx.tree.data(node).private_identifier.name);
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            if (ctx.tree.data(i) != .class_body) continue;
            for (ctx.tree.extra(ctx.tree.data(i).class_body.body)) |child| {
                const declared = privateElementName(ctx.tree, ctx.tree.data(child)) orelse continue;
                if (eql(u8, declared, name)) return;
            }
        }
        const message = "Private field '#{s}' must be declared in an enclosing class";
        try self.report(node, try self.fmt(message, .{name}), .{});
    }

    fn checkStrictReserved(
        self: *Self,
        name: []const u8,
        node: NodeIndex,
        ctx: *Ctx,
        comptime as_what: []const u8,
    ) Error!void {
        if (!isStrict(ctx)) return;
        if (!lexical.mayBeStrictReserved(name)) return;
        const tag = lexical.keyword(name) orelse return;
        if (!tag.isStrictModeReserved()) return;
        const message = "'{s}' is a reserved word in strict mode and cannot be used as " ++ as_what;
        try self.report(node, try self.fmt(message, .{name}), .{});
    }

    // 14.7.5.1 allows one ForBinding and, outside annex B 3.5, no initializer
    fn checkForInOfInitializer(
        self: *Self,
        ctx: *Ctx,
        left: NodeIndex,
        comptime is_for_in: bool,
    ) Error!void {
        if (ctx.tree.data(left) != .variable_declaration) return;
        const decl = ctx.tree.data(left).variable_declaration;
        const declarators = ctx.tree.extra(decl.declarators);

        if (declarators.len > 1) {
            const message = "Only a single variable declaration is allowed in a 'for..." ++
                (if (is_for_in) "in" else "of") ++ "' statement";
            return self.report(left, message, .{});
        }
        if (!isStrict(ctx)) return;
        for (declarators) |child| {
            const declarator = ctx.tree.data(child).variable_declarator;
            if (declarator.init == .null) continue;
            if (static_semantics.isAnnexBForInHead(ctx.tree, decl.kind, declarator.id, is_for_in)) {
                const message =
                    "The variable declaration of a 'for...in' statement cannot have an initializer";
                return self.report(child, message, .{ .help = "Remove the initializer" });
            }
        }
    }

    fn checkAssignTargetEvalArguments(self: *Self, node: NodeIndex, ctx: *Ctx) Error!void {
        if (node == .null) return;
        switch (ctx.tree.data(node)) {
            .identifier_reference => try self.checkEvalArguments(node, node, ctx),
            inline .array_pattern, .object_pattern => |pattern, tag| {
                const nested = if (tag == .array_pattern) pattern.elements else pattern.properties;
                for (ctx.tree.extra(nested)) |child| {
                    try self.checkAssignTargetEvalArguments(child, ctx);
                }
                try self.checkAssignTargetEvalArguments(pattern.rest, ctx);
            },
            .binding_property => |p| try self.checkAssignTargetEvalArguments(p.value, ctx),
            .assignment_pattern => |ap| try self.checkAssignTargetEvalArguments(ap.left, ctx),
            .binding_rest_element => |r| try self.checkAssignTargetEvalArguments(r.argument, ctx),
            else => {},
        }
    }

    fn checkEvalArguments(self: *Self, target: NodeIndex, node: NodeIndex, ctx: *Ctx) Error!void {
        const name = switch (ctx.tree.data(target)) {
            .identifier_reference => |id| ctx.tree.string(id.name),
            else => return,
        };
        if (!lexical.isEvalOrArguments(name)) return;
        try self.report(node, try self.fmt("Cannot assign to '{s}' in strict mode", .{name}), .{});
    }

    const SuperHome = union(enum) {
        function: ?NodeIndex,
        class_element,
        none,
    };

    fn superHome(ctx: *Ctx, above: *traverser.NodePath.AncestorIterator) SuperHome {
        var child: NodeIndex = .null;
        while (above.next()) |i| : (child = i) {
            switch (ctx.tree.data(i)) {
                .function => return .{ .function = above.next() },
                .property_definition => |field| if (field.value == child) return .class_element,
                .static_block => return .class_element,
                else => {},
            }
        }
        return .none;
    }

    fn isSuperPropertyValid(ctx: *Ctx) bool {
        var above = ctx.path.ancestors();
        const parent = switch (superHome(ctx, &above)) {
            .function => |parent| parent orelse return false,
            .class_element => return true,
            .none => return false,
        };
        return switch (ctx.tree.data(parent)) {
            .method_definition => true,
            .object_property => |prop| prop.method or prop.kind != .init,
            else => false,
        };
    }

    fn isAtProgramLevel(ctx: *Ctx) bool {
        const parent = ctx.path.parent() orelse return true;
        return switch (ctx.tree.data(parent)) {
            .program, .ts_module_block => true,
            else => false,
        };
    }

    fn isFunctionBoundary(data: ast.NodeData) bool {
        return switch (data) {
            .function, .arrow_function_expression, .static_block => true,
            else => false,
        };
    }

    fn isInsideJumpTarget(ctx: *Ctx, comptime or_switch: bool) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.data(i);
            if (data.isIteration()) return true;
            if (or_switch and data == .switch_statement) return true;
            if (isFunctionBoundary(data)) return false;
        }
        return false;
    }

    // the body a labelled jump lands in, or `null` once a missing target is reported
    fn jumpTarget(
        self: *Self,
        label_node: NodeIndex,
        node: NodeIndex,
        ctx: *Ctx,
        comptime keyword: []const u8,
    ) Error!?NodeIndex {
        const name = labelName(ctx.tree, label_node);
        var crossed_boundary = false;
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.data(i);
            if (data == .labeled_statement) {
                const statement = data.labeled_statement;
                if (eql(u8, labelName(ctx.tree, statement.label), name)) {
                    if (!crossed_boundary) return statement.body;
                    const message = "A '" ++ keyword ++ "' statement cannot jump to label '{s}'" ++
                        " across a function boundary";
                    try self.report(node, try self.fmt(message, .{name}), .{});
                    return null;
                }
            }
            if (isFunctionBoundary(data)) crossed_boundary = true;
        }
        try self.report(node, try self.fmt("Cannot find label '{s}'", .{name}), .{
            .help = if (comptime eql(u8, keyword, "break"))
                "A 'break' statement can only jump to a label of an enclosing statement"
            else
                continue_label_message,
        });
        return null;
    }

    fn labelName(tree: *const ast.Tree, label_node: NodeIndex) []const u8 {
        return tree.string(tree.data(label_node).label_identifier.name);
    }

    fn enclosingParameters(ctx: *Ctx) ?ast.FormalParameters {
        const tags = [_]ast.NodeTag{ .formal_parameters, .function, .arrow_function_expression };
        return switch (enclosing(ctx, &tags) orelse return null) {
            .formal_parameters => |params| params,
            else => null,
        };
    }

    fn moduleExportName(tree: *const ast.Tree, node: NodeIndex) []const u8 {
        return switch (tree.data(node)) {
            inline .identifier_name, .identifier_reference => |id| tree.string(id.name),
            .string_literal => |lit| tree.string(lit.value),
            else => "",
        };
    }

    // `export import x = y.z` is an alias
    fn exportForm(flags: Symbol.Flags, ctx: *Ctx) ExportForm {
        if (flags.intersects(Symbol.any_import)) return .{ .alias = null };
        const declaration = ctx.tree.data(ctx.path.parent().?);
        const signature = declaration == .function and declaration.function.body == .null;
        return .{ .declaration = .{ .flags = flags, .counted = !flags.interface and !signature } };
    }

    fn defaultExportForm(tree: *const ast.Tree, declaration: NodeIndex) ExportForm {
        return .{ .declaration = switch (tree.data(declaration)) {
            .ts_interface_declaration => .{ .flags = .{ .interface = true }, .counted = false },
            .function => |func| .{ .flags = .{ .function = true }, .counted = func.body != .null },
            .class => .{ .flags = .{ .class = true }, .counted = true },
            else => return .default_expression,
        } };
    }

    fn checkDuplicateWithAttributes(self: *Self, attributes: ast.IndexRange) Error!void {
        const tree = self.tree;
        const items = tree.extra(attributes);
        for (items, 0..) |attribute, i| {
            const key_node = tree.data(attribute).import_attribute.key;
            const key = static_semantics.propName(tree, key_node) orelse continue;
            for (items[0..i]) |prev_attribute| {
                const prev_node = tree.data(prev_attribute).import_attribute.key;
                const prev_key = static_semantics.propName(tree, prev_node) orelse continue;
                if (!key.eql(prev_key.name)) continue;
                const name = if (!prev_key.is_string_literal) prev_key.name else key.name;
                const message = try self.fmt("Duplicate import attribute '{s}'", .{name});
                try self.report(key_node, message, .{
                    .labels = try self.labels(&.{self.label(prev_node, "first used here")}),
                });
                break;
            }
        }
    }

    fn recordExportedName(
        self: *Self,
        name: []const u8,
        node: NodeIndex,
        ctx: *Ctx,
        form: ExportForm,
    ) Error!void {
        if (!ctx.tree.isModule()) return;
        // exports inside a ts namespace are namespace-scoped
        if (ctx.inTsNamespace()) return;
        const gop = try self.exported_names.getOrPut(self.allocator, name);
        if (!gop.found_existing) gop.value_ptr.* = .{ .first = node, .last = node };
        const exported = gop.value_ptr;

        // https://tc39.es/ecma262/#sec-module-semantics-static-semantics-early-errors
        if (!ctx.tree.isTs()) {
            if (gop.found_existing) try self.reportDuplicateExport(name, exported.first, node);
            return;
        }

        exported.last = node;
        switch (form) {
            .alias => |local| {
                exported.conflict = exported.conflict or exported.alias;
                exported.alias = true;
                exported.local = local orelse exported.local;
                exported.counted += 1;
            },
            .default_expression => {
                exported.conflict = exported.conflict or gop.found_existing;
                exported.counted += 1;
            },
            .declaration => |declaration| {
                exported.flags = exported.flags.merge(declaration.flags);
                exported.counted += @intFromBool(declaration.counted);
            },
        }
    }

    fn recordLocalKinds(
        self: *Self,
        name: []const u8,
        flags: Symbol.Flags,
        ctx: *Ctx,
    ) Error!void {
        if (!ctx.tree.isTs()) return;
        if (ctx.scope.get(ctx.symbols.pending.scope).kind != .module) return;
        // an import aliases kinds one file cannot know
        if (flags.intersects(Symbol.any_import)) return;
        const gop = try self.local_kinds.getOrPut(self.allocator, name);
        gop.value_ptr.* = if (gop.found_existing) gop.value_ptr.merge(flags) else flags;
    }

    pub fn checkExports(self: *Self, symbols: *const binder.SymbolTracker) Error!void {
        if (!self.tree.isModule()) return;
        for (self.export_specifiers.items) |specifier| {
            if (symbols.binding(scope.module_scope, specifier.local_name) != null) continue;
            const message = "Cannot export '{s}' because it is not declared in this module";
            try self.report(specifier.node, try self.fmt(message, .{specifier.local_name}), .{});
        }
        if (!self.tree.isTs()) return;
        for (self.exported_names.keys(), self.exported_names.values()) |name, *exported| {
            if (self.tsExportsConflict(exported)) {
                try self.reportDuplicateExport(name, exported.first, exported.last);
            }
        }
    }

    fn tsExportsConflict(self: *const Self, exported: *const ExportedName) bool {
        if (exported.conflict) return true;
        // a namespace or an enum merges with the rest, a type alias with one more
        const merges = exported.flags.intersects(Symbol.namespace_space);
        const allowed: u32 = if (exported.flags.type_alias) 2 else 1;
        if (!merges and exported.counted > allowed) return true;
        // a local `export { x }` cannot share a kind with an exported declaration
        const local = self.local_kinds.get(exported.local orelse return false) orelse return false;
        inline for (.{ Symbol.value_space, Symbol.type_space, Symbol.namespace_space }) |space| {
            if (exported.flags.intersects(space) and local.intersects(space)) return true;
        }
        return false;
    }

    fn reportDuplicateExport(
        self: *Self,
        name: []const u8,
        first: NodeIndex,
        again: NodeIndex,
    ) Error!void {
        std.debug.assert(self.tree.span(first).start < self.tree.span(again).start);
        try self.report(again, try self.fmt("Duplicate export '{s}'", .{name}), .{
            .labels = try self.labels(&.{
                self.label(first, "first exported here"),
                self.label(again, "exported again here"),
            }),
        });
    }

    fn reportRedeclaration(
        self: *Self,
        name: []const u8,
        node: NodeIndex,
        existing_id: binder.SymbolId,
        ctx: *Ctx,
    ) Error!void {
        const flags = ctx.symbols.symbol(existing_id).flags;
        const kind = flags.toString();
        const article: []const u8 = switch (kind[0]) {
            'a', 'e', 'i', 'o', 'u' => "an",
            else => "a",
        };
        const message = if (flags.block_scoped_var)
            try self.fmt("Cannot redeclare block-scoped variable '{s}'", .{name})
        else
            try self.fmt("Duplicate identifier '{s}'", .{name});
        const first = try self.fmt(
            "'{s}' was first declared as {s} {s} here",
            .{ name, article, kind },
        );
        try self.report(node, message, .{
            .labels = try self.labels(&.{
                self.label(ctx.symbols.firstDeclOf(existing_id), first),
                self.label(node, "cannot be redeclared here"),
            }),
            .help = try self.fmt("Rename or remove this declaration of '{s}'", .{name}),
        });
    }

    const Details = struct {
        help: ?[]const u8 = null,
        labels: []const ast.Label = &.{},
    };

    fn report(self: *Self, node: NodeIndex, message: []const u8, details: Details) Error!void {
        try self.tree.addDiagnostic(.{
            .message = message,
            .span = self.tree.span(node),
            .help = details.help,
            .labels = details.labels,
        });
    }

    fn label(self: *const Self, node: NodeIndex, message: []const u8) ast.Label {
        return .{ .span = self.tree.span(node), .message = message };
    }

    fn labels(self: *Self, items: []const ast.Label) Error![]const ast.Label {
        return self.allocator.dupe(ast.Label, items);
    }

    fn fmt(self: *Self, comptime format: []const u8, args: anytype) Error![]u8 {
        return self.allocator.print(format, args);
    }
};

inline fn isStrict(ctx: *const Ctx) bool {
    return ctx.scope.get(ctx.scope.current).flags.strict;
}

fn enclosing(ctx: *const Ctx, comptime tags: []const ast.NodeTag) ?ast.NodeData {
    const set = comptime std.EnumSet(ast.NodeTag).initMany(tags);
    var iter = ctx.path.ancestors();
    while (iter.next()) |i| {
        const data = ctx.tree.data(i);
        if (set.contains(data)) return data;
    }
    return null;
}
