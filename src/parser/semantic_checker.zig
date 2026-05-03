const std = @import("std");
const util = @import("util");
const traverser = @import("traverser/root.zig");
const ast = @import("ast.zig");
const ecmascript = @import("ecmascript.zig");

const Allocator = std.mem.Allocator;

const semantic = traverser.semantic;
const Symbol = semantic.Symbol;

const Action = traverser.Action;
const SemanticCtx = semantic.Ctx;

const eql = std.mem.eql;

pub const AnalysisError = Allocator.Error;

/// Runs semantic analysis on a tree.
///
/// Appends diagnostics directly to the tree alongside parse errors.
/// All allocations use the tree's arena, so the returned scope tree
/// and symbol table are valid as long as the tree is alive.
pub fn analyze(tree: *ast.Tree) AnalysisError!semantic.Result {
    var visitor = SemanticVisit{
        .tree = tree,
        .allocator = tree.allocator(),
    };

    const result = try semantic.traverse(SemanticVisit, tree, &visitor);

    // post traversal
    try visitor.checkUnresolvedExports(result);

    return result;
}

const SemanticVisit = struct {
    const Self = @This();

    tree: *ast.Tree,
    allocator: Allocator,

    exported_names: std.StringHashMapUnmanaged(ast.NodeIndex) = .{},
    export_specifiers: std.ArrayList(ExportSpecifierInfo) = .empty,

    const ExportSpecifierInfo = struct {
        local_name: []const u8,
        node: ast.NodeIndex,
    };

    pub fn enter_binding_identifier(self: *Self, id: ast.BindingIdentifier, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const name = ctx.tree.string(id.name);
        const flags = ctx.symbols.currentBindingFlags();

        // type-position binding identifiers are parameter labels, not
        // real bindings. only type parameters are real here.
        if (ctx.inTypePosition() and !flags.type_parameter) return .proceed;

        // https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
        if (ctx.scope.isStrict() and !flags.ambient) {
            try self.checkStrictReserved(name, node_index, ctx, "a binding identifier");
            if (isEvalOrArguments(name))
                try self.report(ctx.tree.span(node_index), try self.fmt("'{s}' is not allowed as a binding identifier in strict mode", .{name}), .{});
        } else if (flags.block_scoped_var and eql(u8, name, "let")) {
            try self.report(ctx.tree.span(node_index), "'let' is not allowed as a variable name in a lexical declaration", .{});
        }

        const existing = ctx.symbols.findInScopeOrHoisted(ctx.symbols.currentTarget(), name);
        try self.checkRedeclaration(id, node_index, name, flags, ctx, existing);

        // ts loosens duplicate-export for declaration merges (interface
        // +class, namespace+value, function overloads, var redeclares).
        // skip in ts when the name is already in scope.
        if (ctx.symbols.export_state == .named) {
            const skip = existing != null and ctx.tree.isTs();
            if (!skip) try self.recordExportedName(name, node_index, ctx);
        }

        return .proceed;
    }

    fn checkRedeclaration(
        self: *Self,
        id: ast.BindingIdentifier,
        node_index: ast.NodeIndex,
        name: []const u8,
        flags: Symbol.Flags,
        ctx: *SemanticCtx,
        existing_id: ?semantic.SymbolId,
    ) AnalysisError!void {
        const target = ctx.symbols.currentTarget();
        const excludes = ctx.symbols.currentBindingExcludes();

        if (existing_id) |sym| {
            const existing = ctx.symbols.getSymbol(sym);
            const merging_with_ambient = flags.ambient or existing.flags.ambient;

            if (!merging_with_ambient and existing.flags.intersects(excludes)) {
                try self.reportRedeclaration(id, node_index, existing, ctx);
                return;
            }

            // https://tc39.es/ecma262/#sec-parameter-lists-static-semantics-early-errors
            // duplicate params are allowed in sloppy simple-param-list
            // functions. re-add the conflict for strict, unique, arrow,
            // and non-simple cases.
            if (existing.flags.parameter and flags.parameter) {
                if (findFormalParameters(ctx)) |params| {
                    const must_be_unique = ctx.scope.isStrict() or
                        params.kind == .unique_formal_parameters or
                        params.kind == .arrow_formal_parameters or
                        ecmascript.findNonSimpleParameter(ctx.tree, params) != null;
                    if (must_be_unique)
                        try self.reportRedeclaration(id, node_index, existing, ctx);
                }
            }
        }

        // 14.2.1: hoisting var conflicts with block-scoped names in
        // any intermediate block it passes through.
        if (flags.isHoistingVar()) {
            var iter = ctx.scope.ancestors(ctx.scope.currentScopeId());
            while (iter.next()) |scope_id| {
                if (scope_id == target) break;
                if (ctx.symbols.findInScope(scope_id, name)) |sym| {
                    const existing = ctx.symbols.getSymbol(sym);
                    if (existing.flags.intersects(Symbol.BLOCK_SCOPED_LIKE)) {
                        try self.reportRedeclaration(id, node_index, existing, ctx);
                        break;
                    }
                }
            }
        }
    }

    /// https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
    pub fn enter_identifier_reference(self: *Self, id: ast.IdentifierReference, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        // type-position identifiers are ts type references, not js
        // identifier references. js early-error rules don't apply.
        if (ctx.inTypePosition()) return .proceed;

        const name = ctx.tree.string(id.name);
        try self.checkStrictReserved(name, node_index, ctx, "an identifier");

        // https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
        if (eql(u8, name, "arguments") and !isArgumentsAvailable(ctx))
            try self.report(ctx.tree.span(node_index), "'arguments' is not allowed in class field initializers or static blocks", .{});

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
    pub fn enter_label_identifier(self: *Self, id: ast.LabelIdentifier, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const name = ctx.tree.string(id.name);
        try self.checkStrictReserved(name, node_index, ctx, "a label");
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-string-literals-static-semantics-early-errors
    pub fn enter_string_literal(self: *Self, _: ast.StringLiteral, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        // type-position strings are type-level, no strict escape rules
        if (ctx.inTypePosition() or !ctx.scope.isStrict()) return .proceed;

        const span = ctx.tree.span(node_index);
        if (util.Utf.hasOctalEscape(ctx.tree.source[span.start..span.end]))
            try self.report(span, "Octal escape sequences are not allowed in strict mode", .{
                .help = "Use \\xHH (hex) or \\uHHHH (unicode) escape instead",
            });

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-additional-syntax-numeric-literals
    pub fn enter_numeric_literal(self: *Self, _: ast.NumericLiteral, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.inTypePosition() or !ctx.scope.isStrict()) return .proceed;

        const span = ctx.tree.span(node_index);
        const raw = ctx.tree.source[span.start..span.end];

        if (!isLegacyNumericLiteral(raw)) return .proceed;

        const is_octal = for (raw) |c| {
            if (c == '8' or c == '9') break false;
        } else true;

        try self.report(ctx.tree.span(node_index), if (is_octal)
            "Octal literals are not allowed in strict mode"
        else
            "Decimals with leading zeros are not allowed in strict mode", .{
            .help = if (is_octal)
                "Use the 0o prefix for octal literals (e.g., 0o77), or a decimal equivalent"
            else
                "Remove the leading zero, or use 0o for octal, 0x for hex, or 0b for binary notation",
        });
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-function-definitions-static-semantics-early-errors
    /// "It is a Syntax Error if FunctionBodyContainsUseStrict is true and
    ///  IsSimpleParameterList of FormalParameters is false."
    pub fn enter_directive(self: *Self, directive: ast.Directive, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (eql(u8, ctx.tree.string(directive.value), "use strict")) {
            var iter = ctx.path.ancestors();
            while (iter.next()) |i| {
                switch (ctx.tree.data(i)) {
                    .function => |func| {
                        if (func.params != .null)
                            if (ecmascript.findNonSimpleParameter(ctx.tree, ctx.tree.data(func.params).formal_parameters)) |param| {
                                try self.report(ctx.tree.span(node_index), "Illegal 'use strict' directive in function with non-simple parameter list", .{
                                    .labels = try self.labels(&.{
                                        self.label(ctx.tree.span(param), "non-simple parameter"),
                                    }),
                                });
                            };
                        break;
                    },
                    .arrow_function_expression => |arrow| {
                        if (ecmascript.findNonSimpleParameter(ctx.tree, ctx.tree.data(arrow.params).formal_parameters)) |param| {
                            try self.report(ctx.tree.span(node_index), "Illegal 'use strict' directive in function with non-simple parameter list", .{
                                .labels = try self.labels(&.{
                                    self.label(ctx.tree.span(param), "non-simple parameter"),
                                }),
                            });
                        }
                        break;
                    },
                    .program => break,
                    else => {},
                }
            }
        }

        return .proceed;
    }

    // https://tc39.es/ecma262/#sec-labelled-statements-static-semantics-early-errors (14.13.1)
    //   LabelledItem : FunctionDeclaration - syntax error unless non-strict + web host (B.3.1).
    // https://tc39.es/ecma262/#sec-if-statement-static-semantics-early-errors (14.6.1)
    //   IsLabelledFunction check prevents labelled functions in if/iteration/with bodies.
    // https://tc39.es/ecma262/#sec-functiondeclarations-in-ifstatement-statement-clauses (B.3.3)
    //   Bare FunctionDeclaration in if/else is allowed in non-strict code only.
    pub fn enter_function(self: *Self, func: ast.Function, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (func.type != .function_declaration) return .proceed;

        const is_strict = ctx.scope.isStrict();
        var through_labels = false;
        var iter = ctx.path.ancestors();
        _ = iter.next(); // skip the function itself

        const parent = while (iter.next()) |idx| {
            const d = ctx.tree.data(idx);
            if (d != .labeled_statement) break d;
            through_labels = true;
        } else return .proceed;

        const is_valid = switch (parent) {
            .program, .function_body, .class_body, .static_block,
            .block_statement, .switch_case,
            .export_named_declaration, .export_default_declaration,
            // ts namespace bodies host function declarations
            .ts_module_block,
            => !is_strict or !through_labels,
            .if_statement => !is_strict and !through_labels,
            else => false,
        };

        if (!is_valid) {
            try self.report(ctx.tree.span(node_index), if (is_strict)
                "In strict mode code, functions can only be declared at top level or inside a block"
            else
                "In non-strict mode code, functions can only be declared at top level, inside a block, or as the body of an if statement", .{});
        }
        return .proceed;
    }

    pub fn enter_yield_expression(self: *Self, _: ast.YieldExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (isInFormalParameters(ctx))
            try self.report(ctx.tree.span(node_index), "Yield expression is not allowed in formal parameters", .{});
        return .proceed;
    }

    pub fn enter_await_expression(self: *Self, _: ast.AwaitExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (isInFormalParameters(ctx))
            try self.report(ctx.tree.span(node_index), "Await expression is not allowed in formal parameters", .{});

        // ClassStaticBlockBody uses [~Await]
        if (isInsideStaticBlock(ctx))
            try self.report(ctx.tree.span(node_index), "Cannot use await in class static initialization block", .{});

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-update-expressions-static-semantics-early-errors
    pub fn enter_update_expression(self: *Self, expr: ast.UpdateExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.scope.isStrict() and isEvalOrArgumentsRef(ctx.tree, expr.argument))
            try self.report(ctx.tree.span(node_index), "Cannot assign to 'eval' or 'arguments' in strict mode", .{});
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-assignment-operators-static-semantics-early-errors
    pub fn enter_assignment_expression(self: *Self, expr: ast.AssignmentExpression, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.scope.isStrict())
            try self.checkAssignTargetEvalArguments(expr.left, ctx);
        return .proceed;
    }

    /// 15.7.7 Static Semantics: AllPrivateIdentifiersValid
    /// https://tc39.es/ecma262/#sec-static-semantics-allprivateidentifiersvalid
    ///
    /// class C { #a; f() { #b in {} } }
    ///                     ^^ undeclared
    pub fn enter_binary_expression(self: *Self, expr: ast.BinaryExpression, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (expr.operator == .in and ctx.tree.data(expr.left) == .private_identifier) {
            const name = ctx.tree.string(ctx.tree.data(expr.left).private_identifier.name);
            if (!isPrivateNameDeclared(ctx, name))
                try self.report(ctx.tree.span(expr.left), try self.fmt("Private field '#{s}' must be declared in an enclosing class", .{name}), .{});
        }
        return .proceed;
    }

    pub fn enter_unary_expression(self: *Self, expr: ast.UnaryExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (expr.operator == .delete) {
            const target = unwrapParens(ctx.tree, expr.argument);
            switch (ctx.tree.data(target)) {
                // https://tc39.es/ecma262/#sec-delete-operator-static-semantics-early-errors
                .identifier_reference => if (ctx.scope.isStrict()) {
                    try self.report(ctx.tree.span(node_index), "Deleting a variable in strict mode is not allowed", .{});
                },
                .member_expression => |m| if (!m.computed and ctx.tree.data(m.property) == .private_identifier) {
                    try self.report(ctx.tree.span(node_index), "Private fields cannot be deleted", .{});
                },
                else => {},
            }
        }
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-__proto__-property-names-in-object-initializers
    pub fn enter_object_expression(self: *Self, obj: ast.ObjectExpression, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        var first_proto: ?ast.NodeIndex = null;
        for (ctx.tree.extra(obj.properties)) |child| {
            if (ctx.tree.data(child) != .object_property) continue;
            const prop = ctx.tree.data(child).object_property;
            if (prop.computed or prop.method or prop.shorthand or prop.kind != .init) continue;
            const pn = ecmascript.propName(ctx.tree, prop.key) orelse continue;
            if (!pn.eql("__proto__")) continue;

            if (first_proto) |first| {
                try self.report(ctx.tree.span(child), "Duplicate '__proto__' property in object literal", .{
                    .labels = try self.labels(&.{
                        self.label(ctx.tree.span(first), "first defined here"),
                    }),
                });
            } else {
                first_proto = child;
            }
        }
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn enter_call_expression(self: *Self, expr: ast.CallExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.tree.data(expr.callee) == .super) {
            switch (superCallValidity(ctx)) {
                .valid => {},
                .not_in_constructor => try self.report(ctx.tree.span(node_index), "'super()' is only valid in a constructor of a derived class", .{
                    .help = "Use an arrow function instead of a regular function to inherit the 'super' binding",
                }),
                .no_extends => try self.report(ctx.tree.span(node_index), "'super()' is only valid in a constructor of a derived class", .{
                    .help = "Add an 'extends' clause to the class or remove the 'super()' call",
                }),
            }
        }
        return .proceed;
    }

    pub fn enter_member_expression(self: *Self, expr: ast.MemberExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.tree.data(expr.object) == .super) {
            if (!isSuperPropertyValid(ctx))
                try self.report(ctx.tree.span(node_index), "'super' property access is only valid inside a method or class body", .{
                    .help = "Use an arrow function instead of a regular function to inherit the 'super' binding",
                });
        }

        // 13.3.7 SuperProperty only allows `super . IdentifierName`
        // and `super [ Expression ]`, not `super . PrivateIdentifier`.
        // https://tc39.es/ecma262/#sec-super-keyword
        //
        // 15.7.7 AllPrivateIdentifiersValid (for undeclared names):
        //   MemberExpression : MemberExpression . PrivateIdentifier
        //   "If names contains the StringValue of PrivateIdentifier, [recurse].
        //    Return false."
        // https://tc39.es/ecma262/#sec-static-semantics-allprivateidentifiersvalid
        if (!expr.computed and ctx.tree.data(expr.property) == .private_identifier) {
            const name = ctx.tree.string(ctx.tree.data(expr.property).private_identifier.name);
            if (ctx.tree.data(expr.object) == .super) {
                try self.report(ctx.tree.span(node_index), "'super' keyword unexpected here", .{});
            } else if (!isPrivateNameDeclared(ctx, name)) {
                try self.report(ctx.tree.span(expr.property), try self.fmt("Private field '#{s}' must be declared in an enclosing class", .{name}), .{});
            }
        }

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-left-hand-side-expressions-static-semantics-early-errors
    pub fn enter_meta_property(self: *Self, prop: ast.MetaProperty, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const meta = ctx.tree.data(prop.meta);
        if (meta != .identifier_name) return .proceed;
        const name = ctx.tree.string(meta.identifier_name.name);

        if (eql(u8, name, "import") and !ctx.tree.isModule())
            try self.report(ctx.tree.span(node_index), "'import.meta' is only valid in module code", .{});

        // https://tc39.es/ecma262/#sec-static-semantics-early-errors
        if (eql(u8, name, "new") and !isNewTargetAvailable(ctx))
            try self.report(ctx.tree.span(node_index), "'new.target' is only valid inside functions, class field initializers, or static blocks", .{});

        return .proceed;
    }

    pub fn enter_import_declaration(self: *Self, decl: ast.ImportDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkImportExportPosition(node_index, "import statement", "'import' declaration", ctx);
        try self.checkDuplicateWithClaudeAttributes(decl.attributes, ctx);
        return .proceed;
    }

    pub fn enter_export_named_declaration(self: *Self, decl: ast.ExportNamedDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkImportExportPosition(node_index, "'export' declaration", "'export' declaration", ctx);
        try self.checkDuplicateWithClaudeAttributes(decl.attributes, ctx);
        return .proceed;
    }

    pub fn enter_export_default_declaration(self: *Self, _: ast.ExportDefaultDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkImportExportPosition(node_index, "'export default' declaration", "'export default' declaration", ctx);
        try self.recordExportedName("default", node_index, ctx);
        return .proceed;
    }

    pub fn enter_export_all_declaration(self: *Self, decl: ast.ExportAllDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkImportExportPosition(node_index, "'export *' declaration", "'export *' declaration", ctx);
        if (decl.exported != .null)
            try self.recordExportedName(getModuleExportName(ctx.tree, decl.exported), node_index, ctx);
        try self.checkDuplicateWithClaudeAttributes(decl.attributes, ctx);
        return .proceed;
    }

    fn checkImportExportPosition(
        self: *Self,
        node_index: ast.NodeIndex,
        comptime out_of_module_label: []const u8,
        comptime top_level_label: []const u8,
        ctx: *SemanticCtx,
    ) AnalysisError!void {
        const parent = ctx.path.parent() orelse return;
        const span = ctx.tree.span(node_index);
        switch (ctx.tree.data(parent)) {
            .program => if (!ctx.tree.isModule())
                try self.report(span, "Cannot use " ++ out_of_module_label ++ " outside a module", .{}),
            .ts_module_block => {},
            else => try self.report(span, top_level_label ++ " may only appear at the top level", .{}),
        }
    }

    /// https://tc39.es/ecma262/#sec-exports-static-semantics-early-errors
    pub fn enter_export_specifier(self: *Self, spec: ast.ExportSpecifier, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const exported_name = getModuleExportName(ctx.tree, spec.exported);
        try self.recordExportedName(exported_name, node_index, ctx);

        // collect for unresolved check (local exports only). skip
        // type-only exports and exports inside ts namespaces (the
        // checker tracks the value scope, not type or namespace).
        if (spec.export_kind == .type) return .proceed;
        if (ctx.inTsNamespace()) return .proceed;
        if (ctx.path.parent()) |parent| {
            const parent_data = ctx.tree.data(parent);
            if (parent_data == .export_named_declaration and
                parent_data.export_named_declaration.source == .null and
                parent_data.export_named_declaration.export_kind != .type)
            {
                try self.export_specifiers.append(self.allocator, .{
                    .local_name = getModuleExportName(ctx.tree, spec.local),
                    .node = node_index,
                });
            }
        }
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-with-statement-static-semantics-early-errors
    pub fn enter_with_statement(self: *Self, _: ast.WithStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.scope.isStrict())
            try self.report(ctx.tree.span(node_index), "'with' statements are not allowed in strict mode", .{});
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements-static-semantics-early-errors
    pub fn enter_for_of_statement(self: *Self, stmt: ast.ForOfStatement, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkForInOfInitializer(ctx, stmt.left);
        if (ctx.scope.isStrict())
            try self.checkAssignTargetEvalArguments(stmt.left, ctx);
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements-static-semantics-early-errors
    pub fn enter_for_in_statement(self: *Self, stmt: ast.ForInStatement, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkForInOfInitializer(ctx, stmt.left);
        if (ctx.scope.isStrict())
            try self.checkAssignTargetEvalArguments(stmt.left, ctx);
        return .proceed;
    }

    pub fn enter_variable_declaration(self: *Self, decl: ast.VariableDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if ((decl.kind == .using or decl.kind == .await_using) and
            !ctx.tree.isModule() and isAtProgramLevel(ctx))
        {
            try self.report(ctx.tree.span(node_index), try self.fmt(
                "'{s}' is not allowed at the top level of a script",
                .{decl.kind.toString()},
            ), .{});
        }
        return .proceed;
    }

    pub fn enter_break_statement(self: *Self, stmt: ast.BreakStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (stmt.label != .null) {
            const label_name = ctx.tree.string(ctx.tree.data(stmt.label).label_identifier.name);
            switch (findLabel(ctx, label_name)) {
                .found => {},
                .not_found => try self.report(ctx.tree.span(node_index), try self.fmt("Use of undefined label '{s}'", .{label_name}), .{}),
                .crossed_boundary => try self.report(ctx.tree.span(node_index), try self.fmt("Cannot break to label '{s}' across function boundaries", .{label_name}), .{}),
            }
        } else {
            if (!isInsideBreakable(ctx))
                try self.report(ctx.tree.span(node_index), "Illegal break statement", .{
                    .help = "A 'break' statement can only be used within an enclosing iteration or switch statement",
                });
        }
        return .proceed;
    }

    pub fn enter_continue_statement(self: *Self, stmt: ast.ContinueStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (stmt.label != .null) {
            const label_name = ctx.tree.string(ctx.tree.data(stmt.label).label_identifier.name);
            switch (findLabelForContinue(ctx, label_name)) {
                .found => {},
                .not_found => try self.report(ctx.tree.span(node_index), try self.fmt("Use of undefined label '{s}'", .{label_name}), .{}),
                .crossed_boundary => try self.report(ctx.tree.span(node_index), try self.fmt("Cannot continue to label '{s}' across function boundaries", .{label_name}), .{}),
                .not_iteration => try self.report(ctx.tree.span(node_index), try self.fmt("Label '{s}' does not denote an iteration statement", .{label_name}), .{
                    .help = "A 'continue' statement can only jump to a label of an enclosing 'for', 'while', 'do-while', 'for-in', or 'for-of' statement",
                }),
            }
        } else {
            if (!isInsideIteration(ctx))
                try self.report(ctx.tree.span(node_index), "Illegal continue statement", .{
                    .help = "A 'continue' statement can only be used within an enclosing iteration statement",
                });
        }
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-labelled-statements-static-semantics-early-errors
    pub fn enter_labeled_statement(self: *Self, stmt: ast.LabeledStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        _ = node_index;
        const name = ctx.tree.string(ctx.tree.data(stmt.label).label_identifier.name);
        var iter = ctx.path.ancestors();
        _ = iter.next(); // skip current node
        while (iter.next()) |i| {
            const data = ctx.tree.data(i);
            if (data == .labeled_statement) {
                if (eql(u8, ctx.tree.string(ctx.tree.data(data.labeled_statement.label).label_identifier.name), name))
                    try self.report(ctx.tree.span(stmt.label), try self.fmt("Duplicate label '{s}'", .{name}), .{});
            }
            if (isFunctionBoundary(data)) break;
        }
        return .proceed;
    }

    /// Section 14.12: the CaseBlock grammar permits at most one DefaultClause.
    pub fn enter_switch_statement(self: *Self, stmt: ast.SwitchStatement, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        var first_default: ?ast.NodeIndex = null;
        for (ctx.tree.extra(stmt.cases)) |child| {
            const case = ctx.tree.data(child).switch_case;
            if (case.@"test" != .null) continue;

            if (first_default) |first| {
                try self.report(ctx.tree.span(child), "A switch statement can only have one default clause", .{
                    .labels = try self.labels(&.{
                        self.label(ctx.tree.span(first), "first default defined here"),
                    }),
                });
            } else {
                first_default = child;
            }
        }
        return .proceed;
    }

    /// 15.7.1 Static Semantics: Early Errors
    /// https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    ///
    /// ClassBody : ClassElementList
    ///   - "It is a Syntax Error if PrototypePropertyNameList of ClassElementList
    ///      contains more than one occurrence of "constructor"."
    ///   - "It is a Syntax Error if PrivateBoundIdentifiers of ClassElementList contains
    ///      any duplicate entries, unless the name is used once for a getter and once for
    ///      a setter and in no other entries, and the getter and setter are either both
    ///      static or both non-static."
    pub fn enter_class_body(self: *Self, body: ast.ClassBody, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const children = ctx.tree.extra(body.body);
        var first_constructor: ?ast.NodeIndex = null;

        for (children, 0..) |child, j| {
            const child_data = ctx.tree.data(child);

            // a body-less method is a typescript overload signature,
            // abstract member, or ambient method. it has no
            // implementation, so it does not count toward the
            // duplicate-constructor check.
            if (child_data == .method_definition) {
                const md = child_data.method_definition;
                if (md.kind == .constructor and ctx.tree.data(md.value).function.body != .null) {
                    if (first_constructor) |first| {
                        try self.report(ctx.tree.span(md.key), "A class can only have one constructor", .{
                            .labels = try self.labels(&.{
                                self.label(ctx.tree.span(first), "first constructor defined here"),
                            }),
                        });
                    } else {
                        first_constructor = md.key;
                    }
                }
            }

            // check PrivateBoundIdentifiers for duplicates:
            //   class C { #x; #x; }             (duplicate field)
            //   class C { #m; #m() {} }         (field + method conflict)
            //   class C { get #x(){} }
            //           { static set #x(_){} }  (static/non-static mismatch)
            const name_j = privateElementName(ctx.tree, child_data) orelse continue;
            const kind_j = privateElementKind(child_data);

            for (children[0..j]) |prev| {
                const prev_data = ctx.tree.data(prev);
                const name_i = privateElementName(ctx.tree, prev_data) orelse continue;
                if (!eql(u8, name_i, name_j)) continue;

                const kind_i = privateElementKind(prev_data);
                if (((kind_i == .getter and kind_j == .setter) or
                    (kind_i == .setter and kind_j == .getter)) and
                    privateElementIsStatic(prev_data) == privateElementIsStatic(child_data)) continue;

                try self.report(ctx.tree.span(child), try self.fmt("Duplicate private name '#{s}'", .{name_j}), .{
                    .labels = try self.labels(&.{
                        self.label(ctx.tree.span(prev), "first declared here"),
                    }),
                });
                break;
            }
        }

        return .proceed;
    }

    const PrivateElementKind = enum { field, method, getter, setter };

    /// returns the StringValue of a class element's PrivateIdentifier key, if any.
    fn privateElementName(tree: *const ast.Tree, data: ast.NodeData) ?[]const u8 {
        const key = switch (data) {
            .property_definition => |pd| pd.key,
            .method_definition => |md| md.key,
            else => return null,
        };
        if (tree.data(key) == .private_identifier)
            return tree.string(tree.data(key).private_identifier.name);
        return null;
    }

    fn privateElementKind(data: ast.NodeData) PrivateElementKind {
        return switch (data) {
            .method_definition => |md| switch (md.kind) {
                .get => .getter,
                .set => .setter,
                else => .method,
            },
            else => .field,
        };
    }

    fn privateElementIsStatic(data: ast.NodeData) bool {
        return switch (data) {
            .property_definition => |pd| pd.static,
            .method_definition => |md| md.static,
            else => false,
        };
    }

    /// 15.7.7 AllPrivateIdentifiersValid: walks ancestor ClassBody nodes,
    /// collecting PrivateBoundIdentifiers at each level.
    fn isPrivateNameDeclared(ctx: *SemanticCtx, name: []const u8) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            if (ctx.tree.data(i) != .class_body) continue;
            for (ctx.tree.extra(ctx.tree.data(i).class_body.body)) |child| {
                if (privateElementName(ctx.tree, ctx.tree.data(child))) |pname| {
                    if (eql(u8, pname, name)) return true;
                }
            }
        }
        return false;
    }

    fn checkStrictReserved(self: *Self, name: []const u8, node_index: ast.NodeIndex, ctx: *SemanticCtx, comptime as_what: []const u8) AnalysisError!void {
        if (!ctx.scope.isStrict()) return;
        if (matchStrictReserved(name)) |word|
            try self.report(ctx.tree.span(node_index), try self.fmt("'{s}' is reserved in strict mode and cannot be used as " ++ as_what, .{word}), .{});
    }

    /// https://tc39.es/ecma262/#sec-keywords-and-reserved-words
    fn matchStrictReserved(name: []const u8) ?[]const u8 {
        return switch (name.len) {
            3 => check(name, "let"),
            5 => check(name, "yield"),
            6 => check(name, "static") orelse check(name, "public"),
            7 => check(name, "private") orelse check(name, "package"),
            9 => check(name, "protected") orelse check(name, "interface"),
            10 => check(name, "implements"),
            else => null,
        };
    }

    fn check(name: []const u8, keyword: []const u8) ?[]const u8 {
        return if (eql(u8, name, keyword)) keyword else null;
    }

    /// 14.7.5.1 ForBinding must contain exactly one BoundName, and (outside
    /// Annex B 3.5) may not have an Initializer.
    fn checkForInOfInitializer(self: *Self, ctx: *SemanticCtx, left: ast.NodeIndex) AnalysisError!void {
        if (ctx.tree.data(left) != .variable_declaration) return;
        const decl = ctx.tree.data(left).variable_declaration;
        const declarators = ctx.tree.extra(decl.declarators);

        if (declarators.len > 1) {
            try self.report(ctx.tree.span(left), "Only a single variable declaration is allowed in a for-in/of statement", .{});
            return;
        }

        for (declarators) |child| {
            const declarator = ctx.tree.data(child).variable_declarator;
            if (declarator.init != .null) {
                try self.report(ctx.tree.span(child), "for-in/of loop variable declaration may not have an initializer", .{});
                return;
            }
        }
    }

    fn isEvalOrArguments(name: []const u8) bool {
        return eql(u8, name, "eval") or eql(u8, name, "arguments");
    }

    fn isEvalOrArgumentsRef(tree: *const ast.Tree, node: ast.NodeIndex) bool {
        return tree.data(node) == .identifier_reference and
            isEvalOrArguments(tree.string(tree.data(node).identifier_reference.name));
    }

    /// walks a destructuring assignment target to find eval/arguments references.
    fn checkAssignTargetEvalArguments(self: *Self, node: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!void {
        if (node == .null) return;
        switch (ctx.tree.data(node)) {
            .identifier_reference => |id| {
                if (isEvalOrArguments(ctx.tree.string(id.name)))
                    try self.report(ctx.tree.span(node), "Cannot assign to 'eval' or 'arguments' in strict mode", .{});
            },
            .array_pattern => |arr| {
                for (ctx.tree.extra(arr.elements)) |elem| {
                    try self.checkAssignTargetEvalArguments(elem, ctx);
                }
                try self.checkAssignTargetEvalArguments(arr.rest, ctx);
            },
            .object_pattern => |obj| {
                for (ctx.tree.extra(obj.properties)) |prop| {
                    if (ctx.tree.data(prop) == .binding_property)
                        try self.checkAssignTargetEvalArguments(ctx.tree.data(prop).binding_property.value, ctx);
                }
                try self.checkAssignTargetEvalArguments(obj.rest, ctx);
            },
            .assignment_pattern => |ap| try self.checkAssignTargetEvalArguments(ap.left, ctx),
            .binding_rest_element => |r| try self.checkAssignTargetEvalArguments(r.argument, ctx),
            else => {},
        }
    }

    fn unwrapParens(tree: *const ast.Tree, node: ast.NodeIndex) ast.NodeIndex {
        var current = node;
        while (true) {
            switch (tree.data(current)) {
                .parenthesized_expression => |p| current = p.expression,
                else => return current,
            }
        }
    }

    /// leading zero followed by digits, legacy octal (077) or leading-zero decimal (089).
    fn isLegacyNumericLiteral(raw: []const u8) bool {
        return raw.len >= 2 and raw[0] == '0' and raw[1] >= '0' and raw[1] <= '9';
    }

    const SuperCallValidity = enum { valid, not_in_constructor, no_extends };

    /// super() is only permitted inside a constructor of a derived class.
    /// Arrow functions are transparent (Section 8.5.1), regular functions are opaque.
    fn superCallValidity(ctx: *SemanticCtx) SuperCallValidity {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.data(i)) {
                .arrow_function_expression => {},
                .function => {
                    if (iter.next()) |parent| {
                        if (ctx.tree.data(parent) == .method_definition and
                            ctx.tree.data(parent).method_definition.kind == .constructor)
                        {
                            while (iter.next()) |ancestor| {
                                if (ctx.tree.data(ancestor) == .class)
                                    return if (ctx.tree.data(ancestor).class.super_class != .null)
                                        .valid
                                    else
                                        .no_extends;
                            }
                        }
                    }
                    return .not_in_constructor;
                },
                .property_definition, .static_block, .program => return .not_in_constructor,
                else => {},
            }
        }
        return .not_in_constructor;
    }

    /// super.property is valid in class methods, object methods, field
    /// initializers, static blocks, and arrow functions inheriting from those.
    fn isSuperPropertyValid(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.data(i)) {
                .arrow_function_expression => {},
                .function => {
                    if (iter.next()) |parent| {
                        const data = ctx.tree.data(parent);
                        if (data == .method_definition) return true;
                        if (data == .object_property) {
                            const prop = data.object_property;
                            if (prop.method or prop.kind != .init) return true;
                        }
                    }
                    return false;
                },
                .property_definition, .static_block => return true,
                .program => return false,
                else => {},
            }
        }
        return false;
    }

    fn isAtProgramLevel(ctx: *SemanticCtx) bool {
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

    fn isInsideBreakable(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.data(i);
            if (data.isIteration() or data == .switch_statement) return true;
            if (isFunctionBoundary(data)) return false;
        }
        return false;
    }

    fn isInsideIteration(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.data(i);
            if (data.isIteration()) return true;
            if (isFunctionBoundary(data)) return false;
        }
        return false;
    }

    fn isNewTargetAvailable(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.data(i)) {
                .function, .static_block, .property_definition => return true,
                .arrow_function_expression => {},
                else => {},
            }
        }
        return false;
    }

    const LabelSearch = enum { found, not_found, crossed_boundary };

    fn findLabel(ctx: *SemanticCtx, name: []const u8) LabelSearch {
        var crossed_boundary = false;
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.data(i);
            if (data == .labeled_statement) {
                const lbl_name = ctx.tree.string(ctx.tree.data(data.labeled_statement.label).label_identifier.name);
                if (eql(u8, lbl_name, name))
                    return if (crossed_boundary) .crossed_boundary else .found;
            }
            if (isFunctionBoundary(data)) crossed_boundary = true;
        }
        return .not_found;
    }

    const ContinueLabelSearch = enum { found, not_found, crossed_boundary, not_iteration };

    fn findLabelForContinue(ctx: *SemanticCtx, name: []const u8) ContinueLabelSearch {
        var crossed_boundary = false;
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.data(i);
            if (data == .labeled_statement) {
                const ls = data.labeled_statement;
                const lbl_name = ctx.tree.string(ctx.tree.data(ls.label).label_identifier.name);
                if (eql(u8, lbl_name, name)) {
                    if (crossed_boundary) return .crossed_boundary;
                    const body = ctx.tree.data(ls.body);
                    if (body.isIteration() or body == .labeled_statement) return .found;
                    return .not_iteration;
                }
            }
            if (isFunctionBoundary(data)) crossed_boundary = true;
        }
        return .not_found;
    }

    // true when the nearest enclosing boundary is a static block
    fn isInsideStaticBlock(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.data(i)) {
                .static_block => return true,
                .function, .arrow_function_expression => return false,
                else => {},
            }
        }
        return false;
    }

    /// `arguments` is available inside regular functions but not in class field
    /// initializers or static blocks (arrow functions are transparent).
    fn isArgumentsAvailable(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.data(i)) {
                .function => return true,
                .property_definition, .static_block => return false,
                .arrow_function_expression, .program => {},
                else => {},
            }
        }
        return true;
    }

    fn isInFormalParameters(ctx: *SemanticCtx) bool {
        return findFormalParameters(ctx) != null;
    }

    fn findFormalParameters(ctx: *SemanticCtx) ?ast.FormalParameters {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.data(i)) {
                .formal_parameters => |params| return params,
                .program, .function, .arrow_function_expression => return null,
                else => {},
            }
        }
        return null;
    }

    /// extracts the name from a ModuleExportName (IdentifierName, IdentifierReference, or StringLiteral).
    fn getModuleExportName(tree: *const ast.Tree, node: ast.NodeIndex) []const u8 {
        return switch (tree.data(node)) {
            .identifier_name => |id| tree.string(id.name),
            .identifier_reference => |id| tree.string(id.name),
            .string_literal => |lit| tree.string(lit.value),
            else => "",
        };
    }

    fn checkDuplicateWithClaudeAttributes(self: *Self, attributes: ast.IndexRange, ctx: *SemanticCtx) AnalysisError!void {
        const items = ctx.tree.extra(attributes);
        for (items, 0..) |attr_idx, i| {
            const key = ecmascript.propName(ctx.tree, ctx.tree.data(attr_idx).import_attribute.key) orelse continue;
            for (items[0..i]) |prev_idx| {
                const prev_key = ecmascript.propName(ctx.tree, ctx.tree.data(prev_idx).import_attribute.key) orelse continue;
                if (key.eql(prev_key.name) or prev_key.eql(key.name)) {
                    const name = if (!prev_key.is_string_literal) prev_key.name else key.name;
                    try self.report(key.span, try self.fmt("Duplicate import attribute key '{s}'", .{name}), .{
                        .labels = try self.labels(&.{
                            self.label(prev_key.span, "first used here"),
                        }),
                    });
                    break;
                }
            }
        }
    }

    /// records an exported name and reports a duplicate if one already exists.
    fn recordExportedName(self: *Self, name: []const u8, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!void {
        if (!ctx.tree.isModule()) return;
        // exports inside a TS namespace are namespace-scoped, not module-scoped
        if (ctx.inTsNamespace()) return;
        const gop = try self.exported_names.getOrPut(self.allocator, name);
        if (gop.found_existing) {
            try self.report(ctx.tree.span(node_index), try self.fmt("Duplicate export of '{s}'", .{name}), .{
                .labels = try self.labels(&.{
                    self.label(ctx.tree.span(gop.value_ptr.*), "first exported here"),
                    self.label(ctx.tree.span(node_index), "exported again here"),
                }),
            });
        } else {
            gop.value_ptr.* = node_index;
        }
    }

    /// Post-traversal: checks that all local export specifiers refer to declared bindings.
    fn checkUnresolvedExports(self: *Self, result: semantic.Result) AnalysisError!void {
        if (!self.tree.isModule()) return;
        for (self.export_specifiers.items) |spec| {
            if (result.symbol_table.findInScopeOrHoisted(.module, spec.local_name) == null) {
                try self.report(self.tree.span(spec.node), try self.fmt("Export '{s}' is not defined", .{spec.local_name}), .{});
            }
        }
    }

    fn reportRedeclaration(self: *Self, id: ast.BindingIdentifier, node_index: ast.NodeIndex, existing: Symbol, ctx: *SemanticCtx) Allocator.Error!void {
        const name = ctx.tree.string(id.name);
        const current_span = ctx.tree.span(node_index);
        const existing_span = ctx.tree.span(existing.node);

        try self.report(current_span, try self.fmt("Identifier '{s}' has already been declared", .{name}), .{
            .labels = try self.labels(&.{
                self.label(existing_span, try self.fmt("'{s}' was first declared as a {s} here", .{ name, existing.flags.toString() })),
                self.label(current_span, "cannot be redeclared here"),
            }),
            .help = try self.fmt("Consider removing or renaming this declaration of '{s}'", .{name}),
        });
    }

    const ReportOptions = struct {
        severity: ast.Severity = .@"error",
        help: ?[]const u8 = null,
        labels: []const ast.Label = &.{},
    };

    fn report(self: *Self, span: ast.Span, message: []const u8, opts: ReportOptions) Allocator.Error!void {
        try self.tree.addDiagnostic(.{
            .severity = opts.severity,
            .message = message,
            .span = span,
            .help = opts.help,
            .labels = opts.labels,
        });
    }

    fn label(_: *Self, span: ast.Span, message: []const u8) ast.Label {
        return .{ .span = span, .message = message };
    }

    fn labels(self: *Self, items: []const ast.Label) Allocator.Error![]const ast.Label {
        return try self.allocator.dupe(ast.Label, items);
    }

    fn fmt(self: *Self, comptime format: []const u8, args: anytype) Allocator.Error![]u8 {
        return try std.fmt.allocPrint(self.allocator, format, args);
    }
};
