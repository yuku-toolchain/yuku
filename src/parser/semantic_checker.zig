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
    export_specifiers: std.ArrayListUnmanaged(ExportSpecifierInfo) = .{},

    const ExportSpecifierInfo = struct {
        local_name: []const u8,
        node: ast.NodeIndex,
    };

    pub fn enter_binding_identifier(self: *Self, id: ast.BindingIdentifier, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const name = ctx.tree.getString(id.name);

        // https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
        if (ctx.scope.isStrict()) {
            try self.checkStrictReserved(name, node_index, ctx, "a binding identifier");

            if (isEvalOrArguments(name))
                try self.report(ctx.tree.getSpan(node_index), try self.fmt("'{s}' is not allowed as a binding identifier in strict mode", .{name}), .{});
        } else if (ctx.symbols.currentBindingKind() == .lexical and eql(u8, name, "let")) {
            try self.report(ctx.tree.getSpan(node_index), "'let' is not allowed as a variable name in a lexical declaration", .{});
        }

        try self.checkModuleReserved(name, node_index, ctx, "a binding identifier");

        // redeclaration checks

        const target = ctx.symbols.resolveTargetScope(&ctx.scope);
        const current_kind = ctx.symbols.currentBindingKind();
        const target_scope_kind = ctx.scope.getScope(target).kind;

        if (ctx.symbols.findInScopeOrHoisted(target, name)) |sym| {
            const existing = ctx.symbols.getSymbol(sym);

            // https://tc39.es/ecma262/#sec-block-static-semantics-early-errors
            // https://tc39.es/ecma262/#sec-switch-statement-static-semantics-early-errors
            if (existing.kind.isBlockScoped(target_scope_kind) or
                current_kind.isBlockScoped(target_scope_kind))
            {
                try self.reportRedeclaration(id, node_index, existing, ctx);
                return .proceed;
            }

            // https://tc39.es/ecma262/#sec-parameter-lists-static-semantics-early-errors
            if (existing.kind == .parameter) {
                if (findFormalParameters(ctx)) |formal_parameters| {
                    if (
                        ctx.scope.isStrict() or
                        formal_parameters.kind == .unique_formal_parameters or
                        formal_parameters.kind == .arrow_formal_parameters
                    ) {
                        try self.reportRedeclaration(id, node_index, existing, ctx);
                    } else if (ecmascript.findNonSimpleParameter(ctx.tree, formal_parameters)) |_| {
                        try self.reportRedeclaration(id, node_index, existing, ctx);
                    }
                }
            }
        }

        // a hoisted var must also check intermediate block scopes for
        // conflicting block-scoped declarations (e.g. `{ let x; var x; }`).
        if (current_kind == .hoisted) {
            var iter = ctx.scope.ancestors(ctx.scope.currentScopeId());
            while (iter.next()) |scope_id| {
                if (scope_id == target) break;
                if (ctx.symbols.findInScope(scope_id, name)) |sym| {
                    if (ctx.symbols.getSymbol(sym).kind.isBlockScoped(.block)) {
                        try self.reportRedeclaration(id, node_index, ctx.symbols.getSymbol(sym), ctx);
                        break;
                    }
                }
            }
        }

        // track exported binding names (export const x, export function f, etc.)
        if (ctx.symbols.is_export and !ctx.symbols.is_default_export)
            try self.recordExportedName(name, node_index, ctx);

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
    pub fn enter_identifier_reference(self: *Self, id: ast.IdentifierReference, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const name = ctx.tree.getString(id.name);
        try self.checkStrictReserved(name, node_index, ctx, "an identifier");
        try self.checkModuleReserved(name, node_index, ctx, "an identifier");

        // https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
        if (eql(u8, name, "arguments") and !isArgumentsAvailable(ctx))
            try self.report(ctx.tree.getSpan(node_index), "'arguments' is not allowed in class field initializers or static blocks", .{});

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
    pub fn enter_label_identifier(self: *Self, id: ast.LabelIdentifier, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const name = ctx.tree.getString(id.name);
        try self.checkStrictReserved(name, node_index, ctx, "a label");
        try self.checkModuleReserved(name, node_index, ctx, "a label");
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-string-literals-static-semantics-early-errors
    pub fn enter_string_literal(self: *Self, lit: ast.StringLiteral, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (!ctx.scope.isStrict()) return .proceed;

        if (util.Utf.hasOctalEscape(ctx.tree.getString(lit.raw)))
            try self.report(ctx.tree.getSpan(node_index), "Octal escape sequences are not allowed in strict mode", .{
                .help = "Use \\xHH (hex) or \\uHHHH (unicode) escape instead",
            });

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-additional-syntax-numeric-literals
    pub fn enter_numeric_literal(self: *Self, lit: ast.NumericLiteral, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (!ctx.scope.isStrict()) return .proceed;

        const raw = ctx.tree.getString(lit.raw);

        if (!isLegacyNumericLiteral(raw)) return .proceed;

        const is_octal = for (raw) |c| {
            if (c == '8' or c == '9') break false;
        } else true;

        try self.report(ctx.tree.getSpan(node_index), if (is_octal)
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

    /// https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
    pub fn enter_identifier_reference(self: *Self, id: ast.IdentifierReference, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkStrictReserved(ctx.tree.getString(id.name), node_index, ctx, "an identifier");
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-identifiers-static-semantics-early-errors
    pub fn enter_label_identifier(self: *Self, id: ast.LabelIdentifier, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkStrictReserved(ctx.tree.getString(id.name), node_index, ctx, "a label");
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-string-literals-static-semantics-early-errors
    pub fn enter_string_literal(self: *Self, lit: ast.StringLiteral, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (!ctx.scope.isStrict()) return .proceed;

        if (util.Utf.hasOctalEscape(ctx.tree.getString(lit.raw)))
            try self.report(ctx.tree.getSpan(node_index), "Octal escape sequences are not allowed in strict mode", .{
                .help = "Use \\xHH (hex) or \\uHHHH (unicode) escape instead",
            });

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-additional-syntax-numeric-literals
    pub fn enter_numeric_literal(self: *Self, lit: ast.NumericLiteral, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (!ctx.scope.isStrict()) return .proceed;

        const raw = ctx.tree.getString(lit.raw);

        if (!isLegacyNumericLiteral(raw)) return .proceed;

        const is_octal = for (raw) |c| {
            if (c == '8' or c == '9') break false;
        } else true;

        try self.report(ctx.tree.getSpan(node_index), if (is_octal)
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
        if (eql(u8, ctx.tree.getString(directive.value), "use strict")) {
            var iter = ctx.path.ancestors();
            while (iter.next()) |i| {
                switch (ctx.tree.getData(i)) {
                    .function => |func| {
                        if (func.params != .null)
                            if (ecmascript.findNonSimpleParameter(ctx.tree, ctx.tree.getData(func.params).formal_parameters)) |param| {
                                try self.report(ctx.tree.getSpan(node_index), "Illegal 'use strict' directive in function with non-simple parameter list", .{
                                    .labels = try self.labels(&.{
                                        self.label(ctx.tree.getSpan(param), "non-simple parameter"),
                                    }),
                                });
                            };
                        break;
                    },
                    .arrow_function_expression => |arrow| {
                        if (ecmascript.findNonSimpleParameter(ctx.tree, ctx.tree.getData(arrow.params).formal_parameters)) |param| {
                            try self.report(ctx.tree.getSpan(node_index), "Illegal 'use strict' directive in function with non-simple parameter list", .{
                                .labels = try self.labels(&.{
                                    self.label(ctx.tree.getSpan(param), "non-simple parameter"),
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
        _ = iter.next(); // skip the function node itself

        const parent = while (iter.next()) |idx| {
            const d = ctx.tree.getData(idx);
            if (d != .labeled_statement) break d;
            through_labels = true;
        } else return .proceed;

        const is_valid = switch (parent) {
            .program, .function_body, .class_body, .static_block,
            .block_statement, .switch_case,
            .export_named_declaration, .export_default_declaration,
            => !is_strict or !through_labels,
            .if_statement => !is_strict and !through_labels,
            else => false,
        };

        if (!is_valid) {
            try self.report(ctx.tree.getSpan(node_index), if (is_strict)
                "In strict mode code, functions can only be declared at top level or inside a block"
            else
                "In non-strict mode code, functions can only be declared at top level, inside a block, or as the body of an if statement", .{});
        }
        return .proceed;
    }

    pub fn enter_yield_expression(self: *Self, _: ast.YieldExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (isInFormalParameters(ctx))
            try self.report(ctx.tree.getSpan(node_index), "Yield expression is not allowed in formal parameters", .{});
        return .proceed;
    }

    pub fn enter_await_expression(self: *Self, _: ast.AwaitExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (isInFormalParameters(ctx))
            try self.report(ctx.tree.getSpan(node_index), "Await expression is not allowed in formal parameters", .{});
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-update-expressions-static-semantics-early-errors
    pub fn enter_update_expression(self: *Self, expr: ast.UpdateExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.scope.isStrict() and isEvalOrArgumentsRef(ctx.tree, expr.argument))
            try self.report(ctx.tree.getSpan(node_index), "Cannot assign to 'eval' or 'arguments' in strict mode", .{});
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-assignment-operators-static-semantics-early-errors
    pub fn enter_assignment_expression(self: *Self, expr: ast.AssignmentExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.scope.isStrict() and isEvalOrArgumentsRef(ctx.tree, expr.left))
            try self.report(ctx.tree.getSpan(node_index), "Cannot assign to 'eval' or 'arguments' in strict mode", .{});
        return .proceed;
    }

    pub fn enter_unary_expression(self: *Self, expr: ast.UnaryExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (expr.operator == .delete) {
            const target = unwrapParens(ctx.tree, expr.argument);
            switch (ctx.tree.getData(target)) {
                // https://tc39.es/ecma262/#sec-delete-operator-static-semantics-early-errors
                .identifier_reference => if (ctx.scope.isStrict()) {
                    try self.report(ctx.tree.getSpan(node_index), "Deleting a variable in strict mode is not allowed", .{});
                },
                .member_expression => |m| if (!m.computed and ctx.tree.getData(m.property) == .private_identifier) {
                    try self.report(ctx.tree.getSpan(node_index), "Private fields cannot be deleted", .{});
                },
                else => {},
            }
        }
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-__proto__-property-names-in-object-initializers
    pub fn enter_object_expression(self: *Self, obj: ast.ObjectExpression, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        var first_proto: ?ast.NodeIndex = null;
        for (ctx.tree.getExtra(obj.properties)) |child| {
            if (ctx.tree.getData(child) != .object_property) continue;
            const prop = ctx.tree.getData(child).object_property;
            if (prop.computed or prop.method or prop.shorthand or prop.kind != .init) continue;
            const pn = ecmascript.propName(ctx.tree, prop.key) orelse continue;
            if (!pn.eql("__proto__")) continue;

            if (first_proto) |first| {
                try self.report(ctx.tree.getSpan(child), "Duplicate '__proto__' property in object literal", .{
                    .labels = try self.labels(&.{
                        self.label(ctx.tree.getSpan(first), "first defined here"),
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
        if (ctx.tree.getData(expr.callee) == .super) {
            switch (superCallValidity(ctx)) {
                .valid => {},
                .not_in_constructor => try self.report(ctx.tree.getSpan(node_index), "'super()' is only valid in a constructor of a derived class", .{
                    .help = "Use an arrow function instead of a regular function to inherit the 'super' binding",
                }),
                .no_extends => try self.report(ctx.tree.getSpan(node_index), "'super()' is only valid in a constructor of a derived class", .{
                    .help = "Add an 'extends' clause to the class or remove the 'super()' call",
                }),
            }
        }
        return .proceed;
    }

    pub fn enter_member_expression(self: *Self, expr: ast.MemberExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.tree.getData(expr.object) == .super) {
            if (!isSuperPropertyValid(ctx))
                try self.report(ctx.tree.getSpan(node_index), "'super' property access is only valid inside a method or class body", .{
                    .help = "Use an arrow function instead of a regular function to inherit the 'super' binding",
                });
        }
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-left-hand-side-expressions-static-semantics-early-errors
    pub fn enter_meta_property(self: *Self, prop: ast.MetaProperty, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const meta = ctx.tree.getData(prop.meta);
        if (meta != .identifier_name) return .proceed;
        const name = ctx.tree.getString(meta.identifier_name.name);

        if (eql(u8, name, "import") and !ctx.tree.isModule())
            try self.report(ctx.tree.getSpan(node_index), "'import.meta' is only valid in module code", .{});

        // https://tc39.es/ecma262/#sec-static-semantics-early-errors
        if (eql(u8, name, "new") and !isNewTargetAvailable(ctx))
            try self.report(ctx.tree.getSpan(node_index), "'new.target' is only valid inside functions, class field initializers, or static blocks", .{});

        return .proceed;
    }

    pub fn enter_import_declaration(self: *Self, _: ast.ImportDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (!ctx.tree.isModule())
            try self.report(ctx.tree.getSpan(node_index), "Cannot use import statement outside a module", .{})
        else if (!isAtModuleTopLevel(ctx))
            try self.report(ctx.tree.getSpan(node_index), "'import' declaration may only appear at the top level", .{});
        return .proceed;
    }

    pub fn enter_export_named_declaration(self: *Self, _: ast.ExportNamedDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (!ctx.tree.isModule())
            try self.report(ctx.tree.getSpan(node_index), "Cannot use 'export' declaration outside a module", .{})
        else if (!isAtModuleTopLevel(ctx))
            try self.report(ctx.tree.getSpan(node_index), "'export' declaration may only appear at the top level", .{});
        return .proceed;
    }

    pub fn enter_export_default_declaration(self: *Self, _: ast.ExportDefaultDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (!ctx.tree.isModule())
            try self.report(ctx.tree.getSpan(node_index), "Cannot use 'export default' declaration outside a module", .{})
        else if (!isAtModuleTopLevel(ctx))
            try self.report(ctx.tree.getSpan(node_index), "'export default' declaration may only appear at the top level", .{});

        try self.recordExportedName("default", node_index, ctx);
        return .proceed;
    }

    pub fn enter_export_all_declaration(self: *Self, decl: ast.ExportAllDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (!ctx.tree.isModule())
            try self.report(ctx.tree.getSpan(node_index), "Cannot use 'export *' declaration outside a module", .{})
        else if (!isAtModuleTopLevel(ctx))
            try self.report(ctx.tree.getSpan(node_index), "'export *' declaration may only appear at the top level", .{});

        // export * as name from '...'
        if (decl.exported != .null)
            try self.recordExportedName(getModuleExportName(ctx.tree, decl.exported), node_index, ctx);

        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-exports-static-semantics-early-errors
    pub fn enter_export_specifier(self: *Self, spec: ast.ExportSpecifier, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const exported_name = getModuleExportName(ctx.tree, spec.exported);
        try self.recordExportedName(exported_name, node_index, ctx);

        // collect for unresolved check (only for local exports, not re-exports)
        if (ctx.path.parent()) |parent| {
            if (ctx.tree.getData(parent) == .export_named_declaration and
                ctx.tree.getData(parent).export_named_declaration.source == .null)
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
            try self.report(ctx.tree.getSpan(node_index), "'with' statements are not allowed in strict mode", .{});
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements-static-semantics-early-errors
    pub fn enter_for_of_statement(self: *Self, stmt: ast.ForOfStatement, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkForInOfInitializer(ctx, stmt.left);
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-for-in-and-for-of-statements-static-semantics-early-errors
    pub fn enter_for_in_statement(self: *Self, stmt: ast.ForInStatement, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        try self.checkForInOfInitializer(ctx, stmt.left);
        return .proceed;
    }

    pub fn enter_break_statement(self: *Self, stmt: ast.BreakStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (stmt.label != .null) {
            const label_name = ctx.tree.getString(ctx.tree.getData(stmt.label).label_identifier.name);
            switch (findLabel(ctx, label_name)) {
                .found => {},
                .not_found => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Use of undefined label '{s}'", .{label_name}), .{}),
                .crossed_boundary => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Cannot break to label '{s}' across function boundaries", .{label_name}), .{}),
            }
        } else {
            if (!isInsideBreakable(ctx))
                try self.report(ctx.tree.getSpan(node_index), "Illegal break statement", .{
                    .help = "A 'break' statement can only be used within an enclosing iteration or switch statement",
                });
        }
        return .proceed;
    }

    pub fn enter_continue_statement(self: *Self, stmt: ast.ContinueStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (stmt.label != .null) {
            const label_name = ctx.tree.getString(ctx.tree.getData(stmt.label).label_identifier.name);
            switch (findLabelForContinue(ctx, label_name)) {
                .found => {},
                .not_found => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Use of undefined label '{s}'", .{label_name}), .{}),
                .crossed_boundary => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Cannot continue to label '{s}' across function boundaries", .{label_name}), .{}),
                .not_iteration => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Label '{s}' does not denote an iteration statement", .{label_name}), .{
                    .help = "A 'continue' statement can only jump to a label of an enclosing 'for', 'while', 'do-while', 'for-in', or 'for-of' statement",
                }),
            }
        } else {
            if (!isInsideIteration(ctx))
                try self.report(ctx.tree.getSpan(node_index), "Illegal continue statement", .{
                    .help = "A 'continue' statement can only be used within an enclosing iteration statement",
                });
        }
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-labelled-statements-static-semantics-early-errors
    pub fn enter_labeled_statement(self: *Self, stmt: ast.LabeledStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        _ = node_index;
        const name = ctx.tree.getString(ctx.tree.getData(stmt.label).label_identifier.name);
        var iter = ctx.path.ancestors();
        _ = iter.next(); // skip current node
        while (iter.next()) |i| {
            const data = ctx.tree.getData(i);
            if (data == .labeled_statement) {
                if (eql(u8, ctx.tree.getString(ctx.tree.getData(data.labeled_statement.label).label_identifier.name), name))
                    try self.report(ctx.tree.getSpan(stmt.label), try self.fmt("Duplicate label '{s}'", .{name}), .{});
            }
            if (isFunctionBoundary(data)) break;
        }
        return .proceed;
    }

    /// https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn enter_class_body(self: *Self, body: ast.ClassBody, _: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        var first_constructor: ?ast.NodeIndex = null;
        for (ctx.tree.getExtra(body.body)) |child| {
            const child_data = ctx.tree.getData(child);

            if (child_data != .method_definition) continue;

            const method_definition = child_data.method_definition;

            if (method_definition.kind != .constructor) continue;

            if (first_constructor) |first| {
                try self.report(ctx.tree.getSpan(method_definition.key), "A class can only have one constructor", .{
                    .labels = try self.labels(&.{
                        self.label(ctx.tree.getSpan(first), "first constructor defined here"),
                    }),
                });
            } else {
                first_constructor = method_definition.key;
            }
        }
        return .proceed;
    }

    fn checkStrictReserved(self: *Self, name: []const u8, node_index: ast.NodeIndex, ctx: *SemanticCtx, comptime as_what: []const u8) AnalysisError!void {
        if (!ctx.scope.isStrict()) return;
        if (matchStrictReserved(name)) |word|
            try self.report(ctx.tree.getSpan(node_index), try self.fmt("'{s}' is reserved in strict mode and cannot be used as " ++ as_what, .{word}), .{});
    }

    fn checkModuleReserved(self: *Self, name: []const u8, node_index: ast.NodeIndex, ctx: *SemanticCtx, comptime as_what: []const u8) AnalysisError!void {
        if (!ctx.tree.isModule()) return;
        if (eql(u8, name, "await"))
            try self.report(ctx.tree.getSpan(node_index), "'await' is reserved in module code and cannot be used as " ++ as_what, .{});
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

    fn checkForInOfInitializer(self: *Self, ctx: *SemanticCtx, left: ast.NodeIndex) AnalysisError!void {
        if (ctx.tree.getData(left) != .variable_declaration) return;
        const decl = ctx.tree.getData(left).variable_declaration;
        for (ctx.tree.getExtra(decl.declarators)) |child| {
            const declarator = ctx.tree.getData(child).variable_declarator;
            if (declarator.init != .null) {
                try self.report(ctx.tree.getSpan(child), "for-in/of loop variable declaration may not have an initializer", .{});
                return;
            }
        }
    }

    fn isEvalOrArguments(name: []const u8) bool {
        return eql(u8, name, "eval") or eql(u8, name, "arguments");
    }

    fn isEvalOrArgumentsRef(tree: *const ast.Tree, node: ast.NodeIndex) bool {
        return tree.getData(node) == .identifier_reference and
            isEvalOrArguments(tree.getString(tree.getData(node).identifier_reference.name));
    }

    fn unwrapParens(tree: *const ast.Tree, node: ast.NodeIndex) ast.NodeIndex {
        var current = node;
        while (true) {
            switch (tree.getData(current)) {
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
            switch (ctx.tree.getData(i)) {
                .arrow_function_expression => {},
                .function => {
                    if (iter.next()) |parent| {
                        if (ctx.tree.getData(parent) == .method_definition and
                            ctx.tree.getData(parent).method_definition.kind == .constructor)
                        {
                            while (iter.next()) |ancestor| {
                                if (ctx.tree.getData(ancestor) == .class)
                                    return if (ctx.tree.getData(ancestor).class.super_class != .null)
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
            switch (ctx.tree.getData(i)) {
                .arrow_function_expression => {},
                .function => {
                    if (iter.next()) |parent| {
                        const data = ctx.tree.getData(parent);
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

    fn isAtModuleTopLevel(ctx: *SemanticCtx) bool {
        if (ctx.path.parent()) |parent| {
            return ctx.tree.getData(parent) == .program;
        }
        return true;
    }

    fn isIterationStatement(data: ast.NodeData) bool {
        return switch (data) {
            .for_statement, .for_in_statement, .for_of_statement, .while_statement, .do_while_statement => true,
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
            const data = ctx.tree.getData(i);
            if (isIterationStatement(data) or data == .switch_statement) return true;
            if (isFunctionBoundary(data)) return false;
        }
        return false;
    }

    fn isInsideIteration(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.getData(i);
            if (isIterationStatement(data)) return true;
            if (isFunctionBoundary(data)) return false;
        }
        return false;
    }

    fn isNewTargetAvailable(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.getData(i)) {
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
            const data = ctx.tree.getData(i);
            if (data == .labeled_statement) {
                const lbl_name = ctx.tree.getString(ctx.tree.getData(data.labeled_statement.label).label_identifier.name);
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
            const data = ctx.tree.getData(i);
            if (data == .labeled_statement) {
                const ls = data.labeled_statement;
                const lbl_name = ctx.tree.getString(ctx.tree.getData(ls.label).label_identifier.name);
                if (eql(u8, lbl_name, name)) {
                    if (crossed_boundary) return .crossed_boundary;
                    const body = ctx.tree.getData(ls.body);
                    if (isIterationStatement(body) or body == .labeled_statement) return .found;
                    return .not_iteration;
                }
            }
            if (isFunctionBoundary(data)) crossed_boundary = true;
        }
        return .not_found;
    }

    /// `arguments` is available inside regular functions but not in class field
    /// initializers or static blocks (arrow functions are transparent).
    fn isArgumentsAvailable(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.getData(i)) {
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
            switch (ctx.tree.getData(i)) {
                .formal_parameters => |params| return params,
                .program, .function, .arrow_function_expression => return null,
                else => {},
            }
        }
        return null;
    }

    /// extracts the name from a ModuleExportName (IdentifierName, IdentifierReference, or StringLiteral).
    fn getModuleExportName(tree: *const ast.Tree, node: ast.NodeIndex) []const u8 {
        return switch (tree.getData(node)) {
            .identifier_name => |id| tree.getString(id.name),
            .identifier_reference => |id| tree.getString(id.name),
            .string_literal => |lit| lit.value(tree),
            else => "",
        };
    }

    /// records an exported name and reports a duplicate if one already exists.
    fn recordExportedName(self: *Self, name: []const u8, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!void {
        if (!ctx.tree.isModule()) return;
        const gop = try self.exported_names.getOrPut(self.allocator, name);
        if (gop.found_existing) {
            try self.report(ctx.tree.getSpan(node_index), try self.fmt("Duplicate export of '{s}'", .{name}), .{
                .labels = try self.labels(&.{
                    self.label(ctx.tree.getSpan(gop.value_ptr.*), "first exported here"),
                    self.label(ctx.tree.getSpan(node_index), "exported again here"),
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
                try self.report(self.tree.getSpan(spec.node), try self.fmt("Export '{s}' is not defined", .{spec.local_name}), .{});
            }
        }
    }

    fn reportRedeclaration(self: *Self, id: ast.BindingIdentifier, node_index: ast.NodeIndex, existing: Symbol, ctx: *SemanticCtx) Allocator.Error!void {
        const name = ctx.tree.getString(id.name);
        const current_span = ctx.tree.getSpan(node_index);
        const existing_span = ctx.tree.getSpan(existing.node);

        try self.report(current_span, try self.fmt("Identifier '{s}' has already been declared", .{name}), .{
            .labels = try self.labels(&.{
                self.label(existing_span, try self.fmt("'{s}' was first declared as a {s} here", .{ name, existing.kind.toString() })),
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
        try self.tree.appendDiagnostic(.{
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
