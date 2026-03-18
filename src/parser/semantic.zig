// this is wip, even though redeclaration checks etc are done,
// there are still a lot of semantic errors to cover

const std = @import("std");
const traverser = @import("traverser/root.zig");
const ast = @import("ast.zig");

const Allocator = std.mem.Allocator;

const semantic = traverser.semantic;
const Symbol = semantic.Symbol;

const Action = traverser.Action;
const SemanticCtx = semantic.Ctx;

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

    return try semantic.traverse(SemanticVisit, tree, &visitor);
}

const SemanticVisit = struct {
    const Self = @This();

    tree: *ast.Tree,
    allocator: Allocator,

    pub fn enter_binding_identifier(self: *Self, id: ast.BindingIdentifier, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        const target = ctx.symbols.resolveTargetScope(&ctx.scope);

        if (ctx.symbols.findInScope(target, ctx.tree.getString(id.name))) |sym| {
            const existing = ctx.symbols.getSymbol(sym);
            const current_kind = ctx.symbols.currentBindingKind();

            // Section 14.2.1:  "It is a Syntax Error if the LexicallyDeclaredNames
            //                   of StatementList contains any duplicate entries."
            // Section 16.1.4:  "It is a Syntax Error if any element of the
            //                   LexicallyDeclaredNames ... also occurs in the
            //                   VarDeclaredNames ..."
            if (existing.kind.isLexical() or current_kind.isLexical()) {
                try self.reportRedeclaration(id, node_index, existing, ctx);
            }
        }

        return .proceed;
    }

    pub fn enter_yield_expression(self: *Self, _: ast.YieldExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (isInFormalParameters(ctx)) {
            try self.report(ctx.tree.getSpan(node_index), "Yield expression is not allowed in formal parameters", .{});
        }

        return .proceed;
    }

    pub fn enter_import_declaration(self: *Self, _: ast.ImportDeclaration, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.tree.source_type != .module) {
            try self.report(ctx.tree.getSpan(node_index), "Cannot use import statement outside a module", .{});
        }

        return .proceed;
    }

    pub fn enter_await_expression(self: *Self, _: ast.AwaitExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (isInFormalParameters(ctx)) {
            try self.report(ctx.tree.getSpan(node_index), "Await expression is not allowed in formal parameters", .{});
        }

        return .proceed;
    }

    fn isInFormalParameters(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.getData(i)) {
                .formal_parameter => return true,
                .program, .function, .arrow_function_expression => return false,
                else => {},
            }
        }
        return false;
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

    pub fn report(self: *Self, span: ast.Span, message: []const u8, opts: ReportOptions) Allocator.Error!void {
        try self.tree.appendDiagnostic(.{
            .severity = opts.severity,
            .message = message,
            .span = span,
            .help = opts.help,
            .labels = opts.labels,
        });
    }

    pub fn label(_: *Self, span: ast.Span, message: []const u8) ast.Label {
        return .{ .span = span, .message = message };
    }

    pub fn labels(self: *Self, items: []const ast.Label) Allocator.Error![]const ast.Label {
        return try self.allocator.dupe(ast.Label, items);
    }

    pub fn fmt(self: *Self, comptime format: []const u8, args: anytype) Allocator.Error![]u8 {
        return try std.fmt.allocPrint(self.allocator, format, args);
    }
};

// TODO:
//
// Redeclaration checks.
// It is a Syntax Error if FunctionBodyContainsUseStrict of FunctionBody is true and IsSimpleParameterList of FormalParameters is false.
// It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the LexicallyDeclaredNames of FunctionBody.
// It is a Syntax Error if FormalParameters Contains SuperProperty is true.
// It is a Syntax Error if FunctionBody Contains SuperProperty is true.
// It is a Syntax Error if FormalParameters Contains SuperCall is true.
// It is a Syntax Error if FormalParameters Contains YieldExpression is true.
// It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
// It is a Syntax Error if FunctionBody Contains SuperCall is true.
// 'evals' and 'arguments' in binding identifier and identifier reference.
// Reserved checks: https://tc39.es/ecma262/#prod-ReservedWord
// It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
// It is a Syntax Error if ConciseBodyContainsUseStrict of ConciseBody is true and IsSimpleParameterList of ArrowParameters is false.
// It is a Syntax Error if any element of the BoundNames of ArrowParameters also occurs in the LexicallyDeclaredNames of ConciseBody.
// (delete unary) It is a Syntax Error if IsStrict(the UnaryExpression) is true and the derived UnaryExpression is PrimaryExpression : IdentifierReference , MemberExpression : MemberExpression . PrivateIdentifier , CallExpression : CallExpression . PrivateIdentifier , OptionalChain : ?. PrivateIdentifier , or OptionalChain : OptionalChain . PrivateIdentifier .
// (delete unary) It is a Syntax Error if the derived UnaryExpression is
// PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
// and CoverParenthesizedExpressionAndArrowParameterList ultimately derives a phrase that, if used in place of UnaryExpression, would produce a Syntax Error according to these rules. This rule is recursively applied.
// Note
// (delete unary) The last rule means that expressions such as delete (((foo))) produce early errors because of recursive application of the first rule.
// ImportMeta :
//  import.meta
//  It is a Syntax Error if the syntactic goal symbol is not Module.
// It is a Syntax Error if this BreakStatement is not nested, directly or indirectly (but not crossing function or static initialization block boundaries), within an IterationStatement or a SwitchStatement.
// It is a Syntax Error if this ContinueStatement is not nested, directly or indirectly (but not crossing function or static initialization block boundaries), within an IterationStatement.
// It is a Syntax Error if any element of the BoundNames of ForDeclaration also occurs in the VarDeclaredNames of Statement.
// It is a Syntax Error if the BoundNames of ForDeclaration contains any duplicate entries.
// In strict mode code, functions can only be declared at top level or inside a block
//
// ImportDeclaration:
// - It is a Syntax Error if the BoundNames of ImportDeclaration contains any duplicate entries.
//
// WithClause:
// - It is a Syntax Error if WithClauseToAttributes of WithClause has two different entries a and b such that a.[[Key]] is b.[[Key]].
//
// ExportDeclaration:
// - It is a Syntax Error if the ExportedNames of ModuleItemList contains any duplicate entries.
// - For each IdentifierName n in the ReferencedBindings of NamedExports:
//   It is a Syntax Error if the StringValue of n is a ReservedWord or the StringValue of n is one of
//   "implements", "interface", "let", "package", "private", "protected", "public", or "static".
//   Note: This is already checked in parser during export parsing for local exports without 'from'.
//
// module-level semantic checks:
// - It is a Syntax Error if the LexicallyDeclaredNames of ModuleItemList contains any duplicate entries.
// - It is a Syntax Error if any element of the LexicallyDeclaredNames of ModuleItemList also occurs in the VarDeclaredNames of ModuleItemList.
// - It is a Syntax Error if the ExportedBindings of ModuleItemList does not also occur in the VarDeclaredNames of ModuleItemList,
//   or the LexicallyDeclaredNames of ModuleItemList, or the ImportedLocalNames of ModuleItemList.
// - It is a Syntax Error if ModuleItemList Contains super.
// - It is a Syntax Error if ModuleItemList Contains NewTarget (except in functions).
// 'let' is reserved in strict mode code.
// export statements cannot be outside of a module.
// 'default' case cannot appear more than once in a switch statement.
// for-in/of loop variable declaration may not have an initializer
