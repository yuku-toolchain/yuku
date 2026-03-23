// this is wip, even though redeclaration checks and some of the most important semantic errors are done,
// there are still more semantic errors to cover (but lower priority for now)

const std = @import("std");
const traverser = @import("traverser/root.zig");
const ast = @import("ast.zig");
const ecmascript = @import("ecmascript.zig");

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
        const name = ctx.tree.getString(id.name);

        if (!ctx.scope.isStrict() and
            ctx.symbols.currentBindingKind() == .lexical and !ctx.symbols.binding_is_const and
            std.mem.eql(u8, name, "let"))
        {
            try self.report(ctx.tree.getSpan(node_index), "`let` cannot be declared as a variable name inside of a `let` declaration", .{});
        }

        const target = ctx.symbols.resolveTargetScope(&ctx.scope);

        if (ctx.symbols.findInScope(target, name)) |sym| {
            const existing = ctx.symbols.getSymbol(sym);
            const current_kind = ctx.symbols.currentBindingKind();

            // Section 14.2.1:  "It is a Syntax Error if the LexicallyDeclaredNames
            //                   of StatementList contains any duplicate entries."
            // Section 16.1.4:  "It is a Syntax Error if any element of the
            //                   LexicallyDeclaredNames ... also occurs in the
            //                   VarDeclaredNames ..."
            if (existing.kind.isLexical() or current_kind.isLexical()) {
                try self.reportRedeclaration(id, node_index, existing, ctx);

                return .proceed;
            }

            if (existing.kind == .parameter) {
                // the existing binding is a parameter, so find up for the formal_parameters
                // which this parameter belongs to
                if (findFormalParameters(ctx)) |formal_parameters| {
                    // UniqueFormalParameters : FormalParameters
                    //  - It is a Syntax Error if the BoundNames of FormalParameters contains any duplicate elements.
                    //
                    // Multiple occurrences of the same BindingIdentifier in a FormalParameterList is only allowed for
                    // functions which have simple parameter lists and which are not defined in strict mode code.
                    if (
                        ctx.scope.isStrict() or
                        formal_parameters.kind == .unique_formal_parameters or
                        formal_parameters.kind == .arrow_formal_parameters
                    ) {
                        try self.reportRedeclaration(id, node_index, existing, ctx);
                    }

                    // FormalParameters : FormalParameterList
                    //  - It is a Syntax Error if IsSimpleParameterList of FormalParameterList is false and the BoundNames
                    //    of FormalParameterList contains any duplicate elements.
                    else if (ecmascript.isSimpleParameterList(ctx.tree, formal_parameters)) {
                        try self.reportRedeclaration(id, node_index, existing, ctx);
                    }
                }
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

    /// https://tc39.es/ecma262/#sec-class-definitions-static-semantics-early-errors
    pub fn enter_call_expression(self: *Self, expr: ast.CallExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.tree.getData(expr.callee) == .super) {
            switch (superCallValidity(ctx)) {
                .valid => {},
                // ClassElement : MethodDefinition
                //   It is a Syntax Error if PropName of MethodDefinition is not "constructor"
                //   and HasDirectSuper of MethodDefinition is true.
                // FieldDefinition : ClassElementName Initializer?
                //   It is a Syntax Error if Initializer is present and Initializer Contains SuperCall is true.
                // ClassStaticBlockBody : ClassStaticBlockStatementList
                //   It is a Syntax Error if ClassStaticBlockStatementList Contains SuperCall is true.
                .not_in_constructor => try self.report(ctx.tree.getSpan(node_index), "'super()' is only valid in a constructor of a derived class", .{
                    .help = "Use an arrow function instead of a regular function to inherit the 'super' binding",
                }),
                // ClassTail : ClassHeritage? { ClassBody }
                //   It is a Syntax Error if ClassHeritage is not present and the following
                //   algorithm returns true:
                //     1. Let constructor be ConstructorMethod of ClassBody.
                //     2. If constructor is empty, return false.
                //     3. Return HasDirectSuper of constructor.
                .no_extends => try self.report(ctx.tree.getSpan(node_index), "'super()' is only valid in a constructor of a derived class", .{
                    .help = "Add an 'extends' clause to the class or remove the 'super()' call",
                }),
            }
        }
        return .proceed;
    }

    /// Section 15.2.1, 15.5.1, 15.6.1, 15.8.1:
    ///   It is a Syntax Error if FormalParameters/FunctionBody Contains SuperProperty is true.
    /// Section 16.1.2.1 / 16.2.1.1:
    ///   It is a Syntax Error if StatementList/ModuleItemList Contains super.
    pub fn enter_member_expression(self: *Self, expr: ast.MemberExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (ctx.tree.getData(expr.object) == .super) {
            if (!isSuperPropertyValid(ctx)) {
                try self.report(ctx.tree.getSpan(node_index), "'super' property access is only valid inside a method or class body", .{
                    .help = "Use an arrow function instead of a regular function to inherit the 'super' binding",
                });
            }
        }
        return .proceed;
    }

    pub fn enter_unary_expression(self: *Self, expr: ast.UnaryExpression, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (expr.operator == .delete) {
            const target = unwrapParens(ctx.tree, expr.argument);
            switch (ctx.tree.getData(target)) {
                .member_expression => |m| if (!m.computed and ctx.tree.getData(m.property) == .private_identifier) {
                    try self.report(ctx.tree.getSpan(node_index), "Private fields cannot be deleted", .{});
                },
                else => {},
            }
        }
        return .proceed;
    }

    /// Section 14.9.2: It is a Syntax Error if this BreakStatement is not nested,
    /// directly or indirectly (but not crossing function or static initialization
    /// block boundaries), within an IterationStatement or a SwitchStatement.
    pub fn enter_break_statement(self: *Self, stmt: ast.BreakStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (stmt.label != .null) {
            const label_name = ctx.tree.getString(ctx.tree.getData(stmt.label).label_identifier.name);
            switch (findLabel(ctx, label_name)) {
                .found => {},
                .not_found => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Use of undefined label `{s}`", .{label_name}), .{}),
                .crossed_boundary => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Cannot break to label `{s}` across function boundaries", .{label_name}), .{}),
            }
        } else {
            if (!isInsideBreakable(ctx)) {
                try self.report(ctx.tree.getSpan(node_index), "Illegal break statement", .{
                    .help = "A `break` statement can only be used within an enclosing iteration or switch statement",
                });
            }
        }
        return .proceed;
    }

    /// Section 14.8.2: It is a Syntax Error if this ContinueStatement is not nested,
    /// directly or indirectly (but not crossing function or static initialization
    /// block boundaries), within an IterationStatement.
    pub fn enter_continue_statement(self: *Self, stmt: ast.ContinueStatement, node_index: ast.NodeIndex, ctx: *SemanticCtx) AnalysisError!Action {
        if (stmt.label != .null) {
            const label_name = ctx.tree.getString(ctx.tree.getData(stmt.label).label_identifier.name);
            switch (findLabelForContinue(ctx, label_name)) {
                .found => {},
                .not_found => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Use of undefined label `{s}`", .{label_name}), .{}),
                .crossed_boundary => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Cannot continue to label `{s}` across function boundaries", .{label_name}), .{}),
                .not_iteration => try self.report(ctx.tree.getSpan(node_index), try self.fmt("Label `{s}` does not denote an iteration statement", .{label_name}), .{
                    .help = "A `continue` statement can only jump to a label of an enclosing `for`, `while`, `do-while`, `for-in`, or `for-of` statement",
                }),
            }
        } else {
            if (!isInsideIteration(ctx)) {
                try self.report(ctx.tree.getSpan(node_index), "Illegal continue statement", .{
                    .help = "A `continue` statement can only be used within an enclosing iteration statement",
                });
            }
        }
        return .proceed;
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

    const SuperCallValidity = enum { valid, not_in_constructor, no_extends };

    /// determines if a `super()` call is in a valid position.
    ///
    /// `super()` is only permitted directly inside a constructor of a derived
    /// class (one with an `extends` clause).
    ///
    /// arrow functions are transparent for `super`, Section 8.5.1 defines that
    /// `Contains` passes through arrow functions for `SuperCall`.
    /// Regular functions are opaque boundaries, they always return `false`
    /// for `Contains`.
    fn superCallValidity(ctx: *SemanticCtx) SuperCallValidity {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.getData(i)) {
                // Section 8.5.1: ArrowFunction does not close over `SuperCall`,
                // `Contains` passes through to the enclosing scope.
                .arrow_function_expression => {},

                // Section 8.5.1: Regular functions are opaque to `Contains`.
                // The only valid case is the function that is the *value* of
                // a constructor MethodDefinition in a class with `extends`.
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

                // Section 15.7.1: FieldDefinition, SuperCall in Initializer is a Syntax Error.
                // Section 15.7.1: ClassStaticBlockBody, SuperCall in StatementList is a Syntax Error.
                .property_definition, .static_block => return .not_in_constructor,

                .program => return .not_in_constructor,
                else => {},
            }
        }
        return .not_in_constructor;
    }

    /// determines if a `super.property` access is in a valid position.
    ///
    /// SuperProperty is more permissive than SuperCall. It is valid inside:
    /// - any class method (constructor, regular, getter, setter, static)
    /// - object literal methods/getters/setters
    /// - class field initializers and static blocks
    /// - arrow functions inheriting from the above
    ///
    /// it is not valid in standalone functions or top-level code.
    fn isSuperPropertyValid(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            switch (ctx.tree.getData(i)) {
                // Section 8.5.1: Arrow functions are transparent for SuperProperty.
                .arrow_function_expression => {},

                // Section 8.5.1: regular functions are opaque to `Contains`.
                // valid only if this function is the value of a class method
                // or object literal method/getter/setter.
                .function => {
                    if (iter.next()) |parent| {
                        const data = ctx.tree.getData(parent);
                        // class method (any kind, constructor, method, get, set)
                        if (data == .method_definition) return true;
                        // object literal method/getter/setter
                        if (data == .object_property) {
                            const prop = data.object_property;
                            if (prop.method or prop.kind != .init) return true;
                        }
                    }

                    return false;
                },

                // SuperProperty is valid in field initializers and static blocks
                // (unlike SuperCall which is banned here).
                .property_definition, .static_block => return true,

                .program => return false,
                else => {},
            }
        }
        return false;
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

    /// check if we're inside a loop or switch (for unlabeled `break`).
    fn isInsideBreakable(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.getData(i);
            if (isIterationStatement(data) or data == .switch_statement) return true;
            if (isFunctionBoundary(data)) return false;
        }
        return false;
    }

    /// check if we're inside a loop (for unlabeled `continue`).
    fn isInsideIteration(ctx: *SemanticCtx) bool {
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.getData(i);
            if (isIterationStatement(data)) return true;
            if (isFunctionBoundary(data)) return false;
        }
        return false;
    }

    const LabelSearch = enum { found, not_found, crossed_boundary };

    /// find a matching labeled statement for `break label;`.
    fn findLabel(ctx: *SemanticCtx, name: []const u8) LabelSearch {
        var crossed_boundary = false;
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.getData(i);
            if (data == .labeled_statement) {
                const lbl_name = ctx.tree.getString(ctx.tree.getData(data.labeled_statement.label).label_identifier.name);
                if (std.mem.eql(u8, lbl_name, name))
                    return if (crossed_boundary) .crossed_boundary else .found;
            }
            if (isFunctionBoundary(data)) crossed_boundary = true;
        }
        return .not_found;
    }

    const ContinueLabelSearch = enum { found, not_found, crossed_boundary, not_iteration };

    /// find a matching labeled statement for `continue label;`.
    /// the label must directly wrap an iteration statement (or another label).
    fn findLabelForContinue(ctx: *SemanticCtx, name: []const u8) ContinueLabelSearch {
        var crossed_boundary = false;
        var iter = ctx.path.ancestors();
        while (iter.next()) |i| {
            const data = ctx.tree.getData(i);
            if (data == .labeled_statement) {
                const ls = data.labeled_statement;
                const lbl_name = ctx.tree.getString(ctx.tree.getData(ls.label).label_identifier.name);
                if (std.mem.eql(u8, lbl_name, name)) {
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
// It is a Syntax Error if FunctionBody Contains SuperProperty is true.
// It is a Syntax Error if FormalParameters Contains SuperProperty is true.
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
