const std = @import("std");
const ast = @import("../ast.zig");
const wk = @import("walk.zig");
const sc = @import("tracker/scope.zig");
const sy = @import("tracker/symbol.zig");

const Allocator = std.mem.Allocator;

pub const Scope = sc.Scope;
pub const ScopeId = sc.ScopeId;
pub const ScopeTree = sc.ScopeTree;
pub const ScopeTracker = sc.ScopeTracker;
pub const SymbolId = sy.SymbolId;
pub const ReferenceId = sy.ReferenceId;
pub const Symbol = sy.Symbol;
pub const Reference = sy.Reference;
pub const SymbolTable = sy.SymbolTable;
pub const SymbolTracker = sy.SymbolTracker;

/// Walk context combining the path stack, scope tracker, and symbol
/// tracker. Visitor hooks receive `*Ctx` and use it to inspect the
/// surrounding source structure (`scope`, `path`, `inTypePosition`) or
/// to query and contribute symbols (`symbols`).
pub const Ctx = struct {
    tree: *const ast.Tree,
    path: wk.NodePath = .{},
    scope: ScopeTracker,
    symbols: SymbolTracker,
    /// Depth of TS type-only context. Incremented on enter into a
    /// type-context node, decremented on exit. Inspect via
    /// `inTypePosition()`.
    type_position_depth: u32 = 0,

    pub fn init(tree: *ast.Tree) Allocator.Error!Ctx {
        return .{
            .tree = tree,
            .scope = try ScopeTracker.init(tree),
            .symbols = try SymbolTracker.init(tree),
        };
    }

    /// True when the walker is currently inside a TS type-only subtree.
    /// Visitors should consult this rather than reading
    /// `type_position_depth` directly.
    pub inline fn inTypePosition(self: *const Ctx) bool {
        return self.type_position_depth > 0;
    }

    pub fn enter(self: *Ctx, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        self.path.push(index);
        try self.scope.enter(index, data);
        if (isTypeContextNode(data)) self.type_position_depth += 1;
        self.symbols.setBindingContext(data, &self.scope);
    }

    pub fn post_enter(self: *Ctx, index: ast.NodeIndex, data: ast.NodeData) Allocator.Error!void {
        _ = try self.symbols.declareBindings(index, data, &self.scope, self.inTypePosition());
    }

    pub fn exit(self: *Ctx, data: ast.NodeData) void {
        self.symbols.exit(data);
        self.scope.exit(data);
        if (isTypeContextNode(data)) self.type_position_depth -= 1;
        self.path.pop();
    }
};

/// TS nodes whose children are all type-side
fn isTypeContextNode(data: ast.NodeData) bool {
    return switch (data) {
        .ts_type_annotation,
        .ts_type_reference,
        .ts_qualified_name,
        .ts_type_query,
        .ts_import_type,
        .ts_type_parameter,
        .ts_type_parameter_declaration,
        .ts_type_parameter_instantiation,
        .ts_literal_type,
        .ts_template_literal_type,
        .ts_array_type,
        .ts_indexed_access_type,
        .ts_tuple_type,
        .ts_named_tuple_member,
        .ts_optional_type,
        .ts_rest_type,
        .ts_jsdoc_nullable_type,
        .ts_jsdoc_non_nullable_type,
        .ts_jsdoc_unknown_type,
        .ts_union_type,
        .ts_intersection_type,
        .ts_conditional_type,
        .ts_infer_type,
        .ts_type_operator,
        .ts_parenthesized_type,
        .ts_function_type,
        .ts_constructor_type,
        .ts_type_predicate,
        .ts_type_literal,
        .ts_property_signature,
        .ts_method_signature,
        .ts_call_signature_declaration,
        .ts_construct_signature_declaration,
        .ts_index_signature,
        .ts_mapped_type,
        .ts_class_implements,
        .ts_interface_heritage,
        .ts_interface_body,
        => true,
        else => false,
    };
}

/// Combined output of a semantic traversal. Holds the scope tree and
/// the symbol table.
pub const Result = struct {
    scope_tree: ScopeTree,
    symbol_table: SymbolTable,
};

/// Walks the tree with full path, scope, and symbol tracking. Returns
/// the scope tree and the unresolved symbol table. Call
/// `SymbolTable.resolveAll` to build the reference cross-index.
pub fn traverse(comptime V: type, tree: *ast.Tree, visitor: *V) Allocator.Error!Result {
    var ctx = try Ctx.init(tree);
    var layer = wk.Layer(Ctx, V){ .inner = visitor };
    try wk.walk(Ctx, wk.Layer(Ctx, V), &layer, &ctx);
    return .{
        .scope_tree = ctx.scope.toScopeTree(),
        .symbol_table = ctx.symbols.toSymbolTable(),
    };
}
