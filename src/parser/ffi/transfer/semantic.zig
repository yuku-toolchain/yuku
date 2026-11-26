// the analyzer buffer is the AST buffer with FLAG_SEMANTIC set, then these tables after the
// diagnostics, aligned to 4 bytes, without use lists since the decoder derives them
//
//   sub-header   one SubHeader
//   scopes       scope_count PackedScope
//   symbols      symbol_count PackedSymbol
//   decl_nodes   decl_node_count u32, the NodeIndex array behind `PackedSymbol.decls_*`
//   references   reference_count PackedReference, each resolved
//   imports      import_count PackedImport
//   exports      export_count PackedExport
//   node_scopes  node_scope_count u32, the ScopeId of each node

const std = @import("std");
const parser = @import("parser");
const transfer = @import("root.zig");

const ast = parser.ast;
const semantic = parser.semantic;
const module_record = parser.semantic.module_record;

const Scope = semantic.Scope;
const Symbol = semantic.Symbol;
const Reference = semantic.Reference;
const Semantic = semantic.Semantic;
const Records = module_record.Records;

pub const SubHeader = extern struct {
    scope_count: u32,
    symbol_count: u32,
    reference_count: u32,
    decl_node_count: u32,
    import_count: u32,
    export_count: u32,
    node_scope_count: u32,
    module_flags: u32,
};

pub const PackedScope = extern struct {
    node: u32,
    parent: u32,
    hoist_target: u32,
    bits: u32,
    next_body: u32,
};

pub const PackedSymbol = extern struct {
    name_start: u32,
    name_end: u32,
    flags: u32,
    scope: u32,
    decls_start: u32,
    decls_len: u32,
};

pub const PackedReference = extern struct {
    name_start: u32,
    name_end: u32,
    scope: u32,
    node: u32,
    bits: u32,
    symbol: u32,
};

pub const PackedImport = extern struct {
    symbol: u32,
    bits: u32,
    name_start: u32,
    name_end: u32,
    specifier_start: u32,
    specifier_end: u32,
    node: u32,
    scope: u32,
};

pub const PackedExport = extern struct {
    bits: u32,
    name_start: u32,
    name_end: u32,
    symbol: u32,
    from_name_start: u32,
    from_name_end: u32,
    specifier_start: u32,
    specifier_end: u32,
    node: u32,
    reserved: u32 = 0,
};

pub const SCOPE_KIND_MASK: u32 = 0xFF;
pub const SCOPE_STRICT_BIT: u5 = 8;
pub const REFERENCE_WRITE_BIT: u5 = @bitOffsetOf(Reference.Flags, "write");
pub const REFERENCE_SPACE_SHIFT: u5 = @bitOffsetOf(Reference.Flags, "space");
pub const REFERENCE_SPACE_MASK: u32 = (1 << @bitSizeOf(Reference.Space)) - 1;
pub const REFERENCE_TYPE_POSITION_BIT: u5 = @bitOffsetOf(Reference.Flags, "type_position");
pub const IMPORT_KIND_MASK: u32 = 0b111;
pub const IMPORT_TYPE_BIT: u5 = 3;
pub const IMPORT_HAS_PHASE_BIT: u5 = 4;
pub const IMPORT_PHASE_BIT: u5 = 5;
pub const EXPORT_KIND_MASK: u32 = 0b111;
pub const EXPORT_TYPE_BIT: u5 = 3;

comptime {
    // whole u32s, so no padding bytes leak
    for (.{
        SubHeader, PackedScope, PackedSymbol, PackedReference, PackedImport, PackedExport,
    }) |T| std.debug.assert(@sizeOf(T) == 4 * @typeInfo(T).@"struct".field_names.len);

    // the decoder reads every none id as -1
    for (.{ semantic.ScopeId.none, semantic.SymbolId.none, ast.NodeIndex.null }) |none| {
        std.debug.assert(@backingInt(none) == std.math.maxInt(u32));
    }

    assertBits(Symbol.Flags, &.{
        "function_scoped_var", "block_scoped_var", "function",         "class",
        "regular_enum",        "const_enum",       "value_module",     "interface",
        "type_alias",          "type_parameter",   "namespace_module", "import",
        "type_import",         "const_var",        "ambient",          "parameter",
        "catch_var",           "exported",         "is_default",       "enum_member",
    });
    assertBits(module_record.Flags, &.{
        "uses_require", "uses_module", "uses_exports", "uses_import_meta",
    });
    assertValues(Reference.Space, &.{ .value, .type, .namespace, .typeof, .any });
    assertValues(Scope.Kind, &.{
        .global,       .module,          .function,  .block,         .class,
        .static_block, .expression_name, .ts_module, .function_body,
    });
    assertValues(module_record.Import.Kind, &.{
        .named, .namespace, .side_effect, .import_equals, .dynamic, .require, .augmentation,
    });
    assertValues(module_record.Export.Kind, &.{
        .named, .re_export, .namespace, .star, .equals, .global,
    });
}

fn assertBits(comptime T: type, comptime fields: []const []const u8) void {
    for (fields, 0..) |field, bit| std.debug.assert(@bitOffsetOf(T, field) == bit);
}

fn assertValues(comptime E: type, comptime members: []const E) void {
    for (members, 0..) |member, value| std.debug.assert(@backingInt(member) == value);
}

pub fn bufferSize(
    tree: *const ast.Tree,
    sem: *const Semantic,
    records: Records,
    sections: transfer.Sections,
) usize {
    return std.mem.alignForward(usize, transfer.bufferSize(tree, sections), 4) +
        @sizeOf(SubHeader) +
        sem.scopes.list.len * @sizeOf(PackedScope) +
        sem.symbols.len * @sizeOf(PackedSymbol) +
        sem.decl_nodes.len * 4 +
        sem.references.len * @sizeOf(PackedReference) +
        records.imports.len * @sizeOf(PackedImport) +
        records.exports.len * @sizeOf(PackedExport) +
        sem.node_scopes.len * 4;
}

pub fn serializeInto(
    tree: *const ast.Tree,
    sem: *const Semantic,
    records: Records,
    offsets: *const transfer.utf16.Offsets,
    buf: []u8,
    sections: transfer.Sections,
) usize {
    std.debug.assert(buf.len >= bufferSize(tree, sem, records, sections));

    const base = transfer.serializeInto(tree, offsets, buf, sections);
    const header: *transfer.Header = @ptrCast(@alignCast(buf.ptr));
    header.flags |= transfer.FLAG_SEMANTIC;

    var pos = std.mem.alignForward(usize, base, 4);
    @memset(buf[base..pos], 0);

    write(buf, &pos, SubHeader{
        .scope_count = @intCast(sem.scopes.list.len),
        .symbol_count = @intCast(sem.symbols.len),
        .reference_count = @intCast(sem.references.len),
        .decl_node_count = @intCast(sem.decl_nodes.len),
        .import_count = @intCast(records.imports.len),
        .export_count = @intCast(records.exports.len),
        .node_scope_count = @intCast(sem.node_scopes.len),
        .module_flags = @backingInt(records.flags),
    });

    for (sem.scopes.list, sem.next_bodies) |scope, next_body| write(buf, &pos, PackedScope{
        .node = @backingInt(scope.node),
        .parent = @backingInt(scope.parent),
        .hoist_target = @backingInt(scope.hoist_target),
        .bits = @as(u32, @backingInt(scope.kind)) |
            (@as(u32, @intFromBool(scope.flags.strict)) << SCOPE_STRICT_BIT),
        .next_body = @backingInt(next_body),
    });

    for (sem.symbols, sem.decl_ranges) |symbol, decls| {
        const name = transfer.stringIn(offsets, symbol.name);
        write(buf, &pos, PackedSymbol{
            .name_start = name.start,
            .name_end = name.end,
            .flags = @backingInt(symbol.flags),
            .scope = @backingInt(symbol.scope),
            .decls_start = decls.start,
            .decls_len = decls.len,
        });
    }

    transfer.writeBytes(buf, &pos, std.mem.sliceAsBytes(sem.decl_nodes));

    for (sem.references) |reference| {
        const name = transfer.stringIn(offsets, reference.name);
        write(buf, &pos, PackedReference{
            .name_start = name.start,
            .name_end = name.end,
            .scope = @backingInt(reference.scope),
            .node = @backingInt(reference.node),
            .bits = @backingInt(reference.flags),
            .symbol = @backingInt(reference.symbol),
        });
    }

    for (records.imports) |record| {
        var bits: u32 = @backingInt(record.kind);
        bits |= @as(u32, @intFromBool(record.type_only)) << IMPORT_TYPE_BIT;
        if (record.phase) |phase| {
            bits |= @as(u32, 1) << IMPORT_HAS_PHASE_BIT;
            bits |= @as(u32, @backingInt(phase)) << IMPORT_PHASE_BIT;
        }
        const name = transfer.stringIn(offsets, record.name);
        const specifier = transfer.stringIn(offsets, record.specifier);
        write(buf, &pos, PackedImport{
            .symbol = @backingInt(record.symbol),
            .bits = bits,
            .name_start = name.start,
            .name_end = name.end,
            .specifier_start = specifier.start,
            .specifier_end = specifier.end,
            .node = @backingInt(record.node),
            .scope = @backingInt(record.scope),
        });
    }

    for (records.exports) |record| {
        const name = transfer.stringIn(offsets, record.name);
        const from_name = transfer.stringIn(offsets, record.from_name);
        const specifier = transfer.stringIn(offsets, record.specifier);
        write(buf, &pos, PackedExport{
            .bits = @as(u32, @backingInt(record.kind)) |
                (@as(u32, @intFromBool(record.type_only)) << EXPORT_TYPE_BIT),
            .name_start = name.start,
            .name_end = name.end,
            .symbol = @backingInt(record.symbol),
            .from_name_start = from_name.start,
            .from_name_end = from_name.end,
            .specifier_start = specifier.start,
            .specifier_end = specifier.end,
            .node = @backingInt(record.node),
        });
    }

    transfer.writeBytes(buf, &pos, std.mem.sliceAsBytes(sem.node_scopes));

    std.debug.assert(pos == bufferSize(tree, sem, records, sections));
    return pos;
}

inline fn write(buf: []u8, pos: *usize, entry: anytype) void {
    transfer.writeBytes(buf, pos, std.mem.asBytes(&entry));
}
