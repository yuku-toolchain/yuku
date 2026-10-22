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

/// Section counts.
pub const SubHeader = extern struct {
    scope_count: u32,
    symbol_count: u32,
    reference_count: u32,
    decl_node_count: u32,
    import_count: u32,
    export_count: u32,
    /// One ScopeId per node.
    node_scope_count: u32,
    /// Raw `module_record.Flags` bitset.
    module_flags: u32,
};

/// `bits` packs kind (low 8 bits) and the strict flag (bit 8).
pub const PackedScope = extern struct {
    node: u32,
    parent: u32,
    hoist_target: u32,
    bits: u32,
    /// See `Semantic.next_bodies`.
    next_body: u32,
};

pub const PackedSymbol = extern struct {
    name_start: u32,
    name_end: u32,
    /// Raw `Symbol.Flags` bitset.
    flags: u32,
    scope: u32,
    decls_start: u32,
    decls_len: u32,
};

/// `bits` is the raw `Reference.Flags` bitset. `symbol` is the resolved SymbolId or the
/// none sentinel.
pub const PackedReference = extern struct {
    name_start: u32,
    name_end: u32,
    scope: u32,
    node: u32,
    bits: u32,
    symbol: u32,
};

/// `bits` packs kind (bits 0-2), type_only (bit 3), has_phase (bit 4), and phase
/// (bit 5, 0 source, 1 defer).
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

/// `bits` packs kind (bits 0-2) and type_only (bit 3).
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
pub const REFERENCE_WRITE_BIT: u5 = 0;
pub const REFERENCE_SPACE_SHIFT: u5 = 1;
pub const REFERENCE_SPACE_MASK: u32 = 0b111;
pub const REFERENCE_TYPE_POSITION_BIT: u5 = 4;
pub const IMPORT_KIND_MASK: u32 = 0b111;
pub const IMPORT_TYPE_BIT: u5 = 3;
pub const IMPORT_HAS_PHASE_BIT: u5 = 4;
pub const IMPORT_PHASE_BIT: u5 = 5;
pub const EXPORT_KIND_MASK: u32 = 0b111;
pub const EXPORT_TYPE_BIT: u5 = 3;

comptime {
    // whole u32s, so no padding bytes leak
    std.debug.assert(@sizeOf(SubHeader) == 8 * 4);
    std.debug.assert(@sizeOf(PackedScope) == 5 * 4);
    std.debug.assert(@sizeOf(PackedSymbol) == 6 * 4);
    std.debug.assert(@sizeOf(PackedReference) == 6 * 4);
    std.debug.assert(@sizeOf(PackedImport) == 8 * 4);
    std.debug.assert(@sizeOf(PackedExport) == 10 * 4);

    // the decoder reads every none id as -1
    std.debug.assert(@backingInt(semantic.ScopeId.none) == std.math.maxInt(u32));
    std.debug.assert(@backingInt(semantic.SymbolId.none) == std.math.maxInt(u32));
    std.debug.assert(@backingInt(ast.NodeIndex.null) == std.math.maxInt(u32));

    // raw values on the wire, and the public BindingFlags
    std.debug.assert(@bitSizeOf(Symbol.Flags) == 32);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "function_scoped_var") == 0);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "block_scoped_var") == 1);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "function") == 2);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "class") == 3);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "regular_enum") == 4);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "const_enum") == 5);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "value_module") == 6);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "interface") == 7);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "type_alias") == 8);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "type_parameter") == 9);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "namespace_module") == 10);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "import") == 11);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "type_import") == 12);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "const_var") == 13);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "ambient") == 14);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "parameter") == 15);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "catch_var") == 16);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "exported") == 17);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "is_default") == 18);
    std.debug.assert(@bitOffsetOf(Symbol.Flags, "enum_member") == 19);

    std.debug.assert(@bitSizeOf(Reference.Flags) == 8);
    std.debug.assert(@bitOffsetOf(Reference.Flags, "write") == REFERENCE_WRITE_BIT);
    std.debug.assert(@bitOffsetOf(Reference.Flags, "space") == REFERENCE_SPACE_SHIFT);
    std.debug.assert(
        @bitOffsetOf(Reference.Flags, "type_position") == REFERENCE_TYPE_POSITION_BIT,
    );
    std.debug.assert(@backingInt(Reference.Space.value) == 0);
    std.debug.assert(@backingInt(Reference.Space.type) == 1);
    std.debug.assert(@backingInt(Reference.Space.namespace) == 2);
    std.debug.assert(@backingInt(Reference.Space.typeof) == 3);
    std.debug.assert(@backingInt(Reference.Space.any) == 4);

    std.debug.assert(@backingInt(Scope.Kind.global) == 0);
    std.debug.assert(@backingInt(Scope.Kind.module) == 1);
    std.debug.assert(@backingInt(Scope.Kind.function) == 2);
    std.debug.assert(@backingInt(Scope.Kind.block) == 3);
    std.debug.assert(@backingInt(Scope.Kind.class) == 4);
    std.debug.assert(@backingInt(Scope.Kind.static_block) == 5);
    std.debug.assert(@backingInt(Scope.Kind.expression_name) == 6);
    std.debug.assert(@backingInt(Scope.Kind.ts_module) == 7);
    std.debug.assert(@backingInt(Scope.Kind.function_body) == 8);

    std.debug.assert(@backingInt(module_record.Import.Kind.named) == 0);
    std.debug.assert(@backingInt(module_record.Import.Kind.namespace) == 1);
    std.debug.assert(@backingInt(module_record.Import.Kind.side_effect) == 2);
    std.debug.assert(@backingInt(module_record.Import.Kind.import_equals) == 3);
    std.debug.assert(@backingInt(module_record.Import.Kind.dynamic) == 4);
    std.debug.assert(@backingInt(module_record.Import.Kind.require) == 5);
    std.debug.assert(@backingInt(module_record.Import.Kind.augmentation) == 6);
    std.debug.assert(@backingInt(module_record.Export.Kind.named) == 0);
    std.debug.assert(@backingInt(module_record.Export.Kind.re_export) == 1);
    std.debug.assert(@backingInt(module_record.Export.Kind.namespace) == 2);
    std.debug.assert(@backingInt(module_record.Export.Kind.star) == 3);
    std.debug.assert(@backingInt(module_record.Export.Kind.equals) == 4);
    std.debug.assert(@backingInt(module_record.Export.Kind.global) == 5);

    std.debug.assert(@bitSizeOf(module_record.Flags) == 8);
    std.debug.assert(@bitOffsetOf(module_record.Flags, "uses_require") == 0);
    std.debug.assert(@bitOffsetOf(module_record.Flags, "uses_module") == 1);
    std.debug.assert(@bitOffsetOf(module_record.Flags, "uses_exports") == 2);
    std.debug.assert(@bitOffsetOf(module_record.Flags, "uses_import_meta") == 3);
}

pub fn bufferSize(tree: *const ast.Tree, sem: *const Semantic, records: Records) usize {
    return std.mem.alignForward(usize, transfer.bufferSize(tree), 4) + @sizeOf(SubHeader) +
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
) usize {
    std.debug.assert(buf.len >= bufferSize(tree, sem, records));
    std.debug.assert(sem.next_bodies.len == sem.scopes.list.len);

    const base = transfer.serializeInto(tree, offsets, buf);

    const flags_offset = @offsetOf(transfer.Header, "flags");
    const flags = std.mem.readInt(u32, buf[flags_offset..][0..4], .little);
    std.mem.writeInt(u32, buf[flags_offset..][0..4], flags | transfer.FLAG_SEMANTIC, .little);

    var pos = std.mem.alignForward(usize, base, 4);
    @memset(buf[base..pos], 0);

    const sub: SubHeader = .{
        .scope_count = @intCast(sem.scopes.list.len),
        .symbol_count = @intCast(sem.symbols.len),
        .reference_count = @intCast(sem.references.len),
        .decl_node_count = @intCast(sem.decl_nodes.len),
        .import_count = @intCast(records.imports.len),
        .export_count = @intCast(records.exports.len),
        .node_scope_count = @intCast(sem.node_scopes.len),
        .module_flags = @backingInt(records.flags),
    };
    transfer.writeBytes(buf, &pos, std.mem.asBytes(&sub));

    for (sem.scopes.list, sem.next_bodies) |scope, next_body| {
        const entry: PackedScope = .{
            .node = @backingInt(scope.node),
            .parent = @backingInt(scope.parent),
            .hoist_target = @backingInt(scope.hoist_target),
            .bits = @as(u32, @backingInt(scope.kind)) |
                (@as(u32, @intFromBool(scope.flags.strict)) << SCOPE_STRICT_BIT),
            .next_body = @backingInt(next_body),
        };
        transfer.writeBytes(buf, &pos, std.mem.asBytes(&entry));
    }

    std.debug.assert(sem.decl_ranges.len == sem.symbols.len);
    for (sem.symbols, sem.decl_ranges) |symbol, decls| {
        const name = transfer.stringIn(offsets, symbol.name);
        const entry: PackedSymbol = .{
            .name_start = name.start,
            .name_end = name.end,
            .flags = @backingInt(symbol.flags),
            .scope = @backingInt(symbol.scope),
            .decls_start = decls.start,
            .decls_len = decls.len,
        };
        transfer.writeBytes(buf, &pos, std.mem.asBytes(&entry));
    }

    transfer.writeBytes(buf, &pos, std.mem.sliceAsBytes(sem.decl_nodes));

    for (sem.references) |reference| {
        const name = transfer.stringIn(offsets, reference.name);
        const entry: PackedReference = .{
            .name_start = name.start,
            .name_end = name.end,
            .scope = @backingInt(reference.scope),
            .node = @backingInt(reference.node),
            .bits = @backingInt(reference.flags),
            .symbol = @backingInt(reference.symbol),
        };
        transfer.writeBytes(buf, &pos, std.mem.asBytes(&entry));
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
        const entry: PackedImport = .{
            .symbol = @backingInt(record.symbol),
            .bits = bits,
            .name_start = name.start,
            .name_end = name.end,
            .specifier_start = specifier.start,
            .specifier_end = specifier.end,
            .node = @backingInt(record.node),
            .scope = @backingInt(record.scope),
        };
        transfer.writeBytes(buf, &pos, std.mem.asBytes(&entry));
    }

    for (records.exports) |record| {
        const name = transfer.stringIn(offsets, record.name);
        const from_name = transfer.stringIn(offsets, record.from_name);
        const specifier = transfer.stringIn(offsets, record.specifier);
        const entry: PackedExport = .{
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
        };
        transfer.writeBytes(buf, &pos, std.mem.asBytes(&entry));
    }

    transfer.writeBytes(buf, &pos, std.mem.sliceAsBytes(sem.node_scopes));

    std.debug.assert(pos == bufferSize(tree, sem, records));
    return pos;
}
