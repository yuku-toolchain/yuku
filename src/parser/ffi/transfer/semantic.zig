// Semantic transfer sections, the analyzer buffer format.
//
// `serializeInto` writes the standard AST buffer via the core format,
// sets FLAG_SEMANTIC in its header, then appends the semantic tables.
// Decoders that do not know the flag stop after diagnostics and never
// see the extra bytes.
//
// Sections, in order, starting at the first 4-byte-aligned offset after
// the diagnostics section (zero padding fills the gap so every table
// can be read through a plain Uint32Array view):
//
//   sub-header   SUBHEADER_SIZE bytes. Section counts, see SubHeader.
//   scopes       scope_count * SCOPE_SIZE.       One PackedScope each.
//   symbols      symbol_count * SYMBOL_SIZE.     One PackedSymbol each.
//   decl_nodes   decl_node_count * 4.            Flat NodeIndex array
//                backing `PackedSymbol.decls_start/len` ranges.
//   references   reference_count * REFERENCE_SIZE. One PackedReference
//                each, resolution folded in. Requires `resolveAll`.
//   imports      import_count * IMPORT_SIZE.     One PackedImport each.
//   exports      export_count * EXPORT_SIZE.     One PackedExport each.
//   node_scopes  node_scope_count * 4.           One ScopeId per node,
//                indexed by node index. The node's lexical scope.
//
// Sentinels: ScopeId.none, SymbolId.none, and NodeIndex.null all encode
// as 0xFFFFFFFF. String handles are (start, end) pairs resolved against
// the source/pool sections of the core buffer, exactly like node string
// fields. Per-symbol reference lists and per-scope binding maps are NOT
// shipped: the decoder derives them in one pass over `references` and
// `symbols`, which keeps the wire minimal and the producer simple.

const std = @import("std");
const parser = @import("parser");
const transfer = @import("root.zig");

const ast = parser.ast;
const semantic = parser.traverser.semantic;
const module_record = parser.semantic.module_record;

const Scope = semantic.Scope;
const Symbol = semantic.Symbol;
const Result = semantic.Result;
const ModuleRecords = module_record.ModuleRecords;

/// fixed-size counts block. one reserved slot keeps room for future
/// sections without a layout break.
pub const SubHeader = extern struct {
    scope_count: u32,
    symbol_count: u32,
    reference_count: u32,
    decl_node_count: u32,
    import_count: u32,
    export_count: u32,
    /// one ScopeId per node, indexed by node index.
    node_scope_count: u32,
    reserved0: u32 = 0,
};

/// `bits` packs kind (low 8 bits) and the strict flag (bit 8).
pub const PackedScope = extern struct {
    node: u32,
    parent: u32,
    hoist_target: u32,
    bits: u32,
};

pub const PackedSymbol = extern struct {
    name_start: u32,
    name_end: u32,
    /// raw `Symbol.Flags` bitset, layout frozen by the comptime asserts
    /// below.
    flags: u32,
    scope: u32,
    decls_start: u32,
    decls_len: u32,
};

/// `bits` packs kind (bit 0: 0 value, 1 type) and is_write (bit 1).
/// `symbol` is the resolved SymbolId or the none sentinel.
pub const PackedReference = extern struct {
    name_start: u32,
    name_end: u32,
    scope: u32,
    node: u32,
    bits: u32,
    symbol: u32,
};

/// `bits` packs name_kind (bits 0-2), kind (bit 3: 0 value, 1 type),
/// has_phase (bit 4), and phase (bit 5: 0 source, 1 defer).
pub const PackedImport = extern struct {
    symbol: u32,
    bits: u32,
    name_start: u32,
    name_end: u32,
    specifier_start: u32,
    specifier_end: u32,
    node: u32,
    reserved: u32 = 0,
};

/// `bits` packs name_kind (bits 0-2), from_kind (bits 3-5), and kind
/// (bit 6: 0 value, 1 type).
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

// section entry sizes.
pub const SUBHEADER_SIZE: u32 = @sizeOf(SubHeader);
pub const SCOPE_SIZE: u32 = @sizeOf(PackedScope);
pub const SYMBOL_SIZE: u32 = @sizeOf(PackedSymbol);
pub const REFERENCE_SIZE: u32 = @sizeOf(PackedReference);
pub const IMPORT_SIZE: u32 = @sizeOf(PackedImport);
pub const EXPORT_SIZE: u32 = @sizeOf(PackedExport);

// bit positions inside the packed `bits` words, for the JS decoder.
pub const SCOPE_KIND_MASK: u32 = 0xFF;
pub const SCOPE_STRICT_BIT: u5 = 8;
pub const REFERENCE_TYPE_BIT: u5 = 0;
pub const REFERENCE_WRITE_BIT: u5 = 1;
pub const IMPORT_NAME_KIND_MASK: u32 = 0b111;
pub const IMPORT_TYPE_BIT: u5 = 3;
pub const IMPORT_HAS_PHASE_BIT: u5 = 4;
pub const IMPORT_PHASE_BIT: u5 = 5;
pub const EXPORT_NAME_KIND_MASK: u32 = 0b111;
pub const EXPORT_FROM_KIND_SHIFT: u5 = 3;
pub const EXPORT_TYPE_BIT: u5 = 6;

comptime {
    // every entry is whole u32s, no padding to leak
    std.debug.assert(SUBHEADER_SIZE == 8 * 4);
    std.debug.assert(SCOPE_SIZE == 4 * 4);
    std.debug.assert(SYMBOL_SIZE == 6 * 4);
    std.debug.assert(REFERENCE_SIZE == 6 * 4);
    std.debug.assert(IMPORT_SIZE == 8 * 4);
    std.debug.assert(EXPORT_SIZE == 10 * 4);

    // the symbol flags bitset crosses the wire as a raw u32, so its bit
    // layout is a contract with the JS SymbolFlags constants. freeze it.
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

    // scope kinds cross as raw u8 values inside `bits`, freeze the order
    std.debug.assert(@intFromEnum(Scope.Kind.global) == 0);
    std.debug.assert(@intFromEnum(Scope.Kind.module) == 1);
    std.debug.assert(@intFromEnum(Scope.Kind.function) == 2);
    std.debug.assert(@intFromEnum(Scope.Kind.block) == 3);
    std.debug.assert(@intFromEnum(Scope.Kind.class) == 4);
    std.debug.assert(@intFromEnum(Scope.Kind.static_block) == 5);
    std.debug.assert(@intFromEnum(Scope.Kind.expression_name) == 6);
    std.debug.assert(@intFromEnum(Scope.Kind.ts_module) == 7);

    // name kinds cross as raw bits inside the import/export `bits`
    // words, a contract with the JS NAME_KINDS table. freeze the order.
    std.debug.assert(@intFromEnum(module_record.NameKind.named) == 0);
    std.debug.assert(@intFromEnum(module_record.NameKind.star) == 1);
    std.debug.assert(@intFromEnum(module_record.NameKind.none) == 2);
    std.debug.assert(@intFromEnum(module_record.NameKind.equals) == 3);
    std.debug.assert(@intFromEnum(module_record.NameKind.global) == 4);
}

/// Total buffer size for the core AST sections plus the semantic
/// sections, including the alignment padding between them.
pub fn bufferSize(
    tree: *const ast.Tree,
    result: *const Result,
    records: ModuleRecords,
) usize {
    const base = transfer.bufferSize(tree);
    const aligned = std.mem.alignForward(usize, base, 4);
    return aligned + SUBHEADER_SIZE +
        result.scope_tree.scopes.len * SCOPE_SIZE +
        result.symbol_table.symbols.len * SYMBOL_SIZE +
        result.symbol_table.decl_nodes.len * 4 +
        result.symbol_table.references.len * REFERENCE_SIZE +
        records.imports.len * IMPORT_SIZE +
        records.exports.len * EXPORT_SIZE +
        result.node_scopes.len * 4;
}

/// Serializes the core AST buffer followed by the semantic sections.
/// `result.symbol_table` must have been resolved with `resolveAll` so
/// every reference carries its resolution. Returns bytes written.
pub fn serializeInto(
    tree: *const ast.Tree,
    result: *const Result,
    records: ModuleRecords,
    buf: []u8,
) usize {
    const table = &result.symbol_table;
    // resolveAll has run: one resolution per reference
    std.debug.assert(table.resolutions.len == table.references.len);
    std.debug.assert(buf.len >= bufferSize(tree, result, records));

    const base = transfer.serializeInto(tree, buf);

    // flip FLAG_SEMANTIC in the already-written core header
    const flags_offset = transfer.HDR_FLAGS_U32 * 4;
    const flags = std.mem.readInt(u32, buf[flags_offset..][0..4], .little);
    std.mem.writeInt(u32, buf[flags_offset..][0..4], flags | transfer.FLAG_SEMANTIC, .little);

    var pos = std.mem.alignForward(usize, base, 4);
    @memset(buf[base..pos], 0);

    const sub = SubHeader{
        .scope_count = @intCast(result.scope_tree.scopes.len),
        .symbol_count = @intCast(table.symbols.len),
        .reference_count = @intCast(table.references.len),
        .decl_node_count = @intCast(table.decl_nodes.len),
        .import_count = @intCast(records.imports.len),
        .export_count = @intCast(records.exports.len),
        .node_scope_count = @intCast(result.node_scopes.len),
    };
    @memcpy(buf[pos..][0..SUBHEADER_SIZE], std.mem.asBytes(&sub));
    pos += SUBHEADER_SIZE;

    for (result.scope_tree.scopes) |scope| {
        const entry = PackedScope{
            .node = @intFromEnum(scope.node),
            .parent = @intFromEnum(scope.parent),
            .hoist_target = @intFromEnum(scope.hoist_target),
            .bits = @as(u32, @intFromEnum(scope.kind)) |
                (@as(u32, @intFromBool(scope.flags.strict)) << SCOPE_STRICT_BIT),
        };
        @memcpy(buf[pos..][0..SCOPE_SIZE], std.mem.asBytes(&entry));
        pos += SCOPE_SIZE;
    }

    for (table.symbols) |symbol| {
        const entry = PackedSymbol{
            .name_start = symbol.name.start,
            .name_end = symbol.name.end,
            .flags = @bitCast(symbol.flags),
            .scope = @intFromEnum(symbol.scope),
            .decls_start = symbol.decls.start,
            .decls_len = symbol.decls.len,
        };
        @memcpy(buf[pos..][0..SYMBOL_SIZE], std.mem.asBytes(&entry));
        pos += SYMBOL_SIZE;
    }

    const decl_bytes = std.mem.sliceAsBytes(table.decl_nodes);
    @memcpy(buf[pos..][0..decl_bytes.len], decl_bytes);
    pos += decl_bytes.len;

    for (table.references, 0..) |reference, i| {
        const entry = PackedReference{
            .name_start = reference.name.start,
            .name_end = reference.name.end,
            .scope = @intFromEnum(reference.scope),
            .node = @intFromEnum(reference.node),
            .bits = (@as(u32, @intFromEnum(reference.kind)) << REFERENCE_TYPE_BIT) |
                (@as(u32, @intFromBool(reference.is_write)) << REFERENCE_WRITE_BIT),
            .symbol = @intFromEnum(table.resolutions[i]),
        };
        @memcpy(buf[pos..][0..REFERENCE_SIZE], std.mem.asBytes(&entry));
        pos += REFERENCE_SIZE;
    }

    for (records.imports) |record| {
        var bits: u32 = @intFromEnum(record.name_kind);
        bits |= @as(u32, @intFromEnum(record.kind)) << IMPORT_TYPE_BIT;
        if (record.phase) |phase| {
            bits |= @as(u32, 1) << IMPORT_HAS_PHASE_BIT;
            bits |= @as(u32, @intFromEnum(phase)) << IMPORT_PHASE_BIT;
        }
        const entry = PackedImport{
            .symbol = @intFromEnum(record.symbol),
            .bits = bits,
            .name_start = record.name.start,
            .name_end = record.name.end,
            .specifier_start = record.specifier.start,
            .specifier_end = record.specifier.end,
            .node = @intFromEnum(record.node),
        };
        @memcpy(buf[pos..][0..IMPORT_SIZE], std.mem.asBytes(&entry));
        pos += IMPORT_SIZE;
    }

    for (records.exports) |record| {
        const entry = PackedExport{
            .bits = @as(u32, @intFromEnum(record.name_kind)) |
                (@as(u32, @intFromEnum(record.from_kind)) << EXPORT_FROM_KIND_SHIFT) |
                (@as(u32, @intFromEnum(record.kind)) << EXPORT_TYPE_BIT),
            .name_start = record.name.start,
            .name_end = record.name.end,
            .symbol = @intFromEnum(record.symbol),
            .from_name_start = record.from_name.start,
            .from_name_end = record.from_name.end,
            .specifier_start = record.specifier.start,
            .specifier_end = record.specifier.end,
            .node = @intFromEnum(record.node),
        };
        @memcpy(buf[pos..][0..EXPORT_SIZE], std.mem.asBytes(&entry));
        pos += EXPORT_SIZE;
    }

    const node_scope_bytes = std.mem.sliceAsBytes(result.node_scopes);
    @memcpy(buf[pos..][0..node_scope_bytes.len], node_scope_bytes);
    pos += node_scope_bytes.len;

    // writer and size calculation must agree exactly
    std.debug.assert(pos == bufferSize(tree, result, records));
    return pos;
}
