// binary AST serializer for fast native-to-JS transfer.
//
// format (all little-endian):
//
//   header (36 bytes)
//     magic: [4]u8 = "YUKU"
//     version: u32 = 1
//     node_count: u32
//     extra_count: u32
//     string_pool_len: u32
//     source_len: u32
//     comment_count: u32
//     diagnostic_count: u32
//     program_index: u32
//
//   nodes (node_count * 32 bytes)
//     tag: u8
//     flags: u8
//     field0: u16 (small field, used for IndexRange.len etc)
//     field1: u32
//     field2: u32
//     field3: u32
//     field4: u32
//     field5: u32
//     span_start: u32
//     span_end: u32
//
//   extra (extra_count * 4 bytes)
//
//   string_pool extra bytes
//
//   comments (comment_count * 20 bytes)
//
//   diagnostics (variable length)

const std = @import("std");
const ast = @import("ast.zig");

pub const VERSION: u32 = 1;
pub const HEADER_SIZE: u32 = 36;
pub const NODE_SIZE: u32 = 32;
pub const COMMENT_SIZE: u32 = 20;

// null sentinel for NodeIndex
const NULL: u32 = std.math.maxInt(u32);

pub const Tag = enum(u8) {
    sequence_expression,
    parenthesized_expression,
    arrow_function_expression,
    function,
    function_body,
    block_statement,
    formal_parameters,
    formal_parameter,
    binary_expression,
    logical_expression,
    conditional_expression,
    unary_expression,
    update_expression,
    assignment_expression,
    array_expression,
    object_expression,
    spread_element,
    object_property,
    member_expression,
    call_expression,
    chain_expression,
    tagged_template_expression,
    new_expression,
    await_expression,
    yield_expression,
    meta_property,
    decorator,
    class,
    class_body,
    method_definition,
    property_definition,
    static_block,
    super,
    string_literal,
    numeric_literal,
    bigint_literal,
    boolean_literal,
    null_literal,
    this_expression,
    regexp_literal,
    template_literal,
    template_element,
    identifier_reference,
    private_identifier,
    binding_identifier,
    identifier_name,
    label_identifier,
    expression_statement,
    if_statement,
    switch_statement,
    switch_case,
    for_statement,
    for_in_statement,
    for_of_statement,
    while_statement,
    do_while_statement,
    break_statement,
    continue_statement,
    labeled_statement,
    with_statement,
    return_statement,
    throw_statement,
    try_statement,
    catch_clause,
    debugger_statement,
    empty_statement,
    variable_declaration,
    variable_declarator,
    directive,
    assignment_pattern,
    binding_rest_element,
    array_pattern,
    object_pattern,
    binding_property,
    program,
    import_expression,
    import_declaration,
    import_specifier,
    import_default_specifier,
    import_namespace_specifier,
    import_attribute,
    export_named_declaration,
    export_default_declaration,
    export_all_declaration,
    export_specifier,
    ts_export_assignment,
    ts_namespace_export_declaration,
    jsx_element,
    jsx_opening_element,
    jsx_closing_element,
    jsx_fragment,
    jsx_opening_fragment,
    jsx_closing_fragment,
    jsx_identifier,
    jsx_namespaced_name,
    jsx_member_expression,
    jsx_attribute,
    jsx_spread_attribute,
    jsx_expression_container,
    jsx_empty_expression,
    jsx_text,
    jsx_spread_child,
};

/// 32-byte packed node.
///
/// layout:
///   [0]     tag
///   [1]     flags (packed bools + small enums)
///   [2..4]  field0 (u16, for small values like IndexRange.len)
///   [4..8]  field1 (u32)
///   [8..12] field2 (u32)
///   [12..16] field3 (u32)
///   [16..20] field4 (u32)
///   [20..24] field5 (u32)
///   [24..28] span_start (u32)
///   [28..32] span_end (u32)
const PackedNode = extern struct {
    tag: u8,
    flags: u8,
    field0: u16,
    field1: u32,
    field2: u32,
    field3: u32,
    field4: u32,
    field5: u32,
    span_start: u32,
    span_end: u32,
};

comptime {
    std.debug.assert(@sizeOf(PackedNode) == NODE_SIZE);
}

/// returns the exact byte size needed for the serialized buffer.
pub fn bufferSize(tree: *const ast.Tree) usize {
    const node_count: usize = tree.nodes.len;
    const extra_count: usize = tree.extra.items.len;
    const string_pool_len: usize = tree.strings.extra.items.len;
    const comment_count: usize = tree.comments.len;

    var size = HEADER_SIZE +
        node_count * NODE_SIZE +
        extra_count * 4 +
        string_pool_len +
        comment_count * COMMENT_SIZE;

    for (tree.diagnostics.items) |d| {
        size += 18 + d.message.len;
        if (d.help) |h| size += 4 + h.len;
        for (d.labels) |lbl| {
            size += 12 + lbl.message.len;
        }
    }

    return size;
}

/// serialize the AST into `buf`. buf must be at least `bufferSize(tree)` bytes.
pub fn serializeInto(tree: *const ast.Tree, buf: []u8) usize {
    const node_count: u32 = @intCast(tree.nodes.len);
    const extra_count: u32 = @intCast(tree.extra.items.len);
    const source_len: u32 = @intCast(tree.source.len);
    const string_pool_len: u32 = @intCast(tree.strings.extra.items.len);
    const comment_count: u32 = @intCast(tree.comments.len);
    const diag_count: u32 = @intCast(tree.diagnostics.items.len);
    const program_index: u32 = @intFromEnum(tree.program);

    var pos: usize = 0;

    // -- header --
    @memcpy(buf[0..4], "YUKU");
    std.mem.writeInt(u32, buf[4..8], VERSION, .little);
    std.mem.writeInt(u32, buf[8..12], node_count, .little);
    std.mem.writeInt(u32, buf[12..16], extra_count, .little);
    std.mem.writeInt(u32, buf[16..20], string_pool_len, .little);
    std.mem.writeInt(u32, buf[20..24], source_len, .little);
    std.mem.writeInt(u32, buf[24..28], comment_count, .little);
    std.mem.writeInt(u32, buf[28..32], diag_count, .little);
    std.mem.writeInt(u32, buf[32..36], program_index, .little);
    pos = HEADER_SIZE;

    // -- nodes --
    const data_items = tree.nodes.items(.data);
    const span_items = tree.nodes.items(.span);
    for (0..node_count) |i| {
        const node = packNode(data_items[i], span_items[i]);
        const bytes: *const [NODE_SIZE]u8 = @ptrCast(&node);
        @memcpy(buf[pos..][0..NODE_SIZE], bytes);
        pos += NODE_SIZE;
    }

    // -- extra --
    const extra_bytes = std.mem.sliceAsBytes(tree.extra.items);
    @memcpy(buf[pos..][0..extra_bytes.len], extra_bytes);
    pos += extra_bytes.len;

    // -- string pool --
    @memcpy(buf[pos..][0..string_pool_len], tree.strings.extra.items);
    pos += string_pool_len;

    // -- comments --
    for (tree.comments) |comment| {
        buf[pos] = @intFromEnum(comment.type);
        buf[pos + 1] = 0;
        buf[pos + 2] = 0;
        buf[pos + 3] = 0;
        std.mem.writeInt(u32, buf[pos + 4 ..][0..4], comment.start, .little);
        std.mem.writeInt(u32, buf[pos + 8 ..][0..4], comment.end, .little);
        std.mem.writeInt(u32, buf[pos + 12 ..][0..4], comment.value.start, .little);
        std.mem.writeInt(u32, buf[pos + 16 ..][0..4], comment.value.end, .little);
        pos += COMMENT_SIZE;
    }

    // -- diagnostics --
    for (tree.diagnostics.items) |d| {
        buf[pos] = @intFromEnum(d.severity);
        pos += 1;
        std.mem.writeInt(u32, buf[pos..][0..4], d.span.start, .little);
        pos += 4;
        std.mem.writeInt(u32, buf[pos..][0..4], d.span.end, .little);
        pos += 4;
        const msg_len: u32 = @intCast(d.message.len);
        std.mem.writeInt(u32, buf[pos..][0..4], msg_len, .little);
        pos += 4;
        @memcpy(buf[pos..][0..d.message.len], d.message);
        pos += d.message.len;
        if (d.help) |h| {
            buf[pos] = 1;
            pos += 1;
            const help_len: u32 = @intCast(h.len);
            std.mem.writeInt(u32, buf[pos..][0..4], help_len, .little);
            pos += 4;
            @memcpy(buf[pos..][0..h.len], h);
            pos += h.len;
        } else {
            buf[pos] = 0;
            pos += 1;
        }
        const lbl_count: u32 = @intCast(d.labels.len);
        std.mem.writeInt(u32, buf[pos..][0..4], lbl_count, .little);
        pos += 4;
        for (d.labels) |lbl| {
            std.mem.writeInt(u32, buf[pos..][0..4], lbl.span.start, .little);
            pos += 4;
            std.mem.writeInt(u32, buf[pos..][0..4], lbl.span.end, .little);
            pos += 4;
            const lmsg_len: u32 = @intCast(lbl.message.len);
            std.mem.writeInt(u32, buf[pos..][0..4], lmsg_len, .little);
            pos += 4;
            @memcpy(buf[pos..][0..lbl.message.len], lbl.message);
            pos += lbl.message.len;
        }
    }

    return pos;
}

/// packing convention for each node type:
///
/// flags byte packs bools and small enums (up to 8 bits total).
/// field0 (u16) packs a small value (IndexRange.len, enum ordinal for string kind, etc).
/// field1..field5 (u32 each) pack NodeIndex values, IndexRange.start values, and String start/end.
///
/// for IndexRange fields: start goes in a field slot, len goes in field0 (u16) if
/// there's only one range, otherwise len goes in the next field slot.
///
/// for String fields: start in one slot, end in the next slot.
fn packNode(data: ast.NodeData, span: ast.Span) PackedNode {
    var n = PackedNode{
        .tag = 0,
        .flags = 0,
        .field0 = 0,
        .field1 = 0,
        .field2 = 0,
        .field3 = 0,
        .field4 = 0,
        .field5 = 0,
        .span_start = span.start,
        .span_end = span.end,
    };

    switch (data) {
        // ===== expressions =====

        .sequence_expression => |d| {
            // range: expressions
            n.tag = @intFromEnum(Tag.sequence_expression);
            n.field0 = @intCast(d.expressions.len);
            n.field1 = d.expressions.start;
        },
        .parenthesized_expression => |d| {
            n.tag = @intFromEnum(Tag.parenthesized_expression);
            n.field1 = idx(d.expression);
        },
        .arrow_function_expression => |d| {
            // flags: expression(0), async(1)
            n.tag = @intFromEnum(Tag.arrow_function_expression);
            n.flags = flag(d.expression, 0) | flag(d.async, 1);
            n.field1 = idx(d.params);
            n.field2 = idx(d.body);
        },
        .function => |d| {
            // flags[0:1]: type enum, flags[2]: generator, flags[3]: async
            n.tag = @intFromEnum(Tag.function);
            n.flags = @intFromEnum(d.type) | flag(d.generator, 2) | flag(d.async, 3);
            n.field1 = idx(d.id);
            n.field2 = idx(d.params);
            n.field3 = idx(d.body);
        },
        .function_body => |d| {
            n.tag = @intFromEnum(Tag.function_body);
            n.field0 = @intCast(d.body.len);
            n.field1 = d.body.start;
        },
        .block_statement => |d| {
            n.tag = @intFromEnum(Tag.block_statement);
            n.field0 = @intCast(d.body.len);
            n.field1 = d.body.start;
        },
        .formal_parameters => |d| {
            // flags[0:1]: kind
            n.tag = @intFromEnum(Tag.formal_parameters);
            n.flags = @intFromEnum(d.kind);
            n.field0 = @intCast(d.items.len);
            n.field1 = d.items.start;
            n.field2 = idx(d.rest);
        },
        .formal_parameter => |d| {
            n.tag = @intFromEnum(Tag.formal_parameter);
            n.field1 = idx(d.pattern);
        },
        .binary_expression => |d| {
            n.tag = @intFromEnum(Tag.binary_expression);
            n.flags = @intFromEnum(d.operator);
            n.field1 = idx(d.left);
            n.field2 = idx(d.right);
        },
        .logical_expression => |d| {
            n.tag = @intFromEnum(Tag.logical_expression);
            n.flags = @intFromEnum(d.operator);
            n.field1 = idx(d.left);
            n.field2 = idx(d.right);
        },
        .conditional_expression => |d| {
            n.tag = @intFromEnum(Tag.conditional_expression);
            n.field1 = idx(d.@"test");
            n.field2 = idx(d.consequent);
            n.field3 = idx(d.alternate);
        },
        .unary_expression => |d| {
            n.tag = @intFromEnum(Tag.unary_expression);
            n.flags = @intFromEnum(d.operator);
            n.field1 = idx(d.argument);
        },
        .update_expression => |d| {
            // flags[0:1]: operator, flags[2]: prefix
            n.tag = @intFromEnum(Tag.update_expression);
            n.flags = @intFromEnum(d.operator) | flag(d.prefix, 2);
            n.field1 = idx(d.argument);
        },
        .assignment_expression => |d| {
            n.tag = @intFromEnum(Tag.assignment_expression);
            n.flags = @intFromEnum(d.operator);
            n.field1 = idx(d.left);
            n.field2 = idx(d.right);
        },
        .array_expression => |d| {
            n.tag = @intFromEnum(Tag.array_expression);
            n.field0 = @intCast(d.elements.len);
            n.field1 = d.elements.start;
        },
        .object_expression => |d| {
            n.tag = @intFromEnum(Tag.object_expression);
            n.field0 = @intCast(d.properties.len);
            n.field1 = d.properties.start;
        },
        .spread_element => |d| {
            n.tag = @intFromEnum(Tag.spread_element);
            n.field1 = idx(d.argument);
        },
        .object_property => |d| {
            // flags[0:1]: kind, flags[2]: method, flags[3]: shorthand, flags[4]: computed
            n.tag = @intFromEnum(Tag.object_property);
            n.flags = @intFromEnum(d.kind) | flag(d.method, 2) | flag(d.shorthand, 3) | flag(d.computed, 4);
            n.field1 = idx(d.key);
            n.field2 = idx(d.value);
        },
        .member_expression => |d| {
            // flags[0]: computed, flags[1]: optional
            n.tag = @intFromEnum(Tag.member_expression);
            n.flags = flag(d.computed, 0) | flag(d.optional, 1);
            n.field1 = idx(d.object);
            n.field2 = idx(d.property);
        },
        .call_expression => |d| {
            // flags[0]: optional
            n.tag = @intFromEnum(Tag.call_expression);
            n.flags = flag(d.optional, 0);
            n.field0 = @intCast(d.arguments.len);
            n.field1 = idx(d.callee);
            n.field2 = d.arguments.start;
        },
        .chain_expression => |d| {
            n.tag = @intFromEnum(Tag.chain_expression);
            n.field1 = idx(d.expression);
        },
        .tagged_template_expression => |d| {
            n.tag = @intFromEnum(Tag.tagged_template_expression);
            n.field1 = idx(d.tag);
            n.field2 = idx(d.quasi);
        },
        .new_expression => |d| {
            n.tag = @intFromEnum(Tag.new_expression);
            n.field0 = @intCast(d.arguments.len);
            n.field1 = idx(d.callee);
            n.field2 = d.arguments.start;
        },
        .await_expression => |d| {
            n.tag = @intFromEnum(Tag.await_expression);
            n.field1 = idx(d.argument);
        },
        .yield_expression => |d| {
            // flags[0]: delegate
            n.tag = @intFromEnum(Tag.yield_expression);
            n.flags = flag(d.delegate, 0);
            n.field1 = idx(d.argument);
        },
        .meta_property => |d| {
            n.tag = @intFromEnum(Tag.meta_property);
            n.field1 = idx(d.meta);
            n.field2 = idx(d.property);
        },

        // ===== class / decorator =====

        .decorator => |d| {
            n.tag = @intFromEnum(Tag.decorator);
            n.field1 = idx(d.expression);
        },
        .class => |d| {
            // flags[0]: type (0=declaration, 1=expression)
            // field0(u16): decorators.len
            n.tag = @intFromEnum(Tag.class);
            n.flags = @intFromEnum(d.type);
            n.field0 = @intCast(d.decorators.len);
            n.field1 = d.decorators.start;
            n.field2 = idx(d.id);
            n.field3 = idx(d.super_class);
            n.field4 = idx(d.body);
        },
        .class_body => |d| {
            n.tag = @intFromEnum(Tag.class_body);
            n.field0 = @intCast(d.body.len);
            n.field1 = d.body.start;
        },
        .method_definition => |d| {
            // flags[0:1]: kind, flags[2]: computed, flags[3]: static
            // field0(u16): decorators.len
            n.tag = @intFromEnum(Tag.method_definition);
            n.flags = @intFromEnum(d.kind) | flag(d.computed, 2) | flag(d.static, 3);
            n.field0 = @intCast(d.decorators.len);
            n.field1 = d.decorators.start;
            n.field2 = idx(d.key);
            n.field3 = idx(d.value);
        },
        .property_definition => |d| {
            // flags[0]: computed, flags[1]: static, flags[2]: accessor
            // field0(u16): decorators.len
            n.tag = @intFromEnum(Tag.property_definition);
            n.flags = flag(d.computed, 0) | flag(d.static, 1) | flag(d.accessor, 2);
            n.field0 = @intCast(d.decorators.len);
            n.field1 = d.decorators.start;
            n.field2 = idx(d.key);
            n.field3 = idx(d.value);
        },
        .static_block => |d| {
            n.tag = @intFromEnum(Tag.static_block);
            n.field0 = @intCast(d.body.len);
            n.field1 = d.body.start;
        },

        // ===== literals =====

        .super => {
            n.tag = @intFromEnum(Tag.super);
        },
        .string_literal => |d| {
            // string value: start, end
            n.tag = @intFromEnum(Tag.string_literal);
            n.field1 = d.value.start;
            n.field2 = d.value.end;
        },
        .numeric_literal => |d| {
            // flags[0:1]: kind
            // string raw: start, end
            n.tag = @intFromEnum(Tag.numeric_literal);
            n.flags = @intFromEnum(d.kind);
            n.field1 = d.raw.start;
            n.field2 = d.raw.end;
        },
        .bigint_literal => |d| {
            n.tag = @intFromEnum(Tag.bigint_literal);
            n.field1 = d.raw.start;
            n.field2 = d.raw.end;
        },
        .boolean_literal => |d| {
            // flags[0]: value
            n.tag = @intFromEnum(Tag.boolean_literal);
            n.flags = flag(d.value, 0);
        },
        .null_literal => {
            n.tag = @intFromEnum(Tag.null_literal);
        },
        .this_expression => {
            n.tag = @intFromEnum(Tag.this_expression);
        },
        .regexp_literal => |d| {
            // pattern: start/end, flags: start/end
            n.tag = @intFromEnum(Tag.regexp_literal);
            n.field1 = d.pattern.start;
            n.field2 = d.pattern.end;
            n.field3 = d.flags.start;
            n.field4 = d.flags.end;
        },
        .template_literal => |d| {
            // two ranges: quasis, expressions
            n.tag = @intFromEnum(Tag.template_literal);
            n.field0 = @intCast(d.quasis.len);
            n.field1 = d.quasis.start;
            n.field2 = d.expressions.start;
            n.field3 = @intCast(d.expressions.len);
        },
        .template_element => |d| {
            // flags[0]: tail, flags[1]: is_cooked_undefined
            // cooked string: start/end
            n.tag = @intFromEnum(Tag.template_element);
            n.flags = flag(d.tail, 0) | flag(d.is_cooked_undefined, 1);
            n.field1 = d.cooked.start;
            n.field2 = d.cooked.end;
        },

        // ===== identifiers =====

        .identifier_reference => |d| {
            n.tag = @intFromEnum(Tag.identifier_reference);
            n.field1 = d.name.start;
            n.field2 = d.name.end;
        },
        .private_identifier => |d| {
            n.tag = @intFromEnum(Tag.private_identifier);
            n.field1 = d.name.start;
            n.field2 = d.name.end;
        },
        .binding_identifier => |d| {
            n.tag = @intFromEnum(Tag.binding_identifier);
            n.field1 = d.name.start;
            n.field2 = d.name.end;
        },
        .identifier_name => |d| {
            n.tag = @intFromEnum(Tag.identifier_name);
            n.field1 = d.name.start;
            n.field2 = d.name.end;
        },
        .label_identifier => |d| {
            n.tag = @intFromEnum(Tag.label_identifier);
            n.field1 = d.name.start;
            n.field2 = d.name.end;
        },

        // ===== statements =====

        .expression_statement => |d| {
            n.tag = @intFromEnum(Tag.expression_statement);
            n.field1 = idx(d.expression);
        },
        .if_statement => |d| {
            n.tag = @intFromEnum(Tag.if_statement);
            n.field1 = idx(d.@"test");
            n.field2 = idx(d.consequent);
            n.field3 = idx(d.alternate);
        },
        .switch_statement => |d| {
            n.tag = @intFromEnum(Tag.switch_statement);
            n.field0 = @intCast(d.cases.len);
            n.field1 = idx(d.discriminant);
            n.field2 = d.cases.start;
        },
        .switch_case => |d| {
            n.tag = @intFromEnum(Tag.switch_case);
            n.field0 = @intCast(d.consequent.len);
            n.field1 = idx(d.@"test");
            n.field2 = d.consequent.start;
        },
        .for_statement => |d| {
            n.tag = @intFromEnum(Tag.for_statement);
            n.field1 = idx(d.init);
            n.field2 = idx(d.@"test");
            n.field3 = idx(d.update);
            n.field4 = idx(d.body);
        },
        .for_in_statement => |d| {
            n.tag = @intFromEnum(Tag.for_in_statement);
            n.field1 = idx(d.left);
            n.field2 = idx(d.right);
            n.field3 = idx(d.body);
        },
        .for_of_statement => |d| {
            // flags[0]: await
            n.tag = @intFromEnum(Tag.for_of_statement);
            n.flags = flag(d.await, 0);
            n.field1 = idx(d.left);
            n.field2 = idx(d.right);
            n.field3 = idx(d.body);
        },
        .while_statement => |d| {
            n.tag = @intFromEnum(Tag.while_statement);
            n.field1 = idx(d.@"test");
            n.field2 = idx(d.body);
        },
        .do_while_statement => |d| {
            n.tag = @intFromEnum(Tag.do_while_statement);
            n.field1 = idx(d.body);
            n.field2 = idx(d.@"test");
        },
        .break_statement => |d| {
            n.tag = @intFromEnum(Tag.break_statement);
            n.field1 = idx(d.label);
        },
        .continue_statement => |d| {
            n.tag = @intFromEnum(Tag.continue_statement);
            n.field1 = idx(d.label);
        },
        .labeled_statement => |d| {
            n.tag = @intFromEnum(Tag.labeled_statement);
            n.field1 = idx(d.label);
            n.field2 = idx(d.body);
        },
        .with_statement => |d| {
            n.tag = @intFromEnum(Tag.with_statement);
            n.field1 = idx(d.object);
            n.field2 = idx(d.body);
        },
        .return_statement => |d| {
            n.tag = @intFromEnum(Tag.return_statement);
            n.field1 = idx(d.argument);
        },
        .throw_statement => |d| {
            n.tag = @intFromEnum(Tag.throw_statement);
            n.field1 = idx(d.argument);
        },
        .try_statement => |d| {
            n.tag = @intFromEnum(Tag.try_statement);
            n.field1 = idx(d.block);
            n.field2 = idx(d.handler);
            n.field3 = idx(d.finalizer);
        },
        .catch_clause => |d| {
            n.tag = @intFromEnum(Tag.catch_clause);
            n.field1 = idx(d.param);
            n.field2 = idx(d.body);
        },
        .debugger_statement => {
            n.tag = @intFromEnum(Tag.debugger_statement);
        },
        .empty_statement => {
            n.tag = @intFromEnum(Tag.empty_statement);
        },

        // ===== declarations =====

        .variable_declaration => |d| {
            // flags[0:2]: kind
            n.tag = @intFromEnum(Tag.variable_declaration);
            n.flags = @intFromEnum(d.kind);
            n.field0 = @intCast(d.declarators.len);
            n.field1 = d.declarators.start;
        },
        .variable_declarator => |d| {
            n.tag = @intFromEnum(Tag.variable_declarator);
            n.field1 = idx(d.id);
            n.field2 = idx(d.init);
        },
        .directive => |d| {
            // expression NodeIndex + value String(start, end)
            n.tag = @intFromEnum(Tag.directive);
            n.field1 = idx(d.expression);
            n.field2 = d.value.start;
            n.field3 = d.value.end;
        },

        // ===== patterns =====

        .assignment_pattern => |d| {
            n.tag = @intFromEnum(Tag.assignment_pattern);
            n.field1 = idx(d.left);
            n.field2 = idx(d.right);
        },
        .binding_rest_element => |d| {
            n.tag = @intFromEnum(Tag.binding_rest_element);
            n.field1 = idx(d.argument);
        },
        .array_pattern => |d| {
            n.tag = @intFromEnum(Tag.array_pattern);
            n.field0 = @intCast(d.elements.len);
            n.field1 = d.elements.start;
            n.field2 = idx(d.rest);
        },
        .object_pattern => |d| {
            n.tag = @intFromEnum(Tag.object_pattern);
            n.field0 = @intCast(d.properties.len);
            n.field1 = d.properties.start;
            n.field2 = idx(d.rest);
        },
        .binding_property => |d| {
            // flags[0]: shorthand, flags[1]: computed
            n.tag = @intFromEnum(Tag.binding_property);
            n.flags = flag(d.shorthand, 0) | flag(d.computed, 1);
            n.field1 = idx(d.key);
            n.field2 = idx(d.value);
        },

        // ===== module =====

        .program => |d| {
            // flags[0]: source_type (0=script, 1=module)
            // flags[1]: has hashbang
            n.tag = @intFromEnum(Tag.program);
            n.flags = @intFromEnum(d.source_type) | flag(d.hashbang != null, 1);
            n.field0 = @intCast(d.body.len);
            n.field1 = d.body.start;
            if (d.hashbang) |h| {
                n.field2 = h.value.start;
                n.field3 = h.value.end;
            }
        },
        .import_expression => |d| {
            // flags[0]: has phase, flags[1:2]: phase value
            n.tag = @intFromEnum(Tag.import_expression);
            if (d.phase) |p| {
                n.flags = 1 | (@as(u8, @intFromEnum(p)) << 1);
            }
            n.field1 = idx(d.source);
            n.field2 = idx(d.options);
        },
        .import_declaration => |d| {
            // flags[0]: has phase, flags[1:2]: phase value
            // two ranges: specifiers, attributes
            n.tag = @intFromEnum(Tag.import_declaration);
            if (d.phase) |p| {
                n.flags = 1 | (@as(u8, @intFromEnum(p)) << 1);
            }
            n.field0 = @intCast(d.specifiers.len);
            n.field1 = d.specifiers.start;
            n.field2 = idx(d.source);
            n.field3 = d.attributes.start;
            n.field4 = @intCast(d.attributes.len);
        },
        .import_specifier => |d| {
            n.tag = @intFromEnum(Tag.import_specifier);
            n.field1 = idx(d.imported);
            n.field2 = idx(d.local);
        },
        .import_default_specifier => |d| {
            n.tag = @intFromEnum(Tag.import_default_specifier);
            n.field1 = idx(d.local);
        },
        .import_namespace_specifier => |d| {
            n.tag = @intFromEnum(Tag.import_namespace_specifier);
            n.field1 = idx(d.local);
        },
        .import_attribute => |d| {
            n.tag = @intFromEnum(Tag.import_attribute);
            n.field1 = idx(d.key);
            n.field2 = idx(d.value);
        },
        .export_named_declaration => |d| {
            // two ranges: specifiers, attributes + declaration + source
            n.tag = @intFromEnum(Tag.export_named_declaration);
            n.field0 = @intCast(d.specifiers.len);
            n.field1 = idx(d.declaration);
            n.field2 = d.specifiers.start;
            n.field3 = idx(d.source);
            n.field4 = d.attributes.start;
            n.field5 = @intCast(d.attributes.len);
        },
        .export_default_declaration => |d| {
            n.tag = @intFromEnum(Tag.export_default_declaration);
            n.field1 = idx(d.declaration);
        },
        .export_all_declaration => |d| {
            n.tag = @intFromEnum(Tag.export_all_declaration);
            n.field0 = @intCast(d.attributes.len);
            n.field1 = idx(d.exported);
            n.field2 = idx(d.source);
            n.field3 = d.attributes.start;
        },
        .export_specifier => |d| {
            n.tag = @intFromEnum(Tag.export_specifier);
            n.field1 = idx(d.local);
            n.field2 = idx(d.exported);
        },

        // ===== typescript =====

        .ts_export_assignment => |d| {
            n.tag = @intFromEnum(Tag.ts_export_assignment);
            n.field1 = idx(d.expression);
        },
        .ts_namespace_export_declaration => |d| {
            n.tag = @intFromEnum(Tag.ts_namespace_export_declaration);
            n.field1 = idx(d.id);
        },

        // ===== jsx =====

        .jsx_element => |d| {
            n.tag = @intFromEnum(Tag.jsx_element);
            n.field0 = @intCast(d.children.len);
            n.field1 = idx(d.opening_element);
            n.field2 = d.children.start;
            n.field3 = idx(d.closing_element);
        },
        .jsx_opening_element => |d| {
            // flags[0]: self_closing
            n.tag = @intFromEnum(Tag.jsx_opening_element);
            n.flags = flag(d.self_closing, 0);
            n.field0 = @intCast(d.attributes.len);
            n.field1 = idx(d.name);
            n.field2 = d.attributes.start;
        },
        .jsx_closing_element => |d| {
            n.tag = @intFromEnum(Tag.jsx_closing_element);
            n.field1 = idx(d.name);
        },
        .jsx_fragment => |d| {
            n.tag = @intFromEnum(Tag.jsx_fragment);
            n.field0 = @intCast(d.children.len);
            n.field1 = idx(d.opening_fragment);
            n.field2 = d.children.start;
            n.field3 = idx(d.closing_fragment);
        },
        .jsx_opening_fragment => {
            n.tag = @intFromEnum(Tag.jsx_opening_fragment);
        },
        .jsx_closing_fragment => {
            n.tag = @intFromEnum(Tag.jsx_closing_fragment);
        },
        .jsx_identifier => |d| {
            n.tag = @intFromEnum(Tag.jsx_identifier);
            n.field1 = d.name.start;
            n.field2 = d.name.end;
        },
        .jsx_namespaced_name => |d| {
            n.tag = @intFromEnum(Tag.jsx_namespaced_name);
            n.field1 = idx(d.namespace);
            n.field2 = idx(d.name);
        },
        .jsx_member_expression => |d| {
            n.tag = @intFromEnum(Tag.jsx_member_expression);
            n.field1 = idx(d.object);
            n.field2 = idx(d.property);
        },
        .jsx_attribute => |d| {
            n.tag = @intFromEnum(Tag.jsx_attribute);
            n.field1 = idx(d.name);
            n.field2 = idx(d.value);
        },
        .jsx_spread_attribute => |d| {
            n.tag = @intFromEnum(Tag.jsx_spread_attribute);
            n.field1 = idx(d.argument);
        },
        .jsx_expression_container => |d| {
            n.tag = @intFromEnum(Tag.jsx_expression_container);
            n.field1 = idx(d.expression);
        },
        .jsx_empty_expression => {
            n.tag = @intFromEnum(Tag.jsx_empty_expression);
        },
        .jsx_text => |d| {
            n.tag = @intFromEnum(Tag.jsx_text);
            n.field1 = d.value.start;
            n.field2 = d.value.end;
        },
        .jsx_spread_child => |d| {
            n.tag = @intFromEnum(Tag.jsx_spread_child);
            n.field1 = idx(d.expression);
        },
    }

    return n;
}

inline fn idx(node: ast.NodeIndex) u32 {
    return @intFromEnum(node);
}

inline fn flag(val: bool, bit: u3) u8 {
    return if (val) @as(u8, 1) << bit else 0;
}
