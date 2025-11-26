const std = @import("std");
const token = @import("token.zig");

pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);
pub const Span = token.Span;

pub const IndexRange = struct {
    start: u32,
    len: u32,

    pub const empty: IndexRange = .{ .start = 0, .len = 0 };
};

pub const BinaryOperator = enum(u8) {
    Equal, // ==
    NotEqual, // !=
    StrictEqual, // ===
    StrictNotEqual, // !==
    LessThan, // <
    LessThanOrEqual, // <=
    GreaterThan, // >
    GreaterThanOrEqual, // >=
    Add, // +
    Subtract, // -
    Multiply, // *
    Divide, // /
    Modulo, // %
    Exponent, // **
    BitwiseOr, // |
    BitwiseXor, // ^
    BitwiseAnd, // &
    LeftShift, // <<
    RightShift, // >>
    UnsignedRightShift, // >>>
    In, // in
    Instanceof, // instanceof
};

pub const LogicalOperator = enum(u8) {
    And, // &&
    Or, // ||
    NullishCoalescing, // ??
};

pub const UnaryOperator = enum(u8) {
    Negate, // -
    Positive, // +
    LogicalNot, // !
    BitwiseNot, // ~
    Typeof, // typeof
    Void, // void
    Delete, // delete
};

pub const UpdateOperator = enum(u8) {
    Increment, // ++
    Decrement, // --
};

pub const AssignmentOperator = enum(u8) {
    Assign, // =
    AddAssign, // +=
    SubtractAssign, // -=
    MultiplyAssign, // *=
    DivideAssign, // /=
    ModuloAssign, // %=
    ExponentAssign, // **=
    LeftShiftAssign, // <<=
    RightShiftAssign, // >>=
    UnsignedRightShiftAssign, // >>>=
    BitwiseOrAssign, // |=
    BitwiseXorAssign, // ^=
    BitwiseAndAssign, // &=
    LogicalOrAssign, // ||=
    LogicalAndAssign, // &&=
    NullishAssign, // ??=
};

pub const VariableKind = enum(u8) {
    Var,
    Let,
    Const,
    Using,
    AwaitUsing,
};

pub const PropertyKind = enum(u8) {
    Init,
    Get,
    Set,
};

pub const BinaryExpression = struct {
    left: NodeIndex,
    right: NodeIndex,
    operator: BinaryOperator,
};

pub const LogicalExpression = struct {
    left: NodeIndex,
    right: NodeIndex,
    operator: LogicalOperator,
};

pub const UnaryExpression = struct {
    argument: NodeIndex,
    operator: UnaryOperator,
};

pub const UpdateExpression = struct {
    argument: NodeIndex,
    operator: UpdateOperator,
    prefix: bool,
};

pub const AssignmentExpression = struct {
    left: NodeIndex,
    right: NodeIndex,
    operator: AssignmentOperator,
};

pub const VariableDeclaration = struct {
    declarators: IndexRange,
    kind: VariableKind,
};

pub const VariableDeclarator = struct {
    id: NodeIndex,
    init: NodeIndex,
};

pub const ExpressionStatement = struct {
    expression: NodeIndex,
};

pub const StringLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

pub const NumericLiteral = struct {
    value: f64,
};

pub const BigIntLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

pub const BooleanLiteral = struct {
    value: bool,
};

pub const RegExpLiteral = struct {
    pattern_start: u32,
    pattern_len: u16,
    flags_start: u32,
    flags_len: u8,
};

pub const TemplateLiteral = struct {
    quasis: IndexRange,
    expressions: IndexRange,
};

pub const TemplateElement = struct {
    raw_start: u32,
    raw_len: u16,
    tail: bool,
};

pub const Identifier = struct {
    name_start: u32,
    name_len: u16,
};

pub const PrivateIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

pub const BindingIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

pub const IdentifierName = struct {
    name_start: u32,
    name_len: u16,
};

pub const AssignmentPattern = struct {
    left: NodeIndex,
    right: NodeIndex,
};

pub const RestElement = struct {
    argument: NodeIndex,
};

pub const ArrayPattern = struct {
    elements: IndexRange,
};

pub const ObjectPattern = struct {
    properties: IndexRange,
};

pub const BindingProperty = struct {
    key: NodeIndex,
    value: NodeIndex,
    shorthand: bool,
    computed: bool,
};

pub const ArrayExpression = struct {
    elements: IndexRange,
};

pub const ObjectExpression = struct {
    properties: IndexRange,
};

pub const SpreadElement = struct {
    argument: NodeIndex,
};

pub const ObjectProperty = struct {
    key: NodeIndex,
    value: NodeIndex,
    kind: PropertyKind,
    shorthand: bool,
    computed: bool,
};

pub const Program = struct {
    body: IndexRange,
    source_type: SourceType,
};

pub const SourceType = enum(u8) {
    Script,
    Module,
};

pub const Directive = struct {
    expression: NodeIndex,
    value_start: u32,
    value_len: u16,
};

pub const NodeData = union(enum) {
    binary_expression: BinaryExpression,
    logical_expression: LogicalExpression,
    unary_expression: UnaryExpression,
    update_expression: UpdateExpression,
    assignment_expression: AssignmentExpression,
    array_expression: ArrayExpression,
    object_expression: ObjectExpression,
    spread_element: SpreadElement,
    object_property: ObjectProperty,
    string_literal: StringLiteral,
    numeric_literal: NumericLiteral,
    bigint_literal: BigIntLiteral,
    boolean_literal: BooleanLiteral,
    null_literal,
    regexp_literal: RegExpLiteral,
    template_literal: TemplateLiteral,
    template_element: TemplateElement,
    identifier: Identifier,
    private_identifier: PrivateIdentifier,
    binding_identifier: BindingIdentifier,
    identifier_name: IdentifierName,
    expression_statement: ExpressionStatement,
    variable_declaration: VariableDeclaration,
    variable_declarator: VariableDeclarator,
    directive: Directive,
    assignment_pattern: AssignmentPattern,
    rest_element: RestElement,
    array_pattern: ArrayPattern,
    object_pattern: ObjectPattern,
    binding_property: BindingProperty,
    program: Program,
    simple_assignment_target: NodeIndex,
};

pub const Node = struct {
    data: NodeData,
    span: Span,
};

pub const NodeList = struct {
    nodes: std.MultiArrayList(Node),
    extra: std.ArrayList(NodeIndex),

    pub fn init(allocator: std.mem.Allocator, source_len: u32) NodeList {
        const estimated_nodes = @max(256, source_len / 2);
        const estimated_extra = estimated_nodes / 3;

        var nodes = std.MultiArrayList(Node);
        nodes.ensureTotalCapacity(allocator, estimated_nodes) catch unreachable;

        return .{
            .nodes = nodes,
            .extra = std.ArrayList(NodeIndex).initCapacity(allocator, estimated_extra) catch unreachable,
        };
    }

    pub inline fn add(self: *NodeList, allocator: std.mem.Allocator, data: NodeData, span: Span) NodeIndex {
        const index: NodeIndex = @intCast(self.nodes.len);
        self.nodes.append(allocator, .{ .data = data, .span = span }) catch unreachable;
        return index;
    }

    pub inline fn addExtra(self: *NodeList, allocator: std.mem.Allocator, indices: []const NodeIndex) IndexRange {
        const start: u32 = @intCast(self.extra.items.len);
        const len: u32 = @intCast(indices.len);
        self.extra.appendSlice(allocator, indices) catch unreachable;
        return .{ .start = start, .len = len };
    }

    pub inline fn getData(self: *const NodeList, index: NodeIndex) NodeData {
        return self.nodes.items(.data)[index];
    }

    pub inline fn getSpan(self: *const NodeList, index: NodeIndex) Span {
        return self.nodes.items(.span)[index];
    }

    pub inline fn getExtra(self: *const NodeList, range: IndexRange) []const NodeIndex {
        return self.extra.items[range.start..][0..range.len];
    }
};

pub fn binaryOperatorFromToken(tok: token.TokenType) BinaryOperator {
    return switch (tok) {
        .Equal => .Equal,
        .NotEqual => .NotEqual,
        .StrictEqual => .StrictEqual,
        .StrictNotEqual => .StrictNotEqual,
        .LessThan => .LessThan,
        .LessThanEqual => .LessThanOrEqual,
        .GreaterThan => .GreaterThan,
        .GreaterThanEqual => .GreaterThanOrEqual,
        .Plus => .Add,
        .Minus => .Subtract,
        .Star => .Multiply,
        .Slash => .Divide,
        .Percent => .Modulo,
        .Exponent => .Exponent,
        .BitwiseOr => .BitwiseOr,
        .BitwiseXor => .BitwiseXor,
        .BitwiseAnd => .BitwiseAnd,
        .LeftShift => .LeftShift,
        .RightShift => .RightShift,
        .UnsignedRightShift => .UnsignedRightShift,
        .In => .In,
        .Instanceof => .Instanceof,
        else => unreachable,
    };
}

pub fn logicalOperatorFromToken(tok: token.TokenType) LogicalOperator {
    return switch (tok) {
        .LogicalAnd => .And,
        .LogicalOr => .Or,
        .NullishCoalescing => .NullishCoalescing,
        else => unreachable,
    };
}

pub fn unaryOperatorFromToken(tok: token.TokenType) UnaryOperator {
    return switch (tok) {
        .Minus => .Negate,
        .Plus => .Positive,
        .LogicalNot => .LogicalNot,
        .BitwiseNot => .BitwiseNot,
        .Typeof => .Typeof,
        .Void => .Void,
        .Delete => .Delete,
        else => unreachable,
    };
}

pub fn updateOperatorFromToken(tok: token.TokenType) UpdateOperator {
    return switch (tok) {
        .Increment => .Increment,
        .Decrement => .Decrement,
        else => unreachable,
    };
}

pub fn assignmentOperatorFromToken(tok: token.TokenType) AssignmentOperator {
    return switch (tok) {
        .Assign => .Assign,
        .PlusAssign => .AddAssign,
        .MinusAssign => .SubtractAssign,
        .StarAssign => .MultiplyAssign,
        .SlashAssign => .DivideAssign,
        .PercentAssign => .ModuloAssign,
        .ExponentAssign => .ExponentAssign,
        .LeftShiftAssign => .LeftShiftAssign,
        .RightShiftAssign => .RightShiftAssign,
        .UnsignedRightShiftAssign => .UnsignedRightShiftAssign,
        .BitwiseOrAssign => .BitwiseOrAssign,
        .BitwiseXorAssign => .BitwiseXorAssign,
        .BitwiseAndAssign => .BitwiseAndAssign,
        .LogicalOrAssign => .LogicalOrAssign,
        .LogicalAndAssign => .LogicalAndAssign,
        .NullishAssign => .NullishAssign,
        else => unreachable,
    };
}

pub inline fn isNull(index: NodeIndex) bool {
    return index == null_node;
}
