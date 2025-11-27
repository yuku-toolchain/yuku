const std = @import("std");
const token = @import("token.zig");

/// index into the AST node array. Use `null_node` for optional nodes.
pub const NodeIndex = u32;
pub const null_node: NodeIndex = std.math.maxInt(NodeIndex);
pub const Span = token.Span;

/// range of indices in the extra array for storing node lists.
pub const IndexRange = struct {
    start: u32,
    len: u32,

    pub const empty: IndexRange = .{ .start = 0, .len = 0 };
};

/// https://tc39.es/ecma262/#sec-binary-operators
pub const BinaryOperator = enum {
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

pub const LogicalOperator = enum {
    And, // &&
    Or, // ||
    NullishCoalescing, // ??
};

pub const UnaryOperator = enum {
    Negate, // -
    Positive, // +
    LogicalNot, // !
    BitwiseNot, // ~
    Typeof, // typeof
    Void, // void
    Delete, // delete
};

pub const UpdateOperator = enum {
    Increment, // ++
    Decrement, // --
};

pub const AssignmentOperator = enum {
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

pub const VariableKind = enum {
    Var,
    Let,
    Const,
    Using,
    AwaitUsing,
};

pub const PropertyKind = enum {
    Init,
    Get,
    Set,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-binary-operators
pub const BinaryExpression = struct {
    /// Expression
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: BinaryOperator,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-binary-logical-operators
pub const LogicalExpression = struct {
    /// Expression
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: LogicalOperator,
};

/// `operator argument`
/// https://tc39.es/ecma262/#sec-unary-operators
pub const UnaryExpression = struct {
    /// Expression
    argument: NodeIndex,
    operator: UnaryOperator,
};

/// `++argument` or `argument++`
/// https://tc39.es/ecma262/#sec-update-expressions
pub const UpdateExpression = struct {
    /// SimpleAssignmentTarget (IdentifierReference | MemberExpression)
    argument: NodeIndex,
    operator: UpdateOperator,
    prefix: bool,
};

/// `left operator right`
/// https://tc39.es/ecma262/#sec-assignment-operators
pub const AssignmentExpression = struct {
    /// AssignmentTarget (IdentifierReference | MemberExpression | ArrayPattern | ObjectPattern)
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
    operator: AssignmentOperator,
};

/// https://tc39.es/ecma262/#sec-variable-statement
pub const VariableDeclaration = struct {
    kind: VariableKind,
    /// VariableDeclarator[]
    declarators: IndexRange,
};

/// `id = init`
pub const VariableDeclarator = struct {
    /// BindingPattern
    id: NodeIndex,
    /// Expression (optional, may be null_node)
    init: NodeIndex,
};

/// `expression;`
pub const ExpressionStatement = struct {
    /// Expression
    expression: NodeIndex,
};

/// https://tc39.es/ecma262/#sec-literals-string-literals
pub const StringLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

/// https://tc39.es/ecma262/#sec-literals-numeric-literals
pub const NumericLiteral = struct {
    value: f64,
};

/// https://tc39.es/ecma262/#sec-ecmascript-language-lexical-grammar-literals
pub const BigIntLiteral = struct {
    raw_start: u32,
    raw_len: u16,
};

pub const BooleanLiteral = struct {
    value: bool,
};

/// https://tc39.es/ecma262/#sec-literals-regular-expression-literals
pub const RegExpLiteral = struct {
    pattern_start: u32,
    pattern_len: u16,
    flags_start: u32,
    flags_len: u8,
};

/// https://tc39.es/ecma262/#sec-template-literals
pub const TemplateLiteral = struct {
    /// TemplateElement[]
    quasis: IndexRange,
    /// Expression[]
    expressions: IndexRange,
};

/// quasi
pub const TemplateElement = struct {
    raw_start: u32,
    raw_len: u16,
    tail: bool,
};

/// used in expressions
/// https://tc39.es/ecma262/#sec-identifiers
pub const IdentifierReference = struct {
    name_start: u32,
    name_len: u16,
};

/// `#name`
pub const PrivateIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

/// used in declarations
/// https://tc39.es/ecma262/#sec-identifiers
pub const BindingIdentifier = struct {
    name_start: u32,
    name_len: u16,
};

/// property keys, meta properties
pub const IdentifierName = struct {
    name_start: u32,
    name_len: u16,
};

/// `pattern = init`
/// https://tc39.es/ecma262/#prod-AssignmentPattern
pub const AssignmentPattern = struct {
    /// BindingPattern
    left: NodeIndex,
    /// Expression
    right: NodeIndex,
};

/// `...argument`
/// https://tc39.es/ecma262/#prod-BindingRestElement
pub const BindingRestElement = struct {
    /// BindingPattern
    argument: NodeIndex,
};

/// `[a, b, ...rest]`
/// https://tc39.es/ecma262/#prod-ArrayBindingPattern
pub const ArrayPattern = struct {
    /// (BindingPattern | null)[] - null for holes
    elements: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
};

/// `{a, b: c, ...rest}`
/// https://tc39.es/ecma262/#prod-ObjectBindingPattern
pub const ObjectPattern = struct {
    /// BindingProperty[]
    properties: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
};

/// `key: value` or `key` (shorthand)
pub const BindingProperty = struct {
    /// PropertyKey
    key: NodeIndex,
    /// BindingPattern
    value: NodeIndex,
    shorthand: bool,
    computed: bool,
};

/// `[a, b, ...c]`
pub const ArrayExpression = struct {
    /// (Expression | SpreadElement | null)[] - null for holes
    elements: IndexRange,
};

/// `{a: 1, b, ...c}`
pub const ObjectExpression = struct {
    /// (ObjectProperty | SpreadElement)[]
    properties: IndexRange,
};

/// `...argument`
pub const SpreadElement = struct {
    /// Expression
    argument: NodeIndex,
};

/// `key: value`, getter/setter, or method
pub const ObjectProperty = struct {
    /// PropertyKey
    key: NodeIndex,
    /// Expression
    value: NodeIndex,
    kind: PropertyKind,
    shorthand: bool,
    computed: bool,
    // method: bool, TODO: add this after we implement function/arrow function expressions
};

pub const Program = struct {
    source_type: SourceType,
    /// (Statement | ModuleDeclaration)[]
    body: IndexRange,
    /// Directive[]
    directives: IndexRange,
};

pub const SourceType = enum {
    Script,
    Module,
};

/// `"use strict";`
pub const Directive = struct {
    /// StringLiteral
    expression: NodeIndex,
    /// value without quotes
    value_start: u32,
    value_len: u16,
};

pub const FunctionType = enum {
    FunctionDeclaration,
    FunctionExpression,
};

/// https://tc39.es/ecma262/#sec-function-definitions
pub const Function = struct {
    type: FunctionType,
    /// BindingIdentifier (optional, may be null_node for anonymous functions)
    id: NodeIndex,
    generator: bool,
    async: bool,
    /// FormalParameters
    params: NodeIndex,
    /// FunctionBody (optional, may be null_node for declarations/overloads)
    body: NodeIndex,
};

/// https://tc39.es/ecma262/#prod-FormalParameters
pub const FormalParameters = struct {
    /// FormalParameter[]
    items: IndexRange,
    /// BindingRestElement (optional, may be null_node)
    rest: NodeIndex,
};

pub const FormalParameter = struct {
    /// BindingPattern
    pattern: NodeIndex,
};

pub const NodeData = union(enum) {
    function: Function,
    formal_parameters: FormalParameters,
    formal_parameter: FormalParameter,
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
    identifier_reference: IdentifierReference,
    private_identifier: PrivateIdentifier,
    binding_identifier: BindingIdentifier,
    identifier_name: IdentifierName,
    expression_statement: ExpressionStatement,
    variable_declaration: VariableDeclaration,
    variable_declarator: VariableDeclarator,
    directive: Directive,
    assignment_pattern: AssignmentPattern,
    binding_rest_element: BindingRestElement,
    array_pattern: ArrayPattern,
    object_pattern: ObjectPattern,
    binding_property: BindingProperty,
    program: Program,
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

        var nodes: std.MultiArrayList(Node) = .empty;
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
