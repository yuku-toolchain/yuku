const std = @import("std");
const token = @import("token.zig");

pub const Body = union(enum) {
    statement: *Statement,
    directive: *Directive,

    pub inline fn getSpan(self: *const Body) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

// Program
pub const Program = struct {
    body: []*Body,
    source_type: SourceType = .script,
    span: token.Span,

    pub const SourceType = enum { script, module };

    pub inline fn getSpan(self: *const Program) token.Span {
        return self.span;
    }
};

// statements
pub const Statement = union(enum) {
    expression_statement: ExpressionStatement,
    variable_declaration: VariableDeclaration,

    pub inline fn getSpan(self: *const Statement) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

// patterns
pub const BindingPattern = union(enum) {
    binding_identifier: BindingIdentifier,
    array_pattern: ArrayPattern,
    object_pattern: ObjectPattern,
    assignment_pattern: AssignmentPattern,

    pub inline fn getSpan(self: *const BindingPattern) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

// declarations
pub const Declaration = union(enum) {
    variable_declarator: VariableDeclarator,

    pub inline fn getSpan(self: *const Declaration) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

// expressions
pub const Expression = union(enum) {
    string_literal: StringLiteral,
    boolean_literal: BooleanLiteral,
    null_literal: NullLiteral,
    numeric_literal: NumericLiteral,
    bigint_literal: BigIntLiteral,
    regex_literal: RegExpLiteral,
    template_literal: TemplateLiteral,
    identifier_reference: IdentifierReference,
    private_identifier: PrivateIdentifier,
    binary_expression: BinaryExpression,
    logical_expression: LogicalExpression,
    unary_expression: UnaryExpression,
    update_expression: UpdateExpression,
    assignment_expression: AssignmentExpression,

    pub inline fn getSpan(self: *const Expression) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

// ExpressionStatement
pub const ExpressionStatement = struct {
    expression: *Expression,
    directive: ?[]const u8 = null,
    span: token.Span,
};

pub const BinaryOperator = enum {
    Equal, // ==
    NotEqual, // !=
    StrictEqual, // ===
    StrictNotEqual, // !==

    LessThan, // <
    LessThanEqual, // <=
    GreaterThan, // >
    GreaterThanEqual, // >=

    Plus, // +
    Minus, // -
    Star, // *
    Slash, // /
    Percent, // %
    Exponent, // **

    LeftShift, // <<
    RightShift, // >>
    UnsignedRightShift, // >>>

    BitwiseOr, // |
    BitwiseXor, // ^
    BitwiseAnd, // &

    In, // in
    Instanceof, // instanceof

    pub fn fromToken(token_type: token.TokenType) BinaryOperator {
        return switch (token_type) {
            .Equal => .Equal,
            .NotEqual => .NotEqual,
            .StrictEqual => .StrictEqual,
            .StrictNotEqual => .StrictNotEqual,
            .LessThan => .LessThan,
            .LessThanEqual => .LessThanEqual,
            .GreaterThan => .GreaterThan,
            .GreaterThanEqual => .GreaterThanEqual,
            .Plus => .Plus,
            .Minus => .Minus,
            .Star => .Star,
            .Slash => .Slash,
            .Percent => .Percent,
            .Exponent => .Exponent,
            .LeftShift => .LeftShift,
            .RightShift => .RightShift,
            .UnsignedRightShift => .UnsignedRightShift,
            .BitwiseOr => .BitwiseOr,
            .BitwiseXor => .BitwiseXor,
            .BitwiseAnd => .BitwiseAnd,
            .In => .In,
            .Instanceof => .Instanceof,
            else => unreachable, // safety: we are sure we only call fromToken for binary operators
        };
    }
};

pub const LogicalOperator = enum {
    LogicalOr, // ||
    LogicalAnd, // &&
    NullishCoalescing, // ??

    pub fn fromToken(token_type: token.TokenType) LogicalOperator {
        return switch (token_type) {
            .LogicalOr => .LogicalOr,
            .LogicalAnd => .LogicalAnd,
            .NullishCoalescing => .NullishCoalescing,
            else => unreachable, // safety: we are sure we only call fromToken for logical operators
        };
    }
};

pub const UnaryOperator = enum {
    Plus, // +
    Minus, // -
    LogicalNot, // !
    BitwiseNot, // ~
    Typeof, // typeof
    Void, // void
    Delete, // delete

    pub fn fromToken(token_type: token.TokenType) UnaryOperator {
        return switch (token_type) {
            .Plus => .Plus,
            .Minus => .Minus,
            .LogicalNot => .LogicalNot,
            .BitwiseNot => .BitwiseNot,
            .Typeof => .Typeof,
            .Void => .Void,
            .Delete => .Delete,
            else => unreachable, // safety: we are sure we only call fromToken for unary operators
        };
    }
};

pub const UpdateOperator = enum {
    Increment, // ++
    Decrement, // --

    pub fn fromToken(token_type: token.TokenType) UpdateOperator {
        return switch (token_type) {
            .Increment => .Increment,
            .Decrement => .Decrement,
            else => unreachable, // safety: we are sure we only call fromToken for update operators
        };
    }
};

pub const AssignmentOperator = enum {
    Assign, // =
    PlusAssign, // +=
    MinusAssign, // -=
    StarAssign, // *=
    SlashAssign, // /=
    PercentAssign, // %=
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

    pub fn fromToken(token_type: token.TokenType) AssignmentOperator {
        return switch (token_type) {
            .Assign => .Assign,
            .PlusAssign => .PlusAssign,
            .MinusAssign => .MinusAssign,
            .StarAssign => .StarAssign,
            .SlashAssign => .SlashAssign,
            .PercentAssign => .PercentAssign,
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
            else => unreachable, // safety: we are sure we only call fromToken for assignment operators
        };
    }
};

// UnaryExpression
pub const UnaryExpression = struct {
    operator: UnaryOperator,
    argument: *Expression,
    prefix: bool = true,
    span: token.Span,
};

// UpdateExpression
pub const UpdateExpression = struct {
    operator: UpdateOperator,
    prefix: bool,
    argument: *Expression,
    span: token.Span,
};

// BinaryExpression
pub const BinaryExpression = struct {
    left: *Expression,
    right: *Expression,
    operator: BinaryOperator,
    span: token.Span,
};

// LogicalExpression
pub const LogicalExpression = struct {
    left: *Expression,
    right: *Expression,
    operator: LogicalOperator,
    span: token.Span,
};

// AssignmentTarget (SimpleAssignmentTarget for now)
// Note: Full destructuring assignment targets will be added when needed
pub const AssignmentTarget = union(enum) {
    simple_assignment_target: *Expression, // IdentifierReference or MemberExpression

    pub inline fn getSpan(self: *const AssignmentTarget) token.Span {
        return switch (self.*) {
            .simple_assignment_target => |expr| expr.getSpan(),
        };
    }
};

// AssignmentExpression
pub const AssignmentExpression = struct {
    operator: AssignmentOperator,
    left: AssignmentTarget,
    right: *Expression,
    span: token.Span,
};

// VariableDeclaration
pub const VariableDeclaration = struct {
    kind: VariableDeclarationKind,
    declarations: []*VariableDeclarator,
    span: token.Span,

    pub const VariableDeclarationKind = enum {
        @"var",
        let,
        @"const",
        using,
        @"await using",
    };
};

// directive (like "use strict")
// ExpressionStatement
pub const Directive = struct {
    expression: *StringLiteral,
    directive: []const u8,
    span: token.Span,
};

// Literal
pub const StringLiteral = struct {
    value: []const u8,
    raw: ?[]const u8 = null,
    span: token.Span,
};

// Literal
pub const BooleanLiteral = struct {
    value: bool,
    raw: ?[]const u8 = null,
    span: token.Span,
};

// Literal
pub const NullLiteral = struct {
    value: ?[]const u8 = null,
    raw: ?[]const u8 = null,
    span: token.Span,
};

// Literal
pub const NumericLiteral = struct {
    value: f64,
    raw: ?[]const u8 = null,
    span: token.Span,
};

// Literal
pub const BigIntLiteral = struct {
    value: []const u8,
    raw: ?[]const u8 = null,
    bigint: []const u8,
    span: token.Span,
};

// Literal
pub const RegExpLiteral = struct {
    value: ?[]const u8 = null,
    raw: ?[]const u8 = null,
    regex: RegExp,
    span: token.Span,

    pub const RegExp = struct {
        pattern: []const u8,
        flags: []const u8,
    };
};

pub const TemplateElementValue = struct {
    raw: []const u8,
    cooked: ?[]const u8 = null,
};

// TemplateElement
pub const TemplateElement = struct {
    value: TemplateElementValue,
    tail: bool,
    span: token.Span,
};

// TemplateLiteral
pub const TemplateLiteral = struct {
    quasis: []*TemplateElement,
    expressions: []*Expression,
    span: token.Span,
};

// Identifier
pub const IdentifierReference = struct {
    name: []const u8,
    span: token.Span,
};

// Identifier
pub const IdentifierName = struct {
    name: []const u8,
    span: token.Span,
};

// PrivateIdentifier
pub const PrivateIdentifier = struct {
    name: []const u8,
    span: token.Span,
};

// Identifier
pub const BindingIdentifier = struct {
    name: []const u8,
    span: token.Span,
};

// AssignmentPattern
pub const AssignmentPattern = struct {
    left: *BindingPattern,
    right: *Expression,
    span: token.Span,
};

// RestElement
pub const BindingRestElement = struct {
    argument: *BindingPattern,
    span: token.Span,
};

// ArrayPattern
pub const ArrayPattern = struct {
    elements: []?*ArrayPatternElement,
    span: token.Span,
};

pub const ArrayPatternElement = union(enum) {
    binding_pattern: *BindingPattern,
    rest_element: *BindingRestElement,

    pub inline fn getSpan(self: *const ArrayPatternElement) token.Span {
        return switch (self.*) {
            .binding_pattern => |bp| bp.getSpan(),
            inline else => |variant| variant.span,
        };
    }
};

// VariableDeclarator
pub const VariableDeclarator = struct {
    id: *BindingPattern,
    init: ?*Expression = null,
    span: token.Span,
};

pub const PropertyKey = union(enum) {
    identifier_name: IdentifierName,
    private_identifier: PrivateIdentifier,
    expression: *Expression,

    pub inline fn getSpan(self: *const PropertyKey) token.Span {
        return switch (self.*) {
            .identifier_name => |id| id.span,
            .private_identifier => |id| id.span,
            .expression => |expr| expr.getSpan(),
        };
    }
};

// Property
pub const BindingProperty = struct {
    kind: []const u8 = "init",
    key: *PropertyKey,
    value: *BindingPattern,
    method: bool = false,
    shorthand: bool,
    computed: bool,
    span: token.Span,
};

pub const ObjectPatternProperty = union(enum) {
    binding_property: *BindingProperty,
    rest_element: *BindingRestElement,

    pub inline fn getSpan(self: *const ObjectPatternProperty) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

// ObjectPattern
pub const ObjectPattern = struct {
    properties: []*ObjectPatternProperty,
    span: token.Span,
};
