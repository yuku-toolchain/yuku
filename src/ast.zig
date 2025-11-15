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

pub const Program = struct {
    type: []const u8 = "Program",
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

    pub inline fn getSpan(self: *const Expression) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

pub const ExpressionStatement = struct {
    type: []const u8 = "ExpressionStatement",
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

pub const BinaryExpression = struct {
    type: []const u8 = "BinaryExpression",
    left: *Expression,
    right: *Expression,
    operator: BinaryOperator,
    span: token.Span,
};

pub const LogicalExpression = struct {
    type: []const u8 = "LogicalExpression",
    left: *Expression,
    right: *Expression,
    operator: LogicalOperator,
    span: token.Span,
};

pub const VariableDeclaration = struct {
    type: []const u8 = "VariableDeclaration",
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
pub const Directive = struct {
    type: []const u8 = "ExpressionStatement",
    expression: *StringLiteral,
    directive: []const u8,
    span: token.Span,
};

pub const StringLiteral = struct {
    type: []const u8 = "Literal",
    value: []const u8,
    raw: ?[]const u8 = null,
    span: token.Span,
};

pub const BooleanLiteral = struct {
    type: []const u8 = "Literal",
    value: bool,
    raw: ?[]const u8 = null,
    span: token.Span,
};

pub const NullLiteral = struct {
    type: []const u8 = "Literal",
    value: ?[]const u8 = null,
    raw: ?[]const u8 = null,
    span: token.Span,
};

pub const NumericLiteral = struct {
    type: []const u8 = "Literal",
    value: f64,
    raw: ?[]const u8 = null,
    span: token.Span,
};

pub const BigIntLiteral = struct {
    type: []const u8 = "Literal",
    value: []const u8,
    raw: ?[]const u8 = null,
    bigint: []const u8,
    span: token.Span,
};

pub const RegExpLiteral = struct {
    type: []const u8 = "Literal",
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

pub const TemplateElement = struct {
    type: []const u8 = "TemplateElement",
    value: TemplateElementValue,
    tail: bool,
    span: token.Span,
};

pub const TemplateLiteral = struct {
    type: []const u8 = "TemplateLiteral",
    quasis: []*TemplateElement,
    expressions: []*Expression,
    span: token.Span,
};

pub const IdentifierReference = struct {
    type: []const u8 = "Identifier",
    name: []const u8,
    span: token.Span,
};

pub const IdentifierName = struct {
    type: []const u8 = "Identifier",
    name: []const u8,
    span: token.Span,
};

pub const PrivateIdentifier = struct {
    type: []const u8 = "PrivateIdentifier",
    name: []const u8,
    span: token.Span,
};

pub const BindingIdentifier = struct {
    type: []const u8 = "Identifier",
    name: []const u8,
    span: token.Span,
};

pub const AssignmentPattern = struct {
    type: []const u8 = "AssignmentPattern",
    left: *BindingPattern,
    right: *Expression,
    span: token.Span,
};

pub const BindingRestElement = struct {
    type: []const u8 = "RestElement",
    argument: *BindingPattern,
    span: token.Span,
};

pub const ArrayPattern = struct {
    type: []const u8 = "ArrayPattern",
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

pub const VariableDeclarator = struct {
    type: []const u8 = "VariableDeclarator",
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

pub const BindingProperty = struct {
    type: []const u8 = "Property",
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

pub const ObjectPattern = struct {
    type: []const u8 = "ObjectPattern",
    properties: []*ObjectPatternProperty,
    span: token.Span,
};
