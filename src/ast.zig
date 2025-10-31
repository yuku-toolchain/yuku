const std = @import("std");

const token = @import("token.zig");

pub const Node = union(enum) {
    program: Program,

    // statements
    expression_statement: ExpressionStatement,
    variable_declaration: VariableDeclaration,

    // expressions
    identifier: Identifier,
    literal: Literal,

    // declarations
    variable_declarator: VariableDeclarator,

    directive: Directive,

    pub inline fn getSpan(self: *const Node) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

pub const Program = struct {
    type: []const u8 = "Program",
    body: []*Node,
    source_type: SourceType = .script,
    span: token.Span,

    pub const SourceType = enum { script, module };
};

// statements
pub const ExpressionStatement = struct {
    type: []const u8 = "ExpressionStatement",
    expression: *Node,
    span: token.Span,
};

pub const VariableDeclaration = struct {
    type: []const u8 = "VariableDeclaration",
    kind: VariableKind,
    declarations: []*Node,
    span: token.Span,

    pub const VariableKind = enum {
        @"var",
        let,
        @"const",
        using,
        @"await using",
    };
};

// expressions
pub const Identifier = struct {
    type: []const u8 = "Identifier",
    name: []const u8,
    span: token.Span,
};

pub const Literal = struct {
    type: []const u8 = "Literal",
    value: LiteralValue,
    raw: ?[]const u8 = null,
    span: token.Span,

    pub const LiteralValue = union(enum) {
        string: []const u8,
        number: f64,
        boolean: bool,
        null: void,
    };
};

// declarations
pub const VariableDeclarator = struct {
    type: []const u8 = "VariableDeclarator",
    id: *Node,
    init: ?*Node = null,
    span: token.Span,
};

// like "use strict"
pub const Directive = struct {
    type: []const u8 = "Directive",
    expression: *Node,
    directive: []const u8,
    span: token.Span,
};
