const std = @import("std");
const token = @import("token.zig");

pub const Body = union(enum) {
    statement: *Statement,
    directive: *Directive,

    pub inline fn getSpan(self: *const Body) token.Span {
        return switch (self.*) {
            .statement => |stmt| stmt.getSpan(),
            .directive => |dir| dir.span,
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

pub const ExpressionStatement = struct {
    type: []const u8 = "ExpressionStatement",
    expression: *Expression,
    directive: ?[]const u8 = null,
    span: token.Span,
};

pub const VariableDeclaration = struct {
    type: []const u8 = "VariableDeclaration",
    kind: VariableDeclarationKind,
    declarations: []*VariableDeclarator,
    // declare: ?bool = null, when typescript
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

// expressions
pub const Expression = union(enum) {
    string_literal: StringLiteral,
    identifier_reference: IdentifierReference,
    // boolean_literal, numeric_literal, etc.

    pub inline fn getSpan(self: *const Expression) token.Span {
        return switch (self.*) {
            inline else => |variant| variant.span,
        };
    }
};

pub const StringLiteral = struct {
    type: []const u8 = "Literal",
    value: []const u8,
    raw: ?[]const u8 = null,
    span: token.Span,
};

pub const IdentifierReference = struct {
    type: []const u8 = "Identifier",
    name: []const u8,
    span: token.Span,
};

pub const BindingIdentifier = struct {
    type: []const u8 = "Identifier",
    name: []const u8,
    span: token.Span,
};

// patterns

pub const BindingPattern = union(enum) {
    binding_identifier: BindingIdentifier,
    // TODO: object_pattern, array_pattern, assignment_pattern

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

pub const VariableDeclarator = struct {
    type: []const u8 = "VariableDeclarator",
    id: *BindingPattern,
    init: ?*Expression = null,
    // definite: ?bool = null, when typescript
    span: token.Span,
};
