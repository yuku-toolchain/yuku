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

    pub inline fn getType(self: *const Body) []const u8 {
        return switch (self.*) {
            .statement => |s| s.getType(),
            .directive => "ExpressionStatement",
        };
    }
};

pub const Program = struct {
    body: []*Body,
    source_type: SourceType = .script,
    span: token.Span,

    pub const SourceType = enum { script, module };

    pub inline fn getSpan(self: *const Program) token.Span {
        return self.span;
    }

    pub inline fn getType() []const u8 {
        return "Program";
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

    pub inline fn getType(self: *const Statement) []const u8 {
        return switch (self.*) {
            .expression_statement => "ExpressionStatement",
            .variable_declaration => "VariableDeclaration",
        };
    }
};

pub const ExpressionStatement = struct {
    expression: *Expression,
    directive: ?[]const u8 = null,
    span: token.Span,

    pub inline fn getType() []const u8 {
        return "ExpressionStatement";
    }
};

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

    pub inline fn getType() []const u8 {
        return "VariableDeclaration";
    }
};

// directive (like "use strict")
pub const Directive = struct {
    expression: *StringLiteral,
    directive: []const u8,
    span: token.Span,

    pub inline fn getType() []const u8 {
        return "ExpressionStatement";
    }
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

    pub inline fn getType(self: *const Expression) []const u8 {
        return switch (self.*) {
            .string_literal => "Literal",
            .identifier_reference => "Identifier",
        };
    }
};

pub const StringLiteral = struct {
    value: []const u8,
    raw: ?[]const u8 = null,
    span: token.Span,

    pub inline fn getType() []const u8 {
        return "Literal";
    }
};

pub const IdentifierReference = struct {
    name: []const u8,
    span: token.Span,

    pub inline fn getType() []const u8 {
        return "Identifier";
    }
};

pub const BindingIdentifier = struct {
    name: []const u8,
    span: token.Span,

    pub inline fn getType() []const u8 {
        return "Identifier";
    }
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

    pub inline fn getType(self: *const BindingPattern) []const u8 {
        return switch (self.*) {
            .binding_identifier => "Identifier",
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

    pub inline fn getType(self: *const Declaration) []const u8 {
        return switch (self.*) {
            .variable_declarator => "VariableDeclarator",
        };
    }
};

pub const VariableDeclarator = struct {
    id: *BindingPattern,
    init: ?*Expression = null,
    span: token.Span,

    pub inline fn getType() []const u8 {
        return "VariableDeclarator";
    }
};
