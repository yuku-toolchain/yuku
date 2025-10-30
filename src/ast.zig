const std = @import("std");

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
};

pub const Program = struct {
    body: []*Node,
    source_type: SourceType = .script,

    pub const SourceType = enum { script, module };
};

// statements
pub const ExpressionStatement = struct {
    expression: *Node,
};

pub const VariableDeclaration = struct {
    kind: VariableKind,
    declarations: []*Node,

    pub const VariableKind = enum {
        @"var",
        let,
        @"const",
    };
};

// expressions
pub const Identifier = struct {
    name: []const u8,
};

pub const Literal = struct {
    value: LiteralValue,
    raw: ?[]const u8 = null,

    pub const LiteralValue = union(enum) {
        string: []const u8,
        number: f64,
        boolean: bool,
        null: void,
    };
};

// declarations
pub const VariableDeclarator = struct {
    id: *Node,
    init: ?*Node = null,
};

// like "use strict"
pub const Directive = struct {
    expression: *Node,
    directive: []const u8,
};
