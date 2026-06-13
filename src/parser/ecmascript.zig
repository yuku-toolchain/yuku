const std = @import("std");
const ast = @import("ast.zig");
const NodePath = @import("traverser/walk.zig").NodePath;

pub const PropName = struct {
    name: []const u8,
    span: ast.Span,
    is_string_literal: bool,

    pub fn eql(self: PropName, expected: []const u8) bool {
        return std.mem.eql(u8, self.name, expected);
    }
};

/// https://tc39.es/ecma262/#sec-static-semantics-propname
pub fn propName(tree: *const ast.Tree, key: ast.NodeIndex) ?PropName {
    switch (tree.data(key)) {
        .identifier_name => |id| return .{
            .name = tree.string(id.name),
            .span = tree.span(key),
            .is_string_literal = false,
        },
        .string_literal => |str| {
            const name = tree.string(str.value);
            if (name.len == 0) return null;
            return .{
                .name = name,
                .span = tree.span(key),
                .is_string_literal = true,
            };
        },
        else => return null,
    }
}

/// True when the node at the top of `path` sits in assignment-target
/// position: the LHS of an assignment, the operand of `++`/`--`, the
/// iteration variable of for-in/for-of, or a leaf of a destructuring
/// assignment pattern that reaches one of those.
///
/// Climbs only through pattern containers and transparent wrappers, so a
/// default value (`[a = b] = c`, the `b`), a computed key, or the object
/// of a member write (`obj.x = 1`, the `obj`) all classify as reads.
///
/// https://tc39.es/ecma262/#sec-static-semantics-assignmenttargettype
pub fn isWriteTarget(tree: *const ast.Tree, path: *const NodePath) bool {
    std.debug.assert(path.depth() > 0);

    var child = path.ancestor(0) orelse return false;
    var n: usize = 1;
    while (path.ancestor(n)) |parent| : (n += 1) {
        std.debug.assert(parent != child);
        switch (tree.data(parent)) {
            .assignment_expression => |a| return a.left == child,
            .update_expression => |u| return u.argument == child,
            // assignment form only. the declaration form holds a
            // variable_declaration whose leaves are binding identifiers,
            // never references.
            .for_in_statement => |f| return f.left == child,
            .for_of_statement => |f| return f.left == child,
            .array_pattern => child = parent,
            .object_pattern => child = parent,
            // computed keys are reads
            .binding_property => |bp| {
                if (bp.value != child) return false;
                child = parent;
            },
            // the default value is a read
            .assignment_pattern => |ap| {
                if (ap.left != child) return false;
                child = parent;
            },
            .binding_rest_element => |r| {
                if (r.argument != child) return false;
                child = parent;
            },
            // transparent wrappers: `(a) += 1`, `a!++`, `(a as T) = b`
            .parenthesized_expression => child = parent,
            .ts_non_null_expression => child = parent,
            .ts_as_expression => |e| {
                if (e.expression != child) return false;
                child = parent;
            },
            .ts_satisfies_expression => |e| {
                if (e.expression != child) return false;
                child = parent;
            },
            .ts_type_assertion => |e| {
                if (e.expression != child) return false;
                child = parent;
            },
            // member objects, call arguments, initializers: reads
            else => return false,
        }
    }
    return false;
}

/// https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist
pub fn findNonSimpleParameter(tree: *const ast.Tree, params: ast.FormalParameters) ?ast.NodeIndex {
    if (params.rest != .null) return params.rest;

    for (tree.extra(params.items)) |param_idx| {
        const pattern = switch (tree.data(param_idx)) {
            .formal_parameter => |fp| fp.pattern,
            .ts_parameter_property => |pp| pp.parameter,
            else => return param_idx,
        };
        switch (tree.data(pattern)) {
            // `this: T` is not a runtime parameter, erased at emit, so the
            // ECMAScript "simple parameter list" rule does not apply to it.
            .binding_identifier, .ts_this_parameter => {},
            else => return pattern,
        }
    }

    return null;
}
