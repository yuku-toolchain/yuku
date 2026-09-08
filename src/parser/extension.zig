//! Compile-time extension points that teach the parser syntax outside the ECMAScript and
//! TypeScript grammars. `-Dparser-extension=path` binds a file whose `pub fn`s named after
//! a `Point` run there.

const std = @import("std");
const ast = @import("ast.zig");
const binding = @import("parser_extension");

const Error = @import("parser.zig").Error;

pub const Point = enum {
    at_expression,
    at_statement,
    binding_pattern,
    binding_start,
    expression_prefix,
    for_of_tail,
    function_body,
    function_has_body,
    jsx_child,
    jsx_element_name,
    jsx_element_name_check,
    jsx_element_tail,
    jsx_fragment_tail,
    jsx_names_match,
    jsx_text_boundary,
    jsx_text_value,
    module_specifier,
};

const Kind = enum { node, value, predicate, advisory };

/// A node hook's result. A `null` node means it reported a diagnostic instead.
pub const Outcome = struct {
    node: ?ast.NodeIndex,

    pub const failed: Outcome = .{ .node = null };

    pub fn of(node: ast.NodeIndex) Outcome {
        return .{ .node = node };
    }
};

pub const ForOfTail = struct {
    start: u32,
    left: ast.NodeIndex,
    right: ast.NodeIndex,
    is_await: bool,
};

/// Runs the hook bound at `point`, or answers "nothing bound" in its kind's shape.
pub inline fn at(comptime point: Point, args: anytype) Return(point) {
    comptime validateCall(point, @TypeOf(args));
    comptime if (bound(point)) validateHook(point);

    const kind = comptime spec(point).kind;

    if (comptime !bound(point)) {
        if (comptime kind == .advisory) return;
        return null;
    }

    const hook = @field(binding, @tagName(point));

    if (comptime kind == .predicate) return @call(.auto, hook, args);
    return @call(.auto, hook, .{Return(point)} ++ args);
}

inline fn bound(comptime point: Point) bool {
    return @hasDecl(binding, @tagName(point));
}

const Spec = struct {
    kind: Kind,
    // call-site arguments, excluding the comptime `R`
    args: u8,
    Value: type = void,
    note: []const u8,
};

fn spec(comptime point: Point) Spec {
    return switch (point) {
        .at_expression => .{ .kind = .node, .args = 1, .note = "`@` in expression position" },
        .at_statement => .{ .kind = .node, .args = 1, .note = "`@` in statement position" },
        .binding_pattern => .{ .kind = .node, .args = 1, .note = "head of a binding pattern" },
        .expression_prefix => .{ .kind = .node, .args = 1, .note = "head of any prefix position" },
        // may consume its clause before declining, unlike every other point
        .for_of_tail => .{ .kind = .node, .args = 2, .note = "past a for-of right operand" },
        .function_body => .{ .kind = .node, .args = 1, .note = "head of a function body" },
        .jsx_child => .{ .kind = .node, .args = 1, .note = "past `{` in a JSX child" },
        .jsx_element_name => .{
            .kind = .node,
            .args = 1,
            .note = "an opening or closing tag name",
        },
        .jsx_element_tail => .{ .kind = .node, .args = 3, .note = "past a JSX opening element" },
        .jsx_fragment_tail => .{ .kind = .node, .args = 2, .note = "past a JSX opening fragment" },
        .module_specifier => .{ .kind = .node, .args = 1, .note = "a non-string module specifier" },

        .jsx_text_value => .{
            .kind = .value,
            .args = 2,
            .Value = ast.String,
            .note = "interned value of a JSX text run",
        },

        .binding_start => .{ .kind = .predicate, .args = 1, .note = "can a tag begin a binding" },
        .function_has_body => .{
            .kind = .predicate,
            .args = 1,
            .note = "does a `function` carry a body",
        },
        .jsx_names_match => .{
            .kind = .predicate,
            .args = 3,
            .note = "does a closing tag match its opening tag",
        },
        .jsx_text_boundary => .{
            .kind = .predicate,
            .args = 2,
            .note = "does a byte end a JSX text run",
        },

        .jsx_element_name_check => .{
            .kind = .advisory,
            .args = 2,
            .note = "an opening tag name, already parsed",
        },
    };
}

fn Return(comptime point: Point) type {
    const s = comptime spec(point);
    return switch (s.kind) {
        .node => Error!?Outcome,
        .value => Error!?s.Value,
        .predicate => ?bool,
        .advisory => Error!void,
    };
}

fn validateCall(comptime point: Point, comptime Args: type) void {
    const info = @typeInfo(Args);
    if (info != .@"struct" or !info.@"struct".is_tuple) {
        @compileError("extension.at expects a tuple of arguments, found " ++ @typeName(Args));
    }
    const want = spec(point).args;
    if (info.@"struct".fields.len != want) {
        @compileError(std.fmt.comptimePrint(
            "extension point {s} takes {d} arguments, the call site passes {d}",
            .{ @tagName(point), want, info.@"struct".fields.len },
        ));
    }
}

fn validateHook(comptime point: Point) void {
    const s = spec(point);
    const want = s.args + @intFromBool(s.kind != .predicate);
    const found = @typeInfo(@TypeOf(@field(binding, @tagName(point)))).@"fn".params.len;
    if (found != want) @compileError(std.fmt.comptimePrint(
        "parser_extension.{s} declares {d} parameters, a {s} hook there takes {d}",
        .{ @tagName(point), found, @tagName(s.kind), want },
    ));
}

// a typo must not compile to a hook that never runs
comptime {
    const decls = @typeInfo(binding).@"struct".decls;
    @setEvalBranchQuota(1000 + 64 * decls.len * @typeInfo(Point).@"enum".fields.len);

    for (decls) |decl| {
        if (@typeInfo(@TypeOf(@field(binding, decl.name))) != .@"fn") continue;
        if (!isSnakeCase(decl.name)) continue;

        const point = pointNamed(decl.name) orelse @compileError(
            "parser_extension declares the snake_case function \"" ++ decl.name ++
                "\", which is not an extension point. Rename it, or make it non-`pub`." ++
                " The extension points are:" ++ point_list,
        );

        validateHook(point);
    }
}

fn pointNamed(comptime name: []const u8) ?Point {
    for (std.meta.fields(Point)) |field| {
        if (std.mem.eql(u8, field.name, name)) return @enumFromInt(field.value);
    }
    return null;
}

fn isSnakeCase(comptime name: []const u8) bool {
    for (name) |c| switch (c) {
        'a'...'z', '0'...'9', '_' => {},
        else => return false,
    };
    return true;
}

const point_list = blk: {
    var list: []const u8 = "";
    for (std.meta.fieldNames(Point)) |name| {
        list = list ++ "\n  " ++ name ++ " (" ++ @tagName(spec(@field(Point, name)).kind) ++
            ") " ++ spec(@field(Point, name)).note;
    }
    break :blk list;
};
