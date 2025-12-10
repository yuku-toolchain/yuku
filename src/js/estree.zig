const std = @import("std");
const ast = @import("ast.zig");
const parser = @import("parser.zig");

pub const EstreeJsonOptions = struct {
    pretty: bool = true,
    indent_size: u32 = 2,
};

pub const Serializer = struct {
    tree: *const parser.ParseTree,
    buffer: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    depth: u32 = 0,
    options: EstreeJsonOptions,
    needs_comma: [max_depth]bool = [_]bool{false} ** max_depth,
    scratch: std.ArrayList(u8) = .empty,
    isTs: bool,
    pos_map: []u32,

    const Self = @This();
    const Error = error{ NoSpaceLeft, OutOfMemory };
    const max_depth = 64;

    pub fn serialize(tree: *const parser.ParseTree, allocator: std.mem.Allocator, options: EstreeJsonOptions) ![]u8 {
        var buffer: std.ArrayList(u8) = try .initCapacity(allocator, tree.source.len * 3);
        errdefer buffer.deinit(allocator);

        const pos_map = try buildUtf16PosMap(allocator, tree.source);
        defer allocator.free(pos_map);

        var self = Self{
            .tree = tree,
            .buffer = &buffer,
            .allocator = allocator,
            .options = options,
            .scratch = try .initCapacity(allocator, 256),
            .isTs = switch (tree.getLang()) {
                .ts, .tsx, .dts => true,
                else => false,
            },
            .pos_map = pos_map,
        };
        defer self.scratch.deinit(allocator);

        try self.beginObject();
        try self.fieldNode("program", tree.program);
        try self.field("comments");
        try self.writeComments();
        try self.field("errors");
        try self.writeDiagnostics();
        try self.endObject();

        return buffer.toOwnedSlice(allocator);
    }

    fn writeNode(self: *Self, index: ast.NodeIndex) Error!void {
        if (ast.isNull(index)) {
            try self.writeNull();
            return;
        }

        const data = self.tree.getData(index);
        const span = self.tree.getSpan(index);

        try switch (data) {
            .program => |d| self.writeProgram(d, span),
            .directive => |d| self.writeDirective(d, span),
            .function => |d| self.writeFunction(d, span),
            .function_body => |d| self.writeFunctionBody(d, span),
            .formal_parameters => |d| self.writeFormalParameters(d, span),
            .formal_parameter => |d| self.writeFormalParameter(d),
            .expression_statement => |d| self.writeExpressionStatement(d, span),
            .variable_declaration => |d| self.writeVariableDeclaration(d, span),
            .variable_declarator => |d| self.writeVariableDeclarator(d, span),
            .binary_expression => |d| self.writeBinaryExpression(d, span),
            .logical_expression => |d| self.writeLogicalExpression(d, span),
            .unary_expression => |d| self.writeUnaryExpression(d, span),
            .update_expression => |d| self.writeUpdateExpression(d, span),
            .assignment_expression => |d| self.writeAssignmentExpression(d, span),
            .array_expression => |d| self.writeArrayExpression(d, span),
            .object_expression => |d| self.writeObjectExpression(d, span),
            .spread_element => |d| self.writeSpreadElement(d, span),
            .object_property => |d| self.writeObjectProperty(d, span),
            .template_literal => |d| self.writeTemplateLiteral(d, span),
            .template_element => |d| self.writeTemplateElement(d, span),
            .string_literal => |d| self.writeStringLiteral(d, span),
            .numeric_literal => |d| self.writeNumericLiteral(d, span),
            .bigint_literal => |d| self.writeBigIntLiteral(d, span),
            .boolean_literal => |d| self.writeBooleanLiteral(d, span),
            .null_literal => self.writeNullLiteral(span),
            .regexp_literal => |d| self.writeRegExpLiteral(d, span),
            .identifier_reference => |d| self.writeIdentifier(d, span),
            .binding_identifier => |d| self.writeIdentifier(d, span),
            .identifier_name => |d| self.writeIdentifier(d, span),
            .private_identifier => |d| self.writePrivateIdentifier(d, span),
            .assignment_pattern => |d| self.writeAssignmentPattern(d, span),
            .array_pattern => |d| self.writeArrayPattern(d, span),
            .object_pattern => |d| self.writeObjectPattern(d, span),
            .binding_property => |d| self.writeBindingProperty(d, span),
            .binding_rest_element => |d| self.writeRestElement(d, span),
            .parenthesized_expression => |d| self.writeParenthesizedExpression(d, span),
            .arrow_function_expression => |d| self.writeArrowFunctionExpression(d, span),
            .sequence_expression => |d| self.writeSequenceExpression(d, span),
        };
    }

    fn writeProgram(self: *Self, data: ast.Program, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Program");
        try self.fieldSpan(span);
        try self.fieldString("sourceType", if (data.source_type == .module) "module" else "script");
        try self.field("body");
        try self.beginArray();
        for (self.getExtra(data.directives)) |idx| try self.elemNode(idx);
        for (self.getExtra(data.body)) |idx| try self.elemNode(idx);
        try self.endArray();
        try self.endObject();
    }

    fn writeDirective(self: *Self, data: ast.Directive, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ExpressionStatement");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.fieldString("directive", self.tree.source[data.value_start..][0..data.value_len]);
        try self.endObject();
    }

    fn writeFunction(self: *Self, data: ast.Function, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType(switch (data.type) {
            .function_declaration => "FunctionDeclaration",
            .function_expression => "FunctionExpression",
            .ts_declare_function => "TSDeclareFunction",
            .ts_empty_body_function_expression => "TSEmptyBodyFunctionExpression",
        });
        try self.fieldSpan(span);
        try self.fieldNode("id", data.id);
        try self.fieldBool("generator", data.generator);
        try self.fieldBool("async", data.async);
        if (self.isTs) try self.fieldBool("declare", data.type == .ts_declare_function);
        try self.field("params");
        try self.writeFunctionParams(data.params);
        try self.fieldNode("body", data.body);
        try self.fieldBool("expression", false);
        try self.endObject();
    }

    fn writeFunctionParams(self: *Self, params_index: ast.NodeIndex) !void {
        try self.beginArray();
        if (!ast.isNull(params_index)) {
            const params = self.tree.getData(params_index).formal_parameters;
            for (self.getExtra(params.items)) |idx| {
                try self.elemNode(self.tree.getData(idx).formal_parameter.pattern);
            }
            if (!ast.isNull(params.rest)) try self.elemNode(params.rest);
        }
        try self.endArray();
    }

    fn writeFunctionBody(self: *Self, data: ast.FunctionBody, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("BlockStatement");
        try self.fieldSpan(span);
        try self.field("body");
        try self.beginArray();
        for (self.getExtra(data.directives)) |idx| try self.elemNode(idx);
        for (self.getExtra(data.statements)) |idx| try self.elemNode(idx);
        try self.endArray();
        try self.endObject();
    }

    fn writeFormalParameters(self: *Self, data: ast.FormalParameters, span: ast.Span) !void {
        _ = span;
        try self.beginArray();
        for (self.getExtra(data.items)) |idx| {
            try self.elemNode(self.tree.getData(idx).formal_parameter.pattern);
        }
        if (!ast.isNull(data.rest)) try self.elemNode(data.rest);
        try self.endArray();
    }

    fn writeFormalParameter(self: *Self, data: ast.FormalParameter) !void {
        try self.writeNode(data.pattern);
    }

    fn writeExpressionStatement(self: *Self, data: ast.ExpressionStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ExpressionStatement");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.endObject();
    }

    fn writeVariableDeclaration(self: *Self, data: ast.VariableDeclaration, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("VariableDeclaration");
        try self.fieldSpan(span);
        try self.fieldString("kind", data.kind.toString());
        try self.fieldNodeArray("declarations", data.declarators);
        try self.endObject();
    }

    fn writeVariableDeclarator(self: *Self, data: ast.VariableDeclarator, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("VariableDeclarator");
        try self.fieldSpan(span);
        try self.fieldNode("id", data.id);
        try self.fieldNode("init", data.init);
        try self.endObject();
    }

    fn writeBinaryExpression(self: *Self, data: ast.BinaryExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("BinaryExpression");
        try self.fieldSpan(span);
        try self.fieldNode("left", data.left);
        try self.fieldString("operator", data.operator.toString());
        try self.fieldNode("right", data.right);
        try self.endObject();
    }

    fn writeLogicalExpression(self: *Self, data: ast.LogicalExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("LogicalExpression");
        try self.fieldSpan(span);
        try self.fieldNode("left", data.left);
        try self.fieldString("operator", data.operator.toString());
        try self.fieldNode("right", data.right);
        try self.endObject();
    }

    fn writeUnaryExpression(self: *Self, data: ast.UnaryExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("UnaryExpression");
        try self.fieldSpan(span);
        try self.fieldString("operator", data.operator.toString());
        try self.fieldBool("prefix", true);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeUpdateExpression(self: *Self, data: ast.UpdateExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("UpdateExpression");
        try self.fieldSpan(span);
        try self.fieldString("operator", data.operator.toString());
        try self.fieldBool("prefix", data.prefix);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeAssignmentExpression(self: *Self, data: ast.AssignmentExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("AssignmentExpression");
        try self.fieldSpan(span);
        try self.fieldString("operator", data.operator.toString());
        try self.fieldNode("left", data.left);
        try self.fieldNode("right", data.right);
        try self.endObject();
    }

    fn writeArrayExpression(self: *Self, data: ast.ArrayExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ArrayExpression");
        try self.fieldSpan(span);
        try self.fieldNodeArray("elements", data.elements);
        try self.endObject();
    }

    fn writeObjectExpression(self: *Self, data: ast.ObjectExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ObjectExpression");
        try self.fieldSpan(span);
        try self.fieldNodeArray("properties", data.properties);
        try self.endObject();
    }

    fn writeSpreadElement(self: *Self, data: ast.SpreadElement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("SpreadElement");
        try self.fieldSpan(span);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeObjectProperty(self: *Self, data: ast.ObjectProperty, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Property");
        try self.fieldSpan(span);
        try self.fieldString("kind", data.kind.toString());
        try self.fieldNode("key", data.key);
        try self.fieldNode("value", data.value);
        try self.fieldBool("method", data.method);
        try self.fieldBool("shorthand", data.shorthand);
        try self.fieldBool("computed", data.computed);
        try self.endObject();
    }

    fn writeTemplateLiteral(self: *Self, data: ast.TemplateLiteral, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("TemplateLiteral");
        try self.fieldSpan(span);
        try self.fieldNodeArray("quasis", data.quasis);
        try self.fieldNodeArray("expressions", data.expressions);
        try self.endObject();
    }

    fn writeTemplateElement(self: *Self, data: ast.TemplateElement, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];
        try self.beginObject();
        try self.fieldType("TemplateElement");
        try self.fieldSpan(span);
        try self.field("value");
        try self.beginObject();
        try self.fieldString("raw", raw);
        try self.field("cooked");
        try self.writeDecodedString(raw);
        try self.endObject();
        try self.fieldBool("tail", data.tail);
        try self.endObject();
    }

    fn writeStringLiteral(self: *Self, data: ast.StringLiteral, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.field("value");
        try self.writeDecodedString(raw[1 .. raw.len - 1]);
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeNumericLiteral(self: *Self, data: ast.NumericLiteral, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.fieldRaw("value", raw);
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeBigIntLiteral(self: *Self, data: ast.BigIntLiteral, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.fieldNull("value");
        try self.fieldString("raw", raw);
        try self.fieldString("bigint", raw[0 .. raw.len - 1]);
        try self.endObject();
    }

    fn writeBooleanLiteral(self: *Self, data: ast.BooleanLiteral, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.fieldBool("value", data.value);
        try self.fieldString("raw", if (data.value) "true" else "false");
        try self.endObject();
    }

    fn writeNullLiteral(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.fieldNull("value");
        try self.fieldString("raw", "null");
        try self.endObject();
    }

    fn writeRegExpLiteral(self: *Self, data: ast.RegExpLiteral, span: ast.Span) !void {
        const pattern = self.tree.source[data.pattern_start..][0..data.pattern_len];
        const flags = self.tree.source[data.flags_start..][0..data.flags_len];
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.fieldEmptyObject("value");
        try self.field("raw");
        try self.writeByte('"');
        try self.writeByte('/');
        try self.writeJsonEscaped(pattern);
        try self.writeByte('/');
        try self.write(flags);
        try self.writeByte('"');
        try self.field("regex");
        try self.beginObject();
        try self.fieldString("pattern", pattern);
        try self.fieldString("flags", flags);
        try self.endObject();
        try self.endObject();
    }

    fn writeIdentifier(self: *Self, data: anytype, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Identifier");
        try self.fieldSpan(span);
        try self.field("name");
        try self.writeDecodedString(self.tree.source[data.name_start..][0..data.name_len]);
        try self.endObject();
    }

    fn writePrivateIdentifier(self: *Self, data: ast.PrivateIdentifier, span: ast.Span) !void {
        const name = self.tree.source[data.name_start..][0..data.name_len];
        try self.beginObject();
        try self.fieldType("PrivateIdentifier");
        try self.fieldSpan(span);
        try self.field("name");
        try self.writeDecodedString(name[1..]);
        try self.endObject();
    }

    fn writeAssignmentPattern(self: *Self, data: ast.AssignmentPattern, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("AssignmentPattern");
        try self.fieldSpan(span);
        try self.fieldNode("left", data.left);
        try self.fieldNode("right", data.right);
        try self.endObject();
    }

    fn writeArrayPattern(self: *Self, data: ast.ArrayPattern, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ArrayPattern");
        try self.fieldSpan(span);
        try self.field("elements");
        try self.beginArray();
        for (self.getExtra(data.elements)) |idx| try self.elemNode(idx);
        if (!ast.isNull(data.rest)) try self.elemNode(data.rest);
        try self.endArray();
        try self.endObject();
    }

    fn writeObjectPattern(self: *Self, data: ast.ObjectPattern, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ObjectPattern");
        try self.fieldSpan(span);
        try self.field("properties");
        try self.beginArray();
        for (self.getExtra(data.properties)) |idx| try self.elemNode(idx);
        if (!ast.isNull(data.rest)) try self.elemNode(data.rest);
        try self.endArray();
        try self.endObject();
    }

    fn writeBindingProperty(self: *Self, data: ast.BindingProperty, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Property");
        try self.fieldSpan(span);
        try self.fieldString("kind", "init");
        try self.fieldNode("key", data.key);
        try self.fieldNode("value", data.value);
        try self.fieldBool("method", false);
        try self.fieldBool("shorthand", data.shorthand);
        try self.fieldBool("computed", data.computed);
        try self.endObject();
    }

    fn writeRestElement(self: *Self, data: ast.BindingRestElement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("RestElement");
        try self.fieldSpan(span);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeParenthesizedExpression(self: *Self, data: ast.ParenthesizedExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ParenthesizedExpression");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.endObject();
    }

    fn writeArrowFunctionExpression(self: *Self, data: ast.ArrowFunctionExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ArrowFunctionExpression");
        try self.fieldSpan(span);
        try self.fieldNode("id", ast.null_node);
        try self.fieldBool("generator", false);
        try self.fieldBool("async", data.async);
        try self.field("params");
        try self.writeFunctionParams(data.params);
        try self.fieldNode("body", data.body);
        try self.fieldBool("expression", data.expression);
        try self.endObject();
    }

    fn writeSequenceExpression(self: *Self, data: ast.SequenceExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("SequenceExpression");
        try self.fieldSpan(span);
        try self.fieldNodeArray("expressions", data.expressions);
        try self.endObject();
    }

    fn writeComments(self: *Self) !void {
        try self.beginArray();
        for (self.tree.comments.items) |comment| {
            try self.sep();
            if (self.options.pretty) {
                try self.writeByte('\n');
                try self.writeIndent();
            }
            try self.beginObject();
            try self.fieldString("type", comment.type.toString());
            try self.fieldString("value", comment.getValue(self.tree.source));
            try self.fieldPos("start", comment.start);
            try self.fieldPos("end", comment.end);
            try self.endObject();
        }
        try self.endArray();
    }

    fn writeDiagnostics(self: *Self) !void {
        try self.beginArray();
        for (self.tree.diagnostics.items) |diag| {
            try self.sep();
            if (self.options.pretty) {
                try self.writeByte('\n');
                try self.writeIndent();
            }
            try self.beginObject();
            try self.fieldString("severity", diag.severity.toString());
            try self.fieldString("message", diag.message);
            try self.field("help");
            if (diag.help) |h| try self.writeString(h) else try self.writeNull();
            try self.fieldPos("start", diag.span.start);
            try self.fieldPos("end", diag.span.end);
            try self.field("labels");
            try self.beginArray();
            for (diag.labels) |lbl| {
                try self.sep();
                if (self.options.pretty) {
                    try self.writeByte('\n');
                    try self.writeIndent();
                }
                try self.beginObject();
                try self.fieldPos("start", lbl.span.start);
                try self.fieldPos("end", lbl.span.end);
                try self.fieldString("message", lbl.message);
                try self.endObject();
            }
            try self.endArray();
            try self.endObject();
        }
        try self.endArray();
    }

    inline fn write(self: *Self, bytes: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, bytes);
    }

    inline fn writeByte(self: *Self, byte: u8) !void {
        try self.buffer.append(self.allocator, byte);
    }

    fn sep(self: *Self) !void {
        if (self.needs_comma[self.depth]) try self.writeByte(',');
        self.needs_comma[self.depth] = true;
    }

    fn beginObject(self: *Self) !void {
        try self.writeByte('{');
        self.depth += 1;
        self.needs_comma[self.depth] = false;
    }

    fn endObject(self: *Self) !void {
        self.depth -= 1;
        if (self.options.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
        }
        try self.writeByte('}');
    }

    fn beginArray(self: *Self) !void {
        try self.writeByte('[');
        self.depth += 1;
        self.needs_comma[self.depth] = false;
    }

    fn endArray(self: *Self) !void {
        self.depth -= 1;
        if (self.options.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
        }
        try self.writeByte(']');
    }

    fn field(self: *Self, key: []const u8) !void {
        try self.sep();
        if (self.options.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
        }
        try self.writeByte('"');
        try self.write(key);
        try self.write("\":");
        if (self.options.pretty) try self.writeByte(' ');
    }

    fn fieldType(self: *Self, type_name: []const u8) !void {
        try self.field("type");
        try self.writeString(type_name);
    }

    fn fieldSpan(self: *Self, span: ast.Span) !void {
        try self.fieldPos("start", span.start);
        try self.fieldPos("end", span.end);
    }

    fn fieldPos(self: *Self, key: []const u8, byte_pos: u32) !void {
        try self.field(key);
        try self.writeInt(self.pos_map[@min(byte_pos, self.pos_map.len - 1)]);
    }

    fn fieldNode(self: *Self, key: []const u8, node: ast.NodeIndex) !void {
        try self.field(key);
        try self.writeNode(node);
    }

    fn fieldBool(self: *Self, key: []const u8, value: bool) !void {
        try self.field(key);
        try self.write(if (value) "true" else "false");
    }

    fn fieldString(self: *Self, key: []const u8, value: []const u8) !void {
        try self.field(key);
        try self.writeString(value);
    }

    fn fieldNull(self: *Self, key: []const u8) !void {
        try self.field(key);
        try self.writeNull();
    }

    fn fieldEmptyObject(self: *Self, key: []const u8) !void {
        try self.field(key);
        try self.write("{}");
    }

    fn fieldRaw(self: *Self, key: []const u8, value: []const u8) !void {
        try self.field(key);
        try self.write(value);
    }

    fn fieldNodeArray(self: *Self, key: []const u8, range: ast.IndexRange) !void {
        try self.field(key);
        try self.beginArray();
        for (self.getExtra(range)) |idx| try self.elemNode(idx);
        try self.endArray();
    }

    fn elemNode(self: *Self, node: ast.NodeIndex) !void {
        try self.sep();
        if (self.options.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
        }
        try self.writeNode(node);
    }

    fn writeString(self: *Self, s: []const u8) !void {
        try self.writeByte('"');
        try self.writeJsonEscaped(s);
        try self.writeByte('"');
    }

    fn writeJsonEscaped(self: *Self, s: []const u8) !void {
        for (s) |c| {
            switch (c) {
                '"' => try self.write("\\\""),
                '\\' => try self.write("\\\\"),
                '\n' => try self.write("\\n"),
                '\r' => try self.write("\\r"),
                '\t' => try self.write("\\t"),
                0x08 => try self.write("\\b"),
                0x0C => try self.write("\\f"),
                else => if (c < 0x20) {
                    var buf: [6]u8 = undefined;
                    try self.write(std.fmt.bufPrint(&buf, "\\u{x:0>4}", .{c}) catch unreachable);
                } else {
                    try self.writeByte(c);
                },
            }
        }
    }

    fn writeInt(self: *Self, value: u32) !void {
        var buf: [16]u8 = undefined;
        try self.write(std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable);
    }

    fn writeNull(self: *Self) !void {
        try self.write("null");
    }

    fn writeIndent(self: *Self) !void {
        var remaining = self.depth * self.options.indent_size;
        const spaces = "                                        ";
        while (remaining > 0) {
            const chunk = @min(remaining, spaces.len);
            try self.write(spaces[0..chunk]);
            remaining -= chunk;
        }
    }

    inline fn getExtra(self: *const Self, range: ast.IndexRange) []const ast.NodeIndex {
        return self.tree.getExtra(range);
    }


    fn writeDecodedString(self: *Self, s: []const u8) !void {
        self.scratch.clearRetainingCapacity();
        try decodeEscapes(s, &self.scratch, self.allocator);
        try self.writeByte('"');
        try self.writeJsonEscaped(self.scratch.items);
        try self.writeByte('"');
    }
};

fn buildUtf16PosMap(allocator: std.mem.Allocator, source: []const u8) ![]u32 {
    var map = try allocator.alloc(u32, source.len + 1);
    var byte_pos: usize = 0;
    var utf16_pos: u32 = 0;

    while (byte_pos < source.len) {
        map[byte_pos] = utf16_pos;
        const len = std.unicode.utf8ByteSequenceLength(source[byte_pos]) catch 1;
        utf16_pos += if (len == 4) 2 else 1; // surrogate pair for 4-byte sequences
        for (1..len) |i| {
            if (byte_pos + i < source.len) map[byte_pos + i] = utf16_pos;
        }
        byte_pos += len;
    }
    map[source.len] = utf16_pos;
    return map;
}


fn decodeEscapes(input: []const u8, out: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
    var i: usize = 0;
    while (i < input.len) {
        if (input[i] != '\\') {
            try out.append(allocator, input[i]);
            i += 1;
            continue;
        }

        i += 1;
        if (i >= input.len) {
            try out.append(allocator, '\\');
            break;
        }

        switch (input[i]) {
            'n' => {
                try out.append(allocator, '\n');
                i += 1;
            },
            'r' => {
                try out.append(allocator, '\r');
                i += 1;
            },
            't' => {
                try out.append(allocator, '\t');
                i += 1;
            },
            'b' => {
                try out.append(allocator, 0x08);
                i += 1;
            },
            'f' => {
                try out.append(allocator, 0x0C);
                i += 1;
            },
            'v' => {
                try out.append(allocator, 0x0B);
                i += 1;
            },
            '0' => {
                if (i + 1 < input.len and input[i + 1] >= '0' and input[i + 1] <= '9') {
                    const r = parseOctal(input, i);
                    try appendUtf8(out, allocator, r.value);
                    i = r.end;
                } else {
                    try out.append(allocator, 0);
                    i += 1;
                }
            },
            '1'...'7' => {
                const r = parseOctal(input, i);
                try appendUtf8(out, allocator, r.value);
                i = r.end;
            },
            'x' => {
                i += 1;
                if (i + 2 <= input.len) {
                    if (hexVal(input[i])) |hi| {
                        if (hexVal(input[i + 1])) |lo| {
                            try appendUtf8(out, allocator, (@as(u21, hi) << 4) | lo);
                            i += 2;
                            continue;
                        }
                    }
                }
                try out.append(allocator, 'x');
            },
            'u' => {
                i += 1;
                if (i < input.len and input[i] == '{') {
                    i += 1;
                    var cp: u21 = 0;
                    var has_digits = false;
                    while (i < input.len and input[i] != '}') {
                        if (hexVal(input[i])) |d| {
                            cp = (cp << 4) | d;
                            has_digits = true;
                            i += 1;
                        } else break;
                    }
                    if (has_digits and i < input.len and input[i] == '}') {
                        i += 1;
                        try appendUtf8(out, allocator, cp);
                    } else {
                        try out.append(allocator, 'u');
                    }
                } else if (i + 4 <= input.len) {
                    var cp: u21 = 0;
                    var valid = true;
                    for (0..4) |j| {
                        if (hexVal(input[i + j])) |d| {
                            cp = (cp << 4) | d;
                        } else {
                            valid = false;
                            break;
                        }
                    }
                    if (valid) {
                        i += 4;
                        try appendUtf8(out, allocator, cp);
                    } else {
                        try out.append(allocator, 'u');
                    }
                } else {
                    try out.append(allocator, 'u');
                }
            },
            '\r' => {
                i += 1;
                if (i < input.len and input[i] == '\n') i += 1;
            },
            '\n' => i += 1,
            else => |c| {
                try out.append(allocator, c);
                i += 1;
            },
        }
    }
}

fn parseOctal(input: []const u8, start: usize) struct { value: u21, end: usize } {
    var value: u16 = 0;
    var i = start;
    const max: usize = if (input[start] <= '3') 3 else 2;
    var count: usize = 0;
    while (i < input.len and count < max) : (i += 1) {
        if (input[i] >= '0' and input[i] <= '7') {
            value = value * 8 + (input[i] - '0');
            count += 1;
        } else break;
    }
    return .{ .value = value, .end = i };
}

fn hexVal(c: u8) ?u8 {
    return if (c >= '0' and c <= '9') c - '0' else if (c >= 'a' and c <= 'f') c - 'a' + 10 else if (c >= 'A' and c <= 'F') c - 'A' + 10 else null;
}

fn appendUtf8(out: *std.ArrayList(u8), allocator: std.mem.Allocator, cp: u21) !void {
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(cp, &buf) catch {
        try out.appendSlice(allocator, "\u{FFFD}");
        return;
    };
    try out.appendSlice(allocator, buf[0..len]);
}

pub fn toJSON(tree: *const parser.ParseTree, allocator: std.mem.Allocator, options: EstreeJsonOptions) ![]u8 {
    return Serializer.serialize(tree, allocator, options);
}
