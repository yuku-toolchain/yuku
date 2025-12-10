const std = @import("std");
const ast = @import("ast.zig");
const parser = @import("parser.zig");

pub const EstreeJsonOptions = struct {
    /// Whether to pretty-print the JSON output
    pretty: bool = true,
    /// Number of spaces per indentation level (default: 2)
    indent_size: u32 = 2,
};

pub const Serializer = struct {
    tree: *const parser.ParseTree,
    buffer: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    depth: u32 = 0,
    options: EstreeJsonOptions,
    needs_comma: [max_depth]bool = [_]bool{false} ** max_depth,
    /// Scratch buffer for decoding escape sequences - reused to avoid allocations
    scratch: std.ArrayList(u8) = .empty,

    const Self = @This();
    const Error = error{ NoSpaceLeft, OutOfMemory };
    const max_depth = 64;

    pub fn serialize(tree: *const parser.ParseTree, allocator: std.mem.Allocator, options: EstreeJsonOptions) ![]u8 {
        var buffer: std.ArrayList(u8) = try .initCapacity(allocator, tree.source.len * 3);
        errdefer buffer.deinit(allocator);

        var self = Self{
            .tree = tree,
            .buffer = &buffer,
            .allocator = allocator,
            .options = options,
            .scratch = try .initCapacity(allocator, 256),
        };
        defer self.scratch.deinit(allocator);

        try self.beginObject();
        try self.fieldNode("program", tree.program);
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

        const data = self.tree.nodes.items(.data)[index];
        const span = self.tree.nodes.items(.span)[index];

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
        try self.fieldType(functionTypeToString(data.type));
        try self.fieldSpan(span);
        try self.fieldNode("id", data.id);
        try self.fieldBool("generator", data.generator);
        try self.fieldBool("async", data.async);
        if(data.type == .ts_declare_function) {
            try self.fieldBool("declare", true);
        }
        try self.field("params");
        try self.writeFunctionParams(data.params);
        try self.fieldNode("body", data.body);
        try self.fieldBool("expression", false);
        try self.endObject();
    }

    fn functionTypeToString(ft: ast.FunctionType) []const u8 {
        return switch (ft) {
            .function_declaration => "FunctionDeclaration",
            .function_expression => "FunctionExpression",
            .ts_declare_function => "TSDeclareFunction",
            .ts_empty_body_function_expression => "TSEmptyBodyFunctionExpression",
        };
    }

    fn writeFunctionParams(self: *Self, params_index: ast.NodeIndex) !void {
        try self.beginArray();
        if (!ast.isNull(params_index)) {
            const params = self.tree.nodes.items(.data)[params_index].formal_parameters;
            for (self.getExtra(params.items)) |idx| {
                const param = self.tree.nodes.items(.data)[idx].formal_parameter;
                try self.elemNode(param.pattern);
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
            const param = self.tree.nodes.items(.data)[idx].formal_parameter;
            try self.elemNode(param.pattern);
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
        const content = raw[1 .. raw.len - 1]; // strip quotes
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.field("value");
        try self.writeDecodedString(content);
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeNumericLiteral(self: *Self, data: ast.NumericLiteral, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.fieldRaw("value", raw); // TODO: proper numeric parsing
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeBigIntLiteral(self: *Self, data: ast.BigIntLiteral, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.fieldNull("value"); // big int can't be represented in JSON
        try self.fieldString("raw", raw);
        try self.fieldString("bigint", raw[0 .. raw.len - 1]); // strip 'n' suffix
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
        try self.fieldEmptyObject("value"); // regex objects can't be serialized to JSON
        try self.field("raw");
        try self.writeByte('"');
        try self.writeByte('/');
        try self.writeEscaped(pattern);
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
        const name = self.tree.source[data.name_start..][0..data.name_len];
        try self.beginObject();
        try self.fieldType("Identifier");
        try self.fieldSpan(span);
        try self.field("name");
        try self.writeDecodedString(name);
        try self.endObject();
    }

    fn writePrivateIdentifier(self: *Self, data: ast.PrivateIdentifier, span: ast.Span) !void {
        const name = self.tree.source[data.name_start..][0..data.name_len];
        try self.beginObject();
        try self.fieldType("PrivateIdentifier");
        try self.fieldSpan(span);
        try self.field("name");
        try self.writeDecodedString(name[1..]); // strip '#' prefix
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
        try self.fieldNode("id", ast.null_node); // arrow functions are always anonymous
        try self.fieldBool("generator", false); // arrow functions cannot be generators
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
            if (diag.help) |help| {
                try self.writeString(help);
            } else {
                try self.writeNull();
            }
            try self.fieldInt("start", diag.span.start);
            try self.fieldInt("end", diag.span.end);
            try self.field("labels");
            try self.beginArray();
            for (diag.labels) |lbl| {
                try self.sep();
                if (self.options.pretty) {
                    try self.writeByte('\n');
                    try self.writeIndent();
                }
                try self.beginObject();
                try self.fieldInt("start", lbl.span.start);
                try self.fieldInt("end", lbl.span.end);
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
        if (self.depth >= max_depth) unreachable; // nesting too deep
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
        if (self.depth >= max_depth) unreachable; // nesting too deep
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
        try self.fieldInt("start", span.start);
        try self.fieldInt("end", span.end);
    }

    fn fieldNode(self: *Self, key: []const u8, node: ast.NodeIndex) !void {
        try self.field(key);
        try self.writeNode(node);
    }

    fn fieldBool(self: *Self, key: []const u8, value: bool) !void {
        try self.field(key);
        try self.writeBool(value);
    }

    fn fieldString(self: *Self, key: []const u8, value: []const u8) !void {
        try self.field(key);
        try self.writeString(value);
    }

    fn fieldInt(self: *Self, key: []const u8, value: u32) !void {
        try self.field(key);
        try self.writeInt(value);
    }

    fn fieldNull(self: *Self, key: []const u8) !void {
        try self.field(key);
        try self.writeNull();
    }

    fn fieldEmptyObject(self: *Self, key: []const u8) !void {
        try self.field(key);
        try self.writeByte('{');
        try self.writeByte('}');
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
        try self.writeEscaped(s);
        try self.writeByte('"');
    }

    fn writeEscaped(self: *Self, s: []const u8) !void {
        for (s) |c| {
            switch (c) {
                '"' => try self.write("\\\""),
                '\\' => try self.write("\\\\"),
                '\n' => try self.write("\\n"),
                '\r' => try self.write("\\r"),
                '\t' => try self.write("\\t"),
                0x08 => try self.write("\\b"),
                0x0C => try self.write("\\f"),
                else => {
                    if (c < 0x20) {
                        var buf: [6]u8 = undefined;
                        const len = std.fmt.bufPrint(&buf, "\\u{x:0>4}", .{c}) catch unreachable;
                        try self.write(len);
                    } else {
                        try self.writeByte(c);
                    }
                },
            }
        }
    }

    fn writeInt(self: *Self, value: u32) !void {
        var buf: [16]u8 = undefined;
        const len = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable;
        try self.write(len);
    }

    fn writeBool(self: *Self, value: bool) !void {
        try self.write(if (value) "true" else "false");
    }

    fn writeNull(self: *Self) !void {
        try self.write("null");
    }

    fn writeIndent(self: *Self) !void {
        const indent_size = self.depth * self.options.indent_size;
        const spaces = "                                        "; // 40 spaces
        var remaining = indent_size;
        while (remaining > 0) {
            const chunk = @min(remaining, spaces.len);
            try self.write(spaces[0..chunk]);
            remaining -= chunk;
        }
    }

    inline fn getExtra(self: *const Self, range: ast.IndexRange) []const ast.NodeIndex {
        return self.tree.extra.items[range.start..][0..range.len];
    }

    fn writeDecodedString(self: *Self, s: []const u8) !void {
        self.scratch.clearRetainingCapacity();
        try self.decodeEscapes(s, &self.scratch);
        try self.writeByte('"');
        try self.writeEscaped(self.scratch.items);
        try self.writeByte('"');
    }

    fn decodeEscapes(self: *Self, input: []const u8, out: *std.ArrayList(u8)) !void {
        var i: usize = 0;
        while (i < input.len) {
            const c = input[i];
            if (c != '\\') {
                try out.append(self.allocator, c);
                i += 1;
                continue;
            }

            // handle escape sequence
            i += 1;
            if (i >= input.len) {
                try out.append(self.allocator, '\\');
                break;
            }

            const next = input[i];
            switch (next) {
                'n' => {
                    try out.append(self.allocator, '\n');
                    i += 1;
                },
                'r' => {
                    try out.append(self.allocator, '\r');
                    i += 1;
                },
                't' => {
                    try out.append(self.allocator, '\t');
                    i += 1;
                },
                'b' => {
                    try out.append(self.allocator, 0x08);
                    i += 1;
                },
                'f' => {
                    try out.append(self.allocator, 0x0C);
                    i += 1;
                },
                'v' => {
                    try out.append(self.allocator, 0x0B);
                    i += 1;
                },
                '0' => {
                    // \0 is null char (but not octal if followed by digit 0-9)
                    if (i + 1 < input.len and input[i + 1] >= '0' and input[i + 1] <= '9') {
                        // Octal escape - parse up to 3 digits
                        const octal_result = parseOctalEscape(input, i);
                        try out.append(self.allocator, octal_result.value);
                        i = octal_result.end;
                    } else {
                        try out.append(self.allocator, 0);
                        i += 1;
                    }
                },
                '1'...'7' => {
                    // octal escape
                    const octal_result = parseOctalEscape(input, i);
                    try out.append(self.allocator, octal_result.value);
                    i = octal_result.end;
                },
                'x' => {
                    // \xHH hex escape - produces code point U+00HH (Latin-1)
                    i += 1;
                    if (i + 2 <= input.len) {
                        if (parseHexDigit(input[i])) |hi| {
                            if (parseHexDigit(input[i + 1])) |lo| {
                                const cp: u21 = (@as(u21, hi) << 4) | lo;
                                try appendCodePoint(out, self.allocator, cp);
                                i += 2;
                                continue;
                            }
                        }
                    }
                    // invalid hex escape - output as-is
                    try out.append(self.allocator, 'x');
                },
                'u' => {
                    i += 1;
                    if (i < input.len and input[i] == '{') {
                        // \u{HHHH...} code point escape
                        i += 1;
                        var cp: u21 = 0;
                        var has_digits = false;
                        while (i < input.len and input[i] != '}') {
                            if (parseHexDigit(input[i])) |d| {
                                cp = (cp << 4) | d;
                                has_digits = true;
                                i += 1;
                            } else break;
                        }
                        if (has_digits and i < input.len and input[i] == '}') {
                            i += 1;
                            try appendCodePoint(out, self.allocator, cp);
                        } else {
                            // invalid - output 'u'
                            try out.append(self.allocator, 'u');
                        }
                    } else if (i + 4 <= input.len) {
                        // \uHHHH unicode escape
                        var cp: u21 = 0;
                        var valid = true;
                        for (0..4) |j| {
                            if (parseHexDigit(input[i + j])) |d| {
                                cp = (cp << 4) | d;
                            } else {
                                valid = false;
                                break;
                            }
                        }
                        if (valid) {
                            i += 4;
                            try appendCodePoint(out, self.allocator, cp);
                        } else {
                            try out.append(self.allocator, 'u');
                        }
                    } else {
                        try out.append(self.allocator, 'u');
                    }
                },
                '\r' => {
                    // line continuation - skip CR and optional LF
                    i += 1;
                    if (i < input.len and input[i] == '\n') {
                        i += 1;
                    }
                },
                '\n' => {
                    // line continuation - skip LF
                    i += 1;
                },
                else => {
                    // identity escape (including \\, \', \")
                    try out.append(self.allocator, next);
                    i += 1;
                },
            }
        }
    }

    fn parseOctalEscape(input: []const u8, start: usize) struct { value: u8, end: usize } {
        var value: u16 = 0;
        var i = start;
        var count: usize = 0;
        const max_count: usize = if (input[start] <= '3') 3 else 2;

        while (i < input.len and count < max_count) {
            const c = input[i];
            if (c >= '0' and c <= '7') {
                value = value * 8 + (c - '0');
                i += 1;
                count += 1;
            } else break;
        }
        return .{ .value = @truncate(value), .end = i };
    }

    fn parseHexDigit(c: u8) ?u8 {
        if (c >= '0' and c <= '9') return c - '0';
        if (c >= 'a' and c <= 'f') return c - 'a' + 10;
        if (c >= 'A' and c <= 'F') return c - 'A' + 10;
        return null;
    }

    fn appendCodePoint(out: *std.ArrayList(u8), allocator: std.mem.Allocator, cp: u21) !void {
        var buf: [4]u8 = undefined;
        const len = std.unicode.utf8Encode(cp, &buf) catch {
            // invalid code point, use replacement character
            try out.appendSlice(allocator, "\u{FFFD}");
            return;
        };
        try out.appendSlice(allocator, buf[0..len]);
    }
};

/// Serialize a ParseTree to ESTree-compatible JSON.
/// Caller owns the returned memory and must free it.
pub fn toJSON(tree: *const parser.ParseTree, allocator: std.mem.Allocator, options: EstreeJsonOptions) ![]u8 {
    return Serializer.serialize(tree, allocator, options);
}
