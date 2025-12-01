const std = @import("std");
const ast = @import("ast.zig");
const parser = @import("parser.zig");

pub const Serializer = struct {
    tree: *const parser.ParseTree,
    buffer: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    depth: u32 = 0,
    pretty: bool = true,

    const Self = @This();
    const Error = error{OutOfMemory};

    pub fn serialize(tree: *const parser.ParseTree, allocator: std.mem.Allocator, pretty: bool) ![]u8 {
        var buffer: std.ArrayList(u8) = .empty;
        errdefer buffer.deinit(allocator);

        var self = Self{
            .tree = tree,
            .buffer = &buffer,
            .allocator = allocator,
            .pretty = pretty,
        };

        try self.writeNode(tree.program);

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
        };
    }

    fn writeProgram(self: *Self, data: ast.Program, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("Program");
        try self.writeSpan(span);
        try self.writeKey("sourceType");
        try self.writeString(if (data.source_type == .Module) "module" else "script");

        try self.writeKey("body");
        try self.beginArray();

        const directives = self.getExtra(data.directives);
        for (directives, 0..) |dir_idx, i| {
            if (i > 0) try self.writeComma();
            try self.writeNode(dir_idx);
        }

        const statements = self.getExtra(data.body);
        for (statements, 0..) |stmt_idx, i| {
            if (directives.len > 0 or i > 0) try self.writeComma();
            try self.writeNode(stmt_idx);
        }

        try self.endArray();
        try self.endObject();
    }

    fn writeDirective(self: *Self, data: ast.Directive, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("ExpressionStatement");
        try self.writeSpan(span);
        try self.writeKey("expression");
        try self.writeNode(data.expression);
        try self.writeKey("directive");
        try self.writeString(self.tree.source[data.value_start..][0..data.value_len]);
        try self.endObject();
    }

    fn writeFunction(self: *Self, data: ast.Function, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType(@tagName(data.type));
        try self.writeSpan(span);
        try self.writeKey("id");
        try self.writeNode(data.id);
        try self.writeKey("generator");
        try self.writeBool(data.generator);
        try self.writeKey("async");
        try self.writeBool(data.async);
        try self.writeKey("declare");
        try self.writeBool(data.type == .TSDeclareFunction);
        try self.writeKey("params");
        try self.writeFunctionParams(data.params);
        try self.writeKey("body");
        try self.writeNode(data.body);
        try self.writeKey("expression");
        try self.writeBool(false);
        try self.endObject();
    }

    fn writeFunctionParams(self: *Self, params_index: ast.NodeIndex) !void {
        if (ast.isNull(params_index)) {
            try self.beginArray();
            try self.endArray();
            return;
        }

        const data = self.tree.nodes.items(.data)[params_index];
        const params = data.formal_parameters;

        try self.beginArray();

        const items = self.getExtra(params.items);
        for (items, 0..) |param_idx, i| {
            if (i > 0) try self.writeComma();
            const param_data = self.tree.nodes.items(.data)[param_idx];
            try self.writeNode(param_data.formal_parameter.pattern);
        }

        if (!ast.isNull(params.rest)) {
            if (items.len > 0) try self.writeComma();
            try self.writeNode(params.rest);
        }

        try self.endArray();
    }

    fn writeFunctionBody(self: *Self, data: ast.FunctionBody, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("BlockStatement");
        try self.writeSpan(span);
        try self.writeKey("body");
        try self.beginArray();

        const directives = self.getExtra(data.directives);

        for (directives, 0..) |dir_idx, i| {
            if (i > 0) try self.writeComma();
            try self.writeNode(dir_idx);
        }

        const statements = self.getExtra(data.statements);
        for (statements, 0..) |stmt_idx, i| {
            if (directives.len > 0 or i > 0) try self.writeComma();
            try self.writeNode(stmt_idx);
        }

        try self.endArray();
        try self.endObject();
    }

    fn writeFormalParameters(self: *Self, data: ast.FormalParameters, span: ast.Span) !void {
        _ = span;
        try self.beginArray();

        const items = self.getExtra(data.items);
        for (items, 0..) |param_idx, i| {
            if (i > 0) try self.writeComma();
            const param_data = self.tree.nodes.items(.data)[param_idx];
            try self.writeNode(param_data.formal_parameter.pattern);
        }

        if (!ast.isNull(data.rest)) {
            if (items.len > 0) try self.writeComma();
            try self.writeNode(data.rest);
        }

        try self.endArray();
    }

    fn writeFormalParameter(self: *Self, data: ast.FormalParameter) !void {
        try self.writeNode(data.pattern);
    }

    fn writeExpressionStatement(self: *Self, data: ast.ExpressionStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("ExpressionStatement");
        try self.writeSpan(span);
        try self.writeKey("expression");
        try self.writeNode(data.expression);
        try self.endObject();
    }

    fn writeVariableDeclaration(self: *Self, data: ast.VariableDeclaration, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("VariableDeclaration");
        try self.writeSpan(span);
        try self.writeKey("kind");
        try self.writeString(variableKindToString(data.kind));
        try self.writeKey("declarations");
        try self.writeNodeArray(data.declarators);
        try self.endObject();
    }

    fn writeVariableDeclarator(self: *Self, data: ast.VariableDeclarator, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("VariableDeclarator");
        try self.writeSpan(span);
        try self.writeKey("id");
        try self.writeNode(data.id);
        try self.writeKey("init");
        try self.writeNode(data.init);
        try self.endObject();
    }

    fn writeBinaryExpression(self: *Self, data: ast.BinaryExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("BinaryExpression");
        try self.writeSpan(span);
        try self.writeKey("left");
        try self.writeNode(data.left);
        try self.writeKey("operator");
        try self.writeString(binaryOperatorToString(data.operator));
        try self.writeKey("right");
        try self.writeNode(data.right);
        try self.endObject();
    }

    fn writeLogicalExpression(self: *Self, data: ast.LogicalExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("LogicalExpression");
        try self.writeSpan(span);
        try self.writeKey("left");
        try self.writeNode(data.left);
        try self.writeKey("operator");
        try self.writeString(logicalOperatorToString(data.operator));
        try self.writeKey("right");
        try self.writeNode(data.right);
        try self.endObject();
    }

    fn writeUnaryExpression(self: *Self, data: ast.UnaryExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("UnaryExpression");
        try self.writeSpan(span);
        try self.writeKey("operator");
        try self.writeString(unaryOperatorToString(data.operator));
        try self.writeKey("prefix");
        try self.writeBool(true);
        try self.writeKey("argument");
        try self.writeNode(data.argument);
        try self.endObject();
    }

    fn writeUpdateExpression(self: *Self, data: ast.UpdateExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("UpdateExpression");
        try self.writeSpan(span);
        try self.writeKey("operator");
        try self.writeString(updateOperatorToString(data.operator));
        try self.writeKey("prefix");
        try self.writeBool(data.prefix);
        try self.writeKey("argument");
        try self.writeNode(data.argument);
        try self.endObject();
    }

    fn writeAssignmentExpression(self: *Self, data: ast.AssignmentExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("AssignmentExpression");
        try self.writeSpan(span);
        try self.writeKey("operator");
        try self.writeString(assignmentOperatorToString(data.operator));
        try self.writeKey("left");
        try self.writeNode(data.left);
        try self.writeKey("right");
        try self.writeNode(data.right);
        try self.endObject();
    }

    fn writeArrayExpression(self: *Self, data: ast.ArrayExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("ArrayExpression");
        try self.writeSpan(span);
        try self.writeKey("elements");
        try self.writeNodeArray(data.elements);
        try self.endObject();
    }

    fn writeObjectExpression(self: *Self, data: ast.ObjectExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("ObjectExpression");
        try self.writeSpan(span);
        try self.writeKey("properties");
        try self.writeNodeArray(data.properties);
        try self.endObject();
    }

    fn writeSpreadElement(self: *Self, data: ast.SpreadElement, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("SpreadElement");
        try self.writeSpan(span);
        try self.writeKey("argument");
        try self.writeNode(data.argument);
        try self.endObject();
    }

    fn writeObjectProperty(self: *Self, data: ast.ObjectProperty, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("Property");
        try self.writeSpan(span);
        try self.writeKey("kind");
        try self.writeString(propertyKindToString(data.kind));
        try self.writeKey("key");
        try self.writeNode(data.key);
        try self.writeKey("value");
        try self.writeNode(data.value);
        try self.writeKey("method");
        try self.writeBool(false); // TODO: update when methods are implemented
        try self.writeKey("shorthand");
        try self.writeBool(data.shorthand);
        try self.writeKey("computed");
        try self.writeBool(data.computed);
        try self.endObject();
    }

    fn writeTemplateLiteral(self: *Self, data: ast.TemplateLiteral, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("TemplateLiteral");
        try self.writeSpan(span);
        try self.writeKey("quasis");
        try self.writeNodeArray(data.quasis);
        try self.writeKey("expressions");
        try self.writeNodeArray(data.expressions);
        try self.endObject();
    }

    fn writeTemplateElement(self: *Self, data: ast.TemplateElement, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];

        try self.beginObject();
        try self.writeType("TemplateElement");
        try self.writeSpan(span);
        try self.writeKey("value");
        try self.beginObject();
        try self.writeKey("raw");
        try self.writeString(raw);
        try self.writeKey("cooked");
        try self.writeString(raw); // TODO: proper escape processing
        try self.endObject();
        try self.writeKey("tail");
        try self.writeBool(data.tail);
        try self.endObject();
    }

    fn writeStringLiteral(self: *Self, data: ast.StringLiteral, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];
        const value = raw[1 .. raw.len - 1]; // strip quotes

        try self.beginObject();
        try self.writeType("Literal");
        try self.writeSpan(span);
        try self.writeKey("value");
        try self.writeString(value);
        try self.writeKey("raw");
        try self.writeString(raw);
        try self.endObject();
    }

    fn writeNumericLiteral(self: *Self, data: ast.NumericLiteral, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];

        try self.beginObject();
        try self.writeType("Literal");
        try self.writeSpan(span);
        try self.writeKey("value");
        try self.writeRaw(self.parseNumericValue(raw));
        try self.writeKey("raw");
        try self.writeString(raw);
        try self.endObject();
    }

    fn writeBigIntLiteral(self: *Self, data: ast.BigIntLiteral, span: ast.Span) !void {
        const raw = self.tree.source[data.raw_start..][0..data.raw_len];
        const bigint = raw[0 .. raw.len - 1]; // strip 'n' suffix

        try self.beginObject();
        try self.writeType("Literal");
        try self.writeSpan(span);
        try self.writeKey("value");
        try self.writeNull(); // big int can't be represented in JSON
        try self.writeKey("raw");
        try self.writeString(raw);
        try self.writeKey("bigint");
        try self.writeString(bigint);
        try self.endObject();
    }

    fn writeBooleanLiteral(self: *Self, data: ast.BooleanLiteral, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("Literal");
        try self.writeSpan(span);
        try self.writeKey("value");
        try self.writeBool(data.value);
        try self.writeKey("raw");
        try self.writeString(if (data.value) "true" else "false");
        try self.endObject();
    }

    fn writeNullLiteral(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("Literal");
        try self.writeSpan(span);
        try self.writeKey("value");
        try self.writeNull();
        try self.writeKey("raw");
        try self.writeString("null");
        try self.endObject();
    }

    fn writeRegExpLiteral(self: *Self, data: ast.RegExpLiteral, span: ast.Span) !void {
        const pattern = self.tree.source[data.pattern_start..][0..data.pattern_len];
        const flags = self.tree.source[data.flags_start..][0..data.flags_len];

        try self.beginObject();
        try self.writeType("Literal");
        try self.writeSpan(span);
        try self.writeKey("value");
        try self.writeNull(); // regex objects can't be serialized to JSON
        try self.writeKey("raw");
        try self.writeByte('"');
        try self.writeByte('/');
        try self.writeEscaped(pattern);
        try self.writeByte('/');
        try self.write(flags);
        try self.writeByte('"');
        try self.writeKey("regex");
        try self.beginObject();
        try self.writeKey("pattern");
        try self.writeString(pattern);
        try self.writeKey("flags");
        try self.writeString(flags);
        try self.endObject();
        try self.endObject();
    }

    fn writeIdentifier(self: *Self, data: anytype, span: ast.Span) !void {
        const name = self.tree.source[data.name_start..][0..data.name_len];

        try self.beginObject();
        try self.writeType("Identifier");
        try self.writeSpan(span);
        try self.writeKey("name");
        try self.writeString(name);
        try self.endObject();
    }

    fn writePrivateIdentifier(self: *Self, data: ast.PrivateIdentifier, span: ast.Span) !void {
        const name = self.tree.source[data.name_start..][0..data.name_len];

        try self.beginObject();
        try self.writeType("PrivateIdentifier");
        try self.writeSpan(span);
        try self.writeKey("name");
        try self.writeString(name[1..]); // strip '#' prefix
        try self.endObject();
    }

    fn writeAssignmentPattern(self: *Self, data: ast.AssignmentPattern, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("AssignmentPattern");
        try self.writeSpan(span);
        try self.writeKey("left");
        try self.writeNode(data.left);
        try self.writeKey("right");
        try self.writeNode(data.right);
        try self.endObject();
    }

    fn writeArrayPattern(self: *Self, data: ast.ArrayPattern, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("ArrayPattern");
        try self.writeSpan(span);
        try self.writeKey("elements");

        try self.beginArray();
        const elements = self.getExtra(data.elements);
        for (elements, 0..) |elem_idx, i| {
            if (i > 0) try self.writeComma();
            try self.writeNode(elem_idx);
        }
        if (!ast.isNull(data.rest)) {
            if (elements.len > 0) try self.writeComma();
            try self.writeNode(data.rest);
        }
        try self.endArray();

        try self.endObject();
    }

    fn writeObjectPattern(self: *Self, data: ast.ObjectPattern, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("ObjectPattern");
        try self.writeSpan(span);
        try self.writeKey("properties");

        try self.beginArray();
        const properties = self.getExtra(data.properties);
        for (properties, 0..) |prop_idx, i| {
            if (i > 0) try self.writeComma();
            try self.writeNode(prop_idx);
        }
        if (!ast.isNull(data.rest)) {
            if (properties.len > 0) try self.writeComma();
            try self.writeNode(data.rest);
        }
        try self.endArray();

        try self.endObject();
    }

    fn writeBindingProperty(self: *Self, data: ast.BindingProperty, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("Property");
        try self.writeSpan(span);
        try self.writeKey("kind");
        try self.writeString("init");
        try self.writeKey("key");
        try self.writeNode(data.key);
        try self.writeKey("value");
        try self.writeNode(data.value);
        try self.writeKey("method");
        try self.writeBool(false);
        try self.writeKey("shorthand");
        try self.writeBool(data.shorthand);
        try self.writeKey("computed");
        try self.writeBool(data.computed);
        try self.endObject();
    }

    fn writeRestElement(self: *Self, data: ast.BindingRestElement, span: ast.Span) !void {
        try self.beginObject();
        try self.writeType("RestElement");
        try self.writeSpan(span);
        try self.writeKey("argument");
        try self.writeNode(data.argument);
        try self.endObject();
    }

    inline fn write(self: *Self, bytes: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, bytes);
    }

    inline fn writeByte(self: *Self, byte: u8) !void {
        try self.buffer.append(self.allocator, byte);
    }

    fn beginObject(self: *Self) !void {
        try self.writeByte('{');
        self.depth += 1;
    }

    fn endObject(self: *Self) !void {
        self.depth -= 1;
        if (self.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
        }
        try self.writeByte('}');
    }

    fn beginArray(self: *Self) !void {
        try self.writeByte('[');
        self.depth += 1;
    }

    fn endArray(self: *Self) !void {
        self.depth -= 1;
        if (self.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
        }
        try self.writeByte(']');
    }

    fn writeKey(self: *Self, key: []const u8) !void {
        try self.writeComma();
        if (self.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
        }
        try self.writeByte('"');
        try self.write(key);
        try self.write("\":");
        if (self.pretty) try self.writeByte(' ');
    }

    fn writeType(self: *Self, type_name: []const u8) !void {
        if (self.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
        }
        try self.write("\"type\":");
        if (self.pretty) try self.writeByte(' ');
        try self.writeByte('"');
        try self.write(type_name);
        try self.writeByte('"');
    }

    fn writeSpan(self: *Self, span: ast.Span) !void {
        try self.writeKey("start");
        try self.writeInt(span.start);
        try self.writeKey("end");
        try self.writeInt(span.end);
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

    fn writeRaw(self: *Self, s: []const u8) !void {
        try self.write(s);
    }

    fn writeComma(self: *Self) !void {
        try self.writeByte(',');
    }

    fn writeIndent(self: *Self) !void {
        for (0..self.depth) |_| {
            try self.write("  ");
        }
    }

    fn writeNodeArray(self: *Self, range: ast.IndexRange) !void {
        try self.beginArray();
        const indices = self.getExtra(range);
        for (indices, 0..) |idx, i| {
            if (i > 0) try self.writeComma();
            try self.writeNode(idx);
        }
        try self.endArray();
    }

    inline fn getExtra(self: *const Self, range: ast.IndexRange) []const ast.NodeIndex {
        return self.tree.extra.items[range.start..][0..range.len];
    }

    fn parseNumericValue(self: *Self, raw: []const u8) []const u8 {
        _ = self;
        // remove underscores and handle prefixes for json compatibility
        // TODO: proper numeric parsing with underscore removal
        return raw;
    }
};

fn binaryOperatorToString(op: ast.BinaryOperator) []const u8 {
    return switch (op) {
        .Equal => "==",
        .NotEqual => "!=",
        .StrictEqual => "===",
        .StrictNotEqual => "!==",
        .LessThan => "<",
        .LessThanOrEqual => "<=",
        .GreaterThan => ">",
        .GreaterThanOrEqual => ">=",
        .Add => "+",
        .Subtract => "-",
        .Multiply => "*",
        .Divide => "/",
        .Modulo => "%",
        .Exponent => "**",
        .BitwiseOr => "|",
        .BitwiseXor => "^",
        .BitwiseAnd => "&",
        .LeftShift => "<<",
        .RightShift => ">>",
        .UnsignedRightShift => ">>>",
        .In => "in",
        .Instanceof => "instanceof",
    };
}

fn logicalOperatorToString(op: ast.LogicalOperator) []const u8 {
    return switch (op) {
        .And => "&&",
        .Or => "||",
        .NullishCoalescing => "??",
    };
}

fn unaryOperatorToString(op: ast.UnaryOperator) []const u8 {
    return switch (op) {
        .Negate => "-",
        .Positive => "+",
        .LogicalNot => "!",
        .BitwiseNot => "~",
        .Typeof => "typeof",
        .Void => "void",
        .Delete => "delete",
    };
}

fn updateOperatorToString(op: ast.UpdateOperator) []const u8 {
    return switch (op) {
        .Increment => "++",
        .Decrement => "--",
    };
}

fn assignmentOperatorToString(op: ast.AssignmentOperator) []const u8 {
    return switch (op) {
        .Assign => "=",
        .AddAssign => "+=",
        .SubtractAssign => "-=",
        .MultiplyAssign => "*=",
        .DivideAssign => "/=",
        .ModuloAssign => "%=",
        .ExponentAssign => "**=",
        .LeftShiftAssign => "<<=",
        .RightShiftAssign => ">>=",
        .UnsignedRightShiftAssign => ">>>=",
        .BitwiseOrAssign => "|=",
        .BitwiseXorAssign => "^=",
        .BitwiseAndAssign => "&=",
        .LogicalOrAssign => "||=",
        .LogicalAndAssign => "&&=",
        .NullishAssign => "??=",
    };
}

fn variableKindToString(kind: ast.VariableKind) []const u8 {
    return switch (kind) {
        .Var => "var",
        .Let => "let",
        .Const => "const",
        .Using => "using",
        .AwaitUsing => "await using",
    };
}

fn propertyKindToString(kind: ast.PropertyKind) []const u8 {
    return switch (kind) {
        .Init => "init",
        .Get => "get",
        .Set => "set",
    };
}

/// Serialize a ParseTree to ESTree-compatible JSON.
/// Caller owns the returned memory and must free it.
pub fn toJSON(tree: *const parser.ParseTree, allocator: std.mem.Allocator) ![]u8 {
    return Serializer.serialize(tree, allocator, true);
}

/// Serialize a ParseTree to minified ESTree-compatible JSON.
/// Caller owns the returned memory and must free it.
pub fn toJSONMinified(tree: *const parser.ParseTree, allocator: std.mem.Allocator) ![]u8 {
    return Serializer.serialize(tree, allocator, false);
}
