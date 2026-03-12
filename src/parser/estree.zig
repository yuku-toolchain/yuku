// converts the parsed tree to estree/typescript-estree compatible JSON string.

const std = @import("std");
const ast = @import("ast.zig");
const util = @import("util");

pub const EstreeJsonOptions = struct {
    pretty: bool = true,
    indent_size: u32 = 2,
};

pub const Serializer = struct {
    tree: *const ast.ParseTree,
    buffer: *std.ArrayList(u8),
    allocator: std.mem.Allocator,
    depth: u32 = 0,
    options: EstreeJsonOptions,
    needs_comma_bits: [8]u64 = .{0} ** 8,
    scratch: std.ArrayList(u8) = .empty,
    isTs: bool,
    /// UTF-16 position map. Built from source for parsed trees.
    /// null for manually-built trees (positions pass through unchanged).
    pos_map: ?[]u32,
    in_jsx_attribute: bool = false,

    const Self = @This();
    const Error = error{ InvalidCharacter, NoSpaceLeft, OutOfMemory, Overflow };
    const NodeTag = std.meta.Tag(ast.NodeData);

    pub fn serialize(tree: *const ast.ParseTree, allocator: std.mem.Allocator, options: EstreeJsonOptions) ![]u8 {
        var buffer: std.ArrayList(u8) = try .initCapacity(allocator, tree.nodes.len * 64 + 4096);
        errdefer buffer.deinit(allocator);

        const pos_map: ?[]u32 = if (tree.source.len > 0)
            try buildUtf16PosMap(allocator, tree.source)
        else
            null;
        defer if (pos_map) |m| allocator.free(m);

        var self = Self{
            .tree = tree,
            .buffer = &buffer,
            .allocator = allocator,
            .options = options,
            .scratch = try .initCapacity(allocator, 512),
            .isTs = switch (tree.lang) {
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
        if (index == .null) return self.writeNull();

        const data = self.tree.getData(index);
        const span = self.tree.getSpan(index);

        try switch (data) {
            .program => |d| self.writeProgram(d, span),
            .directive => |d| self.writeDirective(d, span),
            .function => |d| self.writeFunction(d, span),
            .formal_parameters => |d| self.writeFormalParameters(d),
            .formal_parameter => |d| self.writeNode(d.pattern),
            .arrow_function_expression => |d| self.writeArrowFunction(d, span),
            .class => |d| self.writeClass(d, span),
            .property_definition => |d| self.writePropertyDefinition(d, span),
            .binding_property => |d| self.writeBindingProperty(d, span),
            .array_pattern => |d| self.writeArrayPattern(d, span),
            .object_pattern => |d| self.writeObjectPattern(d, span),
            .import_expression => |d| self.writeImportExpression(d, span),
            .import_declaration => |d| self.writeImportDeclaration(d, span),
            .jsx_attribute => |d| self.writeJSXAttribute(d, span),
            .jsx_opening_fragment => self.writeJSXOpeningFragment(span),
            .jsx_identifier => |d| self.writeJSXIdentifier(d, span),
            .jsx_text => |d| self.writeJSXText(d, span),

            .binary_expression => |d| self.writeBinaryExpr("BinaryExpression", d.left, d.operator.toString(), d.right, span),
            .logical_expression => |d| self.writeBinaryExpr("LogicalExpression", d.left, d.operator.toString(), d.right, span),
            .assignment_expression => |d| self.writeAssignmentExpression(d, span),
            .unary_expression => |d| self.writeUnaryExpression(d, span),
            .update_expression => |d| self.writeUpdateExpression(d, span),
            .yield_expression => |d| self.writeYieldExpression(d, span),
            .object_property => |d| self.writeObjectProperty(d, span),

            .identifier_reference => |d| self.writeIdentifier(d, span),
            .binding_identifier => |d| self.writeIdentifier(d, span),
            .identifier_name => |d| self.writeIdentifier(d, span),
            .label_identifier => |d| self.writeIdentifier(d, span),
            .private_identifier => |d| self.writePrivateIdentifier(d, span),

            .string_literal => |d| self.writeStringLiteral(d, span),
            .numeric_literal => |d| self.writeNumericLiteral(d, span),
            .bigint_literal => |d| self.writeBigIntLiteral(d, span),
            .boolean_literal => |d| self.writeBooleanLiteral(d, span),
            .null_literal => self.writeNullLiteral(span),
            .regexp_literal => |d| self.writeRegExpLiteral(d, span),
            .template_element => |d| self.writeTemplateElement(d, span),

            inline else => |payload, tag| self.writeGenericNode(tag, payload, span),
        };
    }

    fn writeGenericNode(self: *Self, comptime tag: NodeTag, payload: anytype, span: ast.Span) Error!void {
        try self.begin(comptime estreeTypeName(tag), span);
        const T = @TypeOf(payload);
        if (@typeInfo(T) == .@"struct") {
            inline for (@typeInfo(T).@"struct".fields) |f| {
                const json_name = comptime fieldJsonName(tag, f.name);
                if (f.type == ast.NodeIndex) {
                    try self.fieldNode(json_name, @field(payload, f.name));
                } else if (f.type == ast.IndexRange) {
                    try self.fieldNodeArray(json_name, @field(payload, f.name));
                } else if (f.type == bool) {
                    try self.fieldBool(json_name, @field(payload, f.name));
                } else if (comptime @typeInfo(f.type) == .@"enum" and @hasDecl(f.type, "toString")) {
                    try self.fieldString(json_name, @field(payload, f.name).toString());
                }
            }
        }
        try self.endObject();
    }

    fn writeProgram(self: *Self, data: ast.Program, span: ast.Span) !void {
        try self.begin("Program", span);
        try self.fieldString("sourceType", if (data.source_type == .module) "module" else "script");
        try self.field("hashbang");
        if (data.hashbang) |h| {
            try self.writeString(self.tree.getString(h.value));
        } else {
            try self.writeNull();
        }
        try self.field("body");
        try self.beginArray();
        for (self.getExtra(data.body)) |idx| try self.elemNode(idx);
        try self.endArray();
        try self.endObject();
    }

    fn writeDirective(self: *Self, data: ast.Directive, span: ast.Span) !void {
        try self.begin("ExpressionStatement", span);
        try self.fieldNode("expression", data.expression);
        try self.fieldString("directive", self.tree.getString(data.value));
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

    fn writeArrowFunction(self: *Self, data: ast.ArrowFunctionExpression, span: ast.Span) !void {
        try self.begin("ArrowFunctionExpression", span);
        try self.fieldNode("id", .null);
        try self.fieldBool("generator", false);
        try self.fieldBool("async", data.async);
        try self.field("params");
        try self.writeFunctionParams(data.params);
        try self.fieldNode("body", data.body);
        try self.fieldBool("expression", data.expression);
        try self.endObject();
    }

    fn writeClass(self: *Self, data: ast.Class, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType(switch (data.type) {
            .class_declaration => "ClassDeclaration",
            .class_expression => "ClassExpression",
        });
        try self.fieldSpan(span);
        try self.fieldNodeArray("decorators", data.decorators);
        try self.fieldNode("id", data.id);
        try self.fieldNode("superClass", data.super_class);
        try self.fieldNode("body", data.body);
        try self.endObject();
    }

    fn writePropertyDefinition(self: *Self, data: ast.PropertyDefinition, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType(if (data.accessor) "AccessorProperty" else "PropertyDefinition");
        try self.fieldSpan(span);
        try self.fieldNodeArray("decorators", data.decorators);
        try self.fieldNode("key", data.key);
        try self.fieldNode("value", data.value);
        try self.fieldBool("computed", data.computed);
        try self.fieldBool("static", data.static);
        try self.endObject();
    }

    fn writeObjectProperty(self: *Self, data: ast.ObjectProperty, span: ast.Span) !void {
        try self.begin("Property", span);
        try self.fieldString("kind", data.kind.toString());
        try self.fieldNode("key", data.key);
        try self.fieldNode("value", data.value);
        try self.fieldBool("method", data.method);
        try self.fieldBool("shorthand", data.shorthand);
        try self.fieldBool("computed", data.computed);
        try self.endObject();
    }

    fn writeBindingProperty(self: *Self, data: ast.BindingProperty, span: ast.Span) !void {
        try self.begin("Property", span);
        try self.fieldString("kind", "init");
        try self.fieldNode("key", data.key);
        try self.fieldNode("value", data.value);
        try self.fieldBool("method", false);
        try self.fieldBool("shorthand", data.shorthand);
        try self.fieldBool("computed", data.computed);
        try self.endObject();
    }

    fn writeBinaryExpr(self: *Self, comptime type_name: []const u8, left: ast.NodeIndex, op: []const u8, right: ast.NodeIndex, span: ast.Span) !void {
        try self.begin(type_name, span);
        try self.fieldNode("left", left);
        try self.fieldString("operator", op);
        try self.fieldNode("right", right);
        try self.endObject();
    }

    fn writeAssignmentExpression(self: *Self, data: ast.AssignmentExpression, span: ast.Span) !void {
        try self.begin("AssignmentExpression", span);
        try self.fieldString("operator", data.operator.toString());
        try self.fieldNode("left", data.left);
        try self.fieldNode("right", data.right);
        try self.endObject();
    }

    fn writeUnaryExpression(self: *Self, data: ast.UnaryExpression, span: ast.Span) !void {
        try self.begin("UnaryExpression", span);
        try self.fieldString("operator", data.operator.toString());
        try self.fieldBool("prefix", true);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeUpdateExpression(self: *Self, data: ast.UpdateExpression, span: ast.Span) !void {
        try self.begin("UpdateExpression", span);
        try self.fieldString("operator", data.operator.toString());
        try self.fieldBool("prefix", data.prefix);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeYieldExpression(self: *Self, data: ast.YieldExpression, span: ast.Span) !void {
        try self.begin("YieldExpression", span);
        try self.fieldBool("delegate", data.delegate);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeArrayPattern(self: *Self, data: ast.ArrayPattern, span: ast.Span) !void {
        try self.begin("ArrayPattern", span);
        try self.field("elements");
        try self.beginArray();
        for (self.getExtra(data.elements)) |idx| try self.elemNode(idx);
        if (data.rest != .null) try self.elemNode(data.rest);
        try self.endArray();
        try self.endObject();
    }

    fn writeObjectPattern(self: *Self, data: ast.ObjectPattern, span: ast.Span) !void {
        try self.begin("ObjectPattern", span);
        try self.field("properties");
        try self.beginArray();
        for (self.getExtra(data.properties)) |idx| try self.elemNode(idx);
        if (data.rest != .null) try self.elemNode(data.rest);
        try self.endArray();
        try self.endObject();
    }

    fn writeImportExpression(self: *Self, data: ast.ImportExpression, span: ast.Span) !void {
        try self.begin("ImportExpression", span);
        try self.fieldNode("source", data.source);
        try self.fieldNode("options", data.options);
        try self.writeImportPhase(data.phase);
        try self.endObject();
    }

    fn writeImportDeclaration(self: *Self, data: ast.ImportDeclaration, span: ast.Span) !void {
        try self.begin("ImportDeclaration", span);
        try self.fieldNodeArray("specifiers", data.specifiers);
        try self.fieldNode("source", data.source);
        try self.writeImportPhase(data.phase);
        try self.fieldNodeArray("attributes", data.attributes);
        try self.endObject();
    }

    fn writeJSXAttribute(self: *Self, data: ast.JSXAttribute, span: ast.Span) !void {
        try self.begin("JSXAttribute", span);
        try self.fieldNode("name", data.name);
        self.in_jsx_attribute = true;
        try self.fieldNode("value", data.value);
        self.in_jsx_attribute = false;
        try self.endObject();
    }

    fn writeJSXOpeningFragment(self: *Self, span: ast.Span) !void {
        try self.begin("JSXOpeningFragment", span);
        try self.fieldEmptyArray("attributes");
        try self.fieldBool("selfClosing", false);
        try self.endObject();
    }

    fn writeJSXIdentifier(self: *Self, data: ast.JSXIdentifier, span: ast.Span) !void {
        try self.begin("JSXIdentifier", span);
        try self.fieldString("name", self.tree.getString(data.name));
        try self.endObject();
    }

    fn writeJSXText(self: *Self, data: ast.JSXText, span: ast.Span) !void {
        const raw = self.tree.getString(data.raw);
        try self.begin("JSXText", span);
        try self.fieldString("value", raw);
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeIdentifier(self: *Self, data: anytype, span: ast.Span) !void {
        try self.begin("Identifier", span);
        try self.field("name");
        try self.writeDecodedString(self.tree.getString(data.name));
        try self.endObject();
    }

    fn writePrivateIdentifier(self: *Self, data: ast.PrivateIdentifier, span: ast.Span) !void {
        try self.begin("PrivateIdentifier", span);
        try self.field("name");
        try self.writeDecodedString(self.tree.getString(data.name));
        try self.endObject();
    }

    fn writeStringLiteral(self: *Self, data: ast.StringLiteral, span: ast.Span) !void {
        const raw = self.tree.getString(data.raw);
        try self.begin("Literal", span);
        try self.field("value");
        if (self.in_jsx_attribute)
            try self.writeString(raw[1 .. raw.len - 1])
        else
            try self.writeDecodedString(raw[1 .. raw.len - 1]);
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeNumericLiteral(self: *Self, data: ast.NumericLiteral, span: ast.Span) !void {
        const raw = self.tree.getString(data.raw);
        var buf: [64]u8 = undefined;
        const num_str: ?[]const u8 = parseJSNumeric(&buf, raw) catch null;

        try self.begin("Literal", span);
        if (num_str) |s| {
            if (std.mem.eql(u8, s, "inf") or std.mem.eql(u8, s, "-inf")) {
                try self.fieldNull("value");
            } else {
                try self.field("value");
                try self.write(s);
            }
        } else {
            try self.fieldNull("value");
        }
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeBigIntLiteral(self: *Self, data: ast.BigIntLiteral, span: ast.Span) !void {
        const raw = self.tree.getString(data.raw);
        self.scratch.clearRetainingCapacity();
        try self.scratch.appendSlice(self.allocator, "(BigInt) ");
        try self.scratch.appendSlice(self.allocator, raw);
        try self.begin("Literal", span);
        try self.fieldString("value", self.scratch.items);
        try self.fieldString("raw", raw);
        try self.field("bigint");
        self.scratch.clearRetainingCapacity();
        for (raw[0 .. raw.len - 1]) |c| {
            if (c != '_') try self.scratch.append(self.allocator, c);
        }
        try self.writeString(self.scratch.items);
        try self.endObject();
    }

    fn writeBooleanLiteral(self: *Self, data: ast.BooleanLiteral, span: ast.Span) !void {
        try self.begin("Literal", span);
        try self.fieldBool("value", data.value);
        try self.fieldString("raw", if (data.value) "true" else "false");
        try self.endObject();
    }

    fn writeNullLiteral(self: *Self, span: ast.Span) !void {
        try self.begin("Literal", span);
        try self.fieldNull("value");
        try self.fieldString("raw", "null");
        try self.endObject();
    }

    fn writeRegExpLiteral(self: *Self, data: ast.RegExpLiteral, span: ast.Span) !void {
        const pattern = self.tree.getString(data.pattern);
        const flags = self.tree.getString(data.flags);

        self.scratch.clearRetainingCapacity();
        try self.scratch.appendSlice(self.allocator, flags);
        std.mem.sort(u8, self.scratch.items, {}, comptime std.sort.asc(u8));

        try self.begin("Literal", span);
        try self.field("value");
        try self.writeTaggedRegExpValue(pattern, flags);
        try self.field("raw");
        try self.writeRegExpRaw(pattern, flags);
        try self.field("regex");
        try self.beginObject();
        try self.fieldString("pattern", pattern);
        try self.fieldString("flags", self.scratch.items);
        try self.endObject();
        try self.endObject();
    }

    fn writeTemplateElement(self: *Self, data: ast.TemplateElement, span: ast.Span) !void {
        const raw = self.tree.getString(data.raw);

        // normalize line endings: CRLF (\r\n) and standalone CR (\r) -> LF (\n) per spec
        self.scratch.clearRetainingCapacity();
        var i: usize = 0;
        while (i < raw.len) {
            const c = raw[i];
            if (c == '\r') {
                try self.scratch.append(self.allocator, '\n');
                i += if (i + 1 < raw.len and raw[i + 1] == '\n') @as(usize, 2) else 1;
            } else {
                try self.scratch.append(self.allocator, c);
                i += 1;
            }
        }

        try self.begin("TemplateElement", span);
        try self.field("value");
        try self.beginObject();
        try self.fieldString("raw", self.scratch.items);
        try self.field("cooked");
        if (data.is_cooked_undefined)
            try self.writeNull()
        else
            try self.writeDecodedString(self.scratch.items);
        try self.endObject();
        try self.fieldBool("tail", data.tail);
        try self.endObject();
    }

    fn writeFormalParameters(self: *Self, data: ast.FormalParameters) !void {
        try self.beginArray();
        for (self.getExtra(data.items)) |idx| {
            try self.elemNode(self.tree.getData(idx).formal_parameter.pattern);
        }
        if (data.rest != .null) try self.elemNode(data.rest);
        try self.endArray();
    }

    fn writeFunctionParams(self: *Self, params_index: ast.NodeIndex) !void {
        try self.beginArray();
        if (params_index != .null) {
            const params = self.tree.getData(params_index).formal_parameters;
            for (self.getExtra(params.items)) |idx| {
                try self.elemNode(self.tree.getData(idx).formal_parameter.pattern);
            }
            if (params.rest != .null) try self.elemNode(params.rest);
        }
        try self.endArray();
    }

    fn writeImportPhase(self: *Self, phase: ?ast.ImportPhase) !void {
        try self.field("phase");
        if (phase) |p| {
            try self.writeString(switch (p) {
                .source => "source",
                .@"defer" => "defer",
            });
        } else {
            try self.writeNull();
        }
    }

    fn writeRegExpRaw(self: *Self, pattern: []const u8, flags: []const u8) !void {
        try self.writeByte('"');
        try self.writeByte('/');
        try self.writeJsonEscaped(pattern);
        try self.writeByte('/');
        try self.write(flags);
        try self.writeByte('"');
    }

    fn writeTaggedRegExpValue(self: *Self, pattern: []const u8, flags: []const u8) !void {
        try self.writeByte('"');
        try self.write("(RegExp) ");
        try self.writeByte('/');
        try self.writeJsonEscaped(pattern);
        try self.writeByte('/');
        try self.write(flags);
        try self.writeByte('"');
    }

    fn writeComments(self: *Self) !void {
        try self.beginArray();
        for (self.tree.comments) |comment| {
            try self.sep();
            if (self.options.pretty) {
                try self.writeByte('\n');
                try self.writeIndent();
            }
            try self.beginObject();
            try self.fieldString("type", comment.type.toString());
            try self.fieldString("value", self.tree.getString(comment.value));
            try self.fieldPos("start", comment.start);
            try self.fieldPos("end", comment.end);
            try self.endObject();
        }
        try self.endArray();
    }

    fn writeDiagnostics(self: *Self) !void {
        try self.beginArray();
        for (self.tree.diagnostics) |diag| {
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

    fn begin(self: *Self, comptime type_name: []const u8, span: ast.Span) Error!void {
        try self.writeByte('{');
        self.depth += 1;
        self.setNeedsComma(false);
        if (self.options.pretty) {
            try self.writeByte('\n');
            try self.writeIndent();
            try self.write("\"type\": \"" ++ type_name ++ "\"");
        } else {
            try self.write("\"type\":\"" ++ type_name ++ "\"");
        }
        self.setNeedsComma(true);
        try self.fieldPos("start", span.start);
        try self.fieldPos("end", span.end);
    }

    inline fn write(self: *Self, bytes: []const u8) !void {
        try self.buffer.appendSlice(self.allocator, bytes);
    }

    inline fn writeByte(self: *Self, byte: u8) !void {
        try self.buffer.append(self.allocator, byte);
    }

    inline fn needsComma(self: *Self) bool {
        const idx = self.depth / 64;
        const bit = @as(u6, @intCast(self.depth % 64));
        return (self.needs_comma_bits[idx] & (@as(u64, 1) << bit)) != 0;
    }

    inline fn setNeedsComma(self: *Self, value: bool) void {
        const idx = self.depth / 64;
        const bit = @as(u6, @intCast(self.depth % 64));
        if (value) {
            self.needs_comma_bits[idx] |= (@as(u64, 1) << bit);
        } else {
            self.needs_comma_bits[idx] &= ~(@as(u64, 1) << bit);
        }
    }

    inline fn sep(self: *Self) !void {
        if (self.needsComma()) try self.writeByte(',');
        self.setNeedsComma(true);
    }

    fn beginObject(self: *Self) !void {
        try self.writeByte('{');
        self.depth += 1;
        self.setNeedsComma(false);
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
        self.setNeedsComma(false);
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
        if (self.pos_map) |map| {
            try self.writeInt(map[@min(byte_pos, map.len - 1)]);
        } else {
            try self.writeInt(byte_pos);
        }
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

    fn fieldEmptyArray(self: *Self, key: []const u8) !void {
        try self.field(key);
        try self.write("[]");
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
        var start: usize = 0;
        for (s, 0..) |c, i| {
            if (c == '"' or c == '\\' or c < 0x20) {
                if (i > start) try self.write(s[start..i]);
                try self.writeJsonEscapedChar(c);
                start = i + 1;
            }
        }
        if (start < s.len) try self.write(s[start..]);
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

    /// writes a JS string value as JSON, handling escape sequences.
    /// JSON-compatible escapes (\n, \r, \t, \b, \f, \\, \", \/, \uXXXX) pass through directly
    /// since JSON.parse will interpret them. JS-only escapes (\v, \xXX, \0, octals, \u{...})
    /// are decoded here since JSON.parse doesn't understand them.
    fn writeDecodedString(self: *Self, input: []const u8) !void {
        try self.writeByte('"');

        var i: usize = 0;
        while (i < input.len) {
            const c = input[i];

            if (c != '\\') {
                try self.writeJsonEscapedChar(c);
                i += 1;
                continue;
            }
            if (i + 1 >= input.len) {
                try self.write("\\\\");
                break;
            }

            const next = input[i + 1];

            if (util.Utf.unicodeSeparatorLen(input, i + 1) > 0) {
                i += 4;
                continue;
            }

            switch (next) {
                'n', 'r', 't', 'b', 'f', '"', '\\' => {
                    try self.writeByte('\\');
                    try self.writeByte(next);
                    i += 2;
                },
                '/' => {
                    try self.writeByte('/');
                    i += 2;
                },
                'u' => {
                    if (i + 2 < input.len and input[i + 2] == '{') {
                        if (util.Utf.parseUnicodeEscape(input, i + 2)) |r| {
                            try self.writeCodePointAsJson(r.value);
                            i = r.end;
                            continue;
                        }
                        try self.writeByte('u');
                        i += 2;
                    } else if (i + 5 < input.len and util.Utf.parseHex4(input, i + 2) != null) {
                        try self.write("\\u");
                        try self.write(input[i + 2 .. i + 6]);
                        i += 6;
                    } else {
                        try self.writeByte('u');
                        i += 2;
                    }
                },
                'v' => {
                    try self.write("\\u000b");
                    i += 2;
                },
                '0' => {
                    if (i + 2 < input.len and util.Utf.isOctalDigit(input[i + 2])) {
                        const r = util.Utf.parseOctal(input, i + 1);
                        try self.writeCodePointAsJson(r.value);
                        i = r.end;
                    } else {
                        try self.write("\\u0000");
                        i += 2;
                    }
                },
                '1'...'7' => {
                    const r = util.Utf.parseOctal(input, i + 1);
                    try self.writeCodePointAsJson(r.value);
                    i = r.end;
                },
                'x' => {
                    if (util.Utf.parseHex2(input, i + 2)) |r| {
                        try self.writeCodePointAsJson(r.value);
                        i = r.end;
                    } else {
                        try self.writeByte('x');
                        i += 2;
                    }
                },
                '\r' => {
                    i += if (i + 2 < input.len and input[i + 2] == '\n') @as(usize, 3) else 2;
                },
                '\n' => i += 2,
                else => {
                    try self.writeJsonEscapedChar(next);
                    i += 2;
                },
            }
        }

        try self.writeByte('"');
    }

    fn writeJsonEscapedChar(self: *Self, c: u8) !void {
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
                    try self.write(std.fmt.bufPrint(&buf, "\\u{x:0>4}", .{c}) catch unreachable);
                } else {
                    try self.writeByte(c);
                }
            },
        }
    }

    /// writes a Unicode code point as JSON. ASCII chars are escaped if needed,
    /// non-ASCII are UTF-8 encoded.
    fn writeCodePointAsJson(self: *Self, cp: u21) !void {
        if (cp < 0x80) {
            try self.writeJsonEscapedChar(@intCast(cp));
        } else {
            var buf: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(cp, &buf) catch {
                try self.write("\u{FFFD}");
                return;
            };
            try self.write(buf[0..len]);
        }
    }

    fn estreeTypeName(comptime tag: NodeTag) []const u8 {
        return switch (tag) {
            .function_body => "BlockStatement",
            .binding_rest_element => "RestElement",
            .jsx_element => "JSXElement",
            .jsx_opening_element => "JSXOpeningElement",
            .jsx_closing_element => "JSXClosingElement",
            .jsx_fragment => "JSXFragment",
            .jsx_closing_fragment => "JSXClosingFragment",
            .jsx_namespaced_name => "JSXNamespacedName",
            .jsx_member_expression => "JSXMemberExpression",
            .jsx_spread_attribute => "JSXSpreadAttribute",
            .jsx_expression_container => "JSXExpressionContainer",
            .jsx_empty_expression => "JSXEmptyExpression",
            .jsx_spread_child => "JSXSpreadChild",
            .ts_export_assignment => "TSExportAssignment",
            .ts_namespace_export_declaration => "TSNamespaceExportDeclaration",
            else => snakeToCase(@tagName(tag), true),
        };
    }

    fn fieldJsonName(comptime tag: NodeTag, comptime name: []const u8) []const u8 {
        return comptime if (tag == .variable_declaration and std.mem.eql(u8, name, "declarators"))
            "declarations"
        else
            snakeToCase(name, false);
    }

    fn snakeToCase(comptime name: []const u8, comptime pascal: bool) []const u8 {
        comptime {
            var buf: [name.len]u8 = undefined;
            var len: usize = 0;
            var cap = pascal;
            for (name) |c| {
                if (c == '_') {
                    cap = true;
                } else {
                    buf[len] = if (cap) (c ^ 0x20) else c;
                    cap = false;
                    len += 1;
                }
            }
            const final = buf[0..len].*;
            return &final;
        }
    }
};

/// Builds a lookup table mapping byte positions to UTF-16 code unit offsets.
/// ESTree positions use UTF-16 code units (matching JavaScript's string indexing).
fn buildUtf16PosMap(allocator: std.mem.Allocator, source: []const u8) ![]u32 {
    var map = try allocator.alloc(u32, source.len + 1);
    var byte_pos: usize = 0;
    var utf16_pos: u32 = 0;

    while (byte_pos < source.len) {
        map[byte_pos] = utf16_pos;
        const len = std.unicode.utf8ByteSequenceLength(source[byte_pos]) catch 1;
        utf16_pos += if (len == 4) 2 else 1;
        for (1..len) |i| {
            if (byte_pos + i < source.len) map[byte_pos + i] = utf16_pos;
        }
        byte_pos += len;
    }
    map[source.len] = utf16_pos;
    return map;
}

fn parseJSNumeric(outbuf: []u8, str: []const u8) ![]const u8 {
    var buf: [128]u8 = undefined;
    var len: usize = 0;
    for (str) |c| {
        if (c != '_') {
            if (len >= buf.len) return error.Overflow;
            buf[len] = c;
            len += 1;
        }
    }
    const s = buf[0..len];
    if (s.len == 0) return error.InvalidCharacter;

    var val: f64 = undefined;
    if (s.len >= 2 and s[0] == '0') {
        switch (s[1]) {
            'x', 'X' => val = @floatFromInt(try std.fmt.parseInt(i64, s[2..], 16)),
            'b', 'B' => val = @floatFromInt(try std.fmt.parseInt(i64, s[2..], 2)),
            'o', 'O' => val = @floatFromInt(try std.fmt.parseInt(i64, s[2..], 8)),
            '0'...'7' => {
                var is_octal = true;
                for (s[1..]) |c| {
                    if (c < '0' or c > '7') {
                        is_octal = false;
                        break;
                    }
                }
                if (is_octal) {
                    val = @floatFromInt(try std.fmt.parseInt(i64, s[1..], 8));
                } else {
                    val = try std.fmt.parseFloat(f64, s);
                }
            },
            else => val = try std.fmt.parseFloat(f64, s),
        }
    } else {
        val = std.fmt.parseFloat(f64, s) catch @floatFromInt(try std.fmt.parseInt(i64, s, 10));
    }

    return std.fmt.bufPrint(outbuf, "{e}", .{val});
}

pub fn toJSON(tree: *const ast.ParseTree, allocator: std.mem.Allocator, options: EstreeJsonOptions) ![]u8 {
    return Serializer.serialize(tree, allocator, options);
}
