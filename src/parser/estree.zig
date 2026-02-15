// this is slow tbh (while other yuku things are super/extremely fast in comparison), but it's as fast as possible.
// it converts the parsed tree to a estree/typescript-estree compatible json string.
// used for testing and ast inspection purposes, such as snapshot tests (in the test folder).
// this module will likely be removed when we have a better approach for passing ast to js,
// and we'll use that approach for snapshot tests.

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
    pos_map: []u32,
    in_jsx_attribute: bool = false,

    const Self = @This();
    const Error = error{ InvalidCharacter, NoSpaceLeft, OutOfMemory, Overflow };

    pub fn serialize(tree: *const ast.ParseTree, allocator: std.mem.Allocator, options: EstreeJsonOptions) ![]u8 {
        var buffer: std.ArrayList(u8) = try .initCapacity(allocator, tree.source.len * 4);
        errdefer buffer.deinit(allocator);

        const pos_map = try buildUtf16PosMap(allocator, tree.source);
        defer allocator.free(pos_map);

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
            .block_statement => |d| self.writeBlockStatement(d, span),
            .formal_parameters => |d| self.writeFormalParameters(d, span),
            .formal_parameter => |d| self.writeFormalParameter(d),
            .expression_statement => |d| self.writeExpressionStatement(d, span),
            .if_statement => |d| self.writeIfStatement(d, span),
            .switch_statement => |d| self.writeSwitchStatement(d, span),
            .switch_case => |d| self.writeSwitchCase(d, span),
            .for_statement => |d| self.writeForStatement(d, span),
            .for_in_statement => |d| self.writeForInStatement(d, span),
            .for_of_statement => |d| self.writeForOfStatement(d, span),
            .while_statement => |d| self.writeWhileStatement(d, span),
            .do_while_statement => |d| self.writeDoWhileStatement(d, span),
            .break_statement => |d| self.writeBreakStatement(d, span),
            .continue_statement => |d| self.writeContinueStatement(d, span),
            .labeled_statement => |d| self.writeLabeledStatement(d, span),
            .with_statement => |d| self.writeWithStatement(d, span),
            .return_statement => |d| self.writeReturnStatement(d, span),
            .throw_statement => |d| self.writeThrowStatement(d, span),
            .try_statement => |d| self.writeTryStatement(d, span),
            .catch_clause => |d| self.writeCatchClause(d, span),
            .debugger_statement => self.writeDebuggerStatement(span),
            .empty_statement => self.writeEmptyStatement(span),
            .variable_declaration => |d| self.writeVariableDeclaration(d, span),
            .variable_declarator => |d| self.writeVariableDeclarator(d, span),
            .binary_expression => |d| self.writeBinaryExpression(d, span),
            .logical_expression => |d| self.writeLogicalExpression(d, span),
            .conditional_expression => |d| self.writeConditionalExpression(d, span),
            .unary_expression => |d| self.writeUnaryExpression(d, span),
            .update_expression => |d| self.writeUpdateExpression(d, span),
            .assignment_expression => |d| self.writeAssignmentExpression(d, span),
            .array_expression => |d| self.writeArrayExpression(d, span),
            .object_expression => |d| self.writeObjectExpression(d, span),
            .spread_element => |d| self.writeSpreadElement(d, span),
            .object_property => |d| self.writeObjectProperty(d, span),
            .member_expression => |d| self.writeMemberExpression(d, span),
            .call_expression => |d| self.writeCallExpression(d, span),
            .chain_expression => |d| self.writeChainExpression(d, span),
            .tagged_template_expression => |d| self.writeTaggedTemplateExpression(d, span),
            .new_expression => |d| self.writeNewExpression(d, span),
            .await_expression => |d| self.writeAwaitExpression(d, span),
            .yield_expression => |d| self.writeYieldExpression(d, span),
            .meta_property => |d| self.writeMetaProperty(d, span),
            .decorator => |d| self.writeDecorator(d, span),
            .this_expression => self.writeThisExpression(span),
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
            .label_identifier => |d| self.writeIdentifier(d, span),
            .private_identifier => |d| self.writePrivateIdentifier(d, span),
            .assignment_pattern => |d| self.writeAssignmentPattern(d, span),
            .array_pattern => |d| self.writeArrayPattern(d, span),
            .object_pattern => |d| self.writeObjectPattern(d, span),
            .binding_property => |d| self.writeBindingProperty(d, span),
            .binding_rest_element => |d| self.writeRestElement(d, span),
            .parenthesized_expression => |d| self.writeParenthesizedExpression(d, span),
            .arrow_function_expression => |d| self.writeArrowFunctionExpression(d, span),
            .sequence_expression => |d| self.writeSequenceExpression(d, span),
            .class => |d| self.writeClass(d, span),
            .class_body => |d| self.writeClassBody(d, span),
            .method_definition => |d| self.writeMethodDefinition(d, span),
            .property_definition => |d| self.writePropertyDefinition(d, span),
            .static_block => |d| self.writeStaticBlock(d, span),
            .super => self.writeSuper(span),
            .import_expression => |d| self.writeImportExpression(d, span),
            .import_declaration => |d| self.writeImportDeclaration(d, span),
            .import_specifier => |d| self.writeImportSpecifier(d, span),
            .import_default_specifier => |d| self.writeImportDefaultSpecifier(d, span),
            .import_namespace_specifier => |d| self.writeImportNamespaceSpecifier(d, span),
            .import_attribute => |d| self.writeImportAttribute(d, span),
            .export_named_declaration => |d| self.writeExportNamedDeclaration(d, span),
            .export_default_declaration => |d| self.writeExportDefaultDeclaration(d, span),
            .export_all_declaration => |d| self.writeExportAllDeclaration(d, span),
            .export_specifier => |d| self.writeExportSpecifier(d, span),
            .ts_export_assignment => |d| self.writeTSExportAssignment(d, span),
            .ts_namespace_export_declaration => |d| self.writeTSNamespaceExportDeclaration(d, span),
            .jsx_element => |d| self.writeJSXElement(d, span),
            .jsx_opening_element => |d| self.writeJSXOpeningElement(d, span),
            .jsx_closing_element => |d| self.writeJSXClosingElement(d, span),
            .jsx_fragment => |d| self.writeJSXFragment(d, span),
            .jsx_opening_fragment => self.writeJSXOpeningFragment(span),
            .jsx_closing_fragment => self.writeJSXClosingFragment(span),
            .jsx_identifier => |d| self.writeJSXIdentifier(d, span),
            .jsx_namespaced_name => |d| self.writeJSXNamespacedName(d, span),
            .jsx_member_expression => |d| self.writeJSXMemberExpression(d, span),
            .jsx_attribute => |d| self.writeJSXAttribute(d, span),
            .jsx_spread_attribute => |d| self.writeJSXSpreadAttribute(d, span),
            .jsx_expression_container => |d| self.writeJSXExpressionContainer(d, span),
            .jsx_empty_expression => self.writeJSXEmptyExpression(span),
            .jsx_text => |d| self.writeJSXText(d, span),
            .jsx_spread_child => |d| self.writeJSXSpreadChild(d, span),
        };
    }

    fn writeProgram(self: *Self, data: ast.Program, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Program");
        try self.fieldSpan(span);
        try self.fieldString("sourceType", if (data.source_type == .module) "module" else "script");
        try self.field("body");
        try self.beginArray();
        for (self.getExtra(data.body)) |idx| try self.elemNode(idx);
        try self.endArray();
        try self.endObject();
    }

    fn writeDirective(self: *Self, data: ast.Directive, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ExpressionStatement");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.fieldString("directive", self.tree.getSourceText(data.value_start, data.value_len));
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
        for (self.getExtra(data.body)) |idx| try self.elemNode(idx);
        try self.endArray();
        try self.endObject();
    }

    fn writeBlockStatement(self: *Self, data: ast.BlockStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("BlockStatement");
        try self.fieldSpan(span);
        try self.field("body");
        try self.beginArray();
        for (self.getExtra(data.body)) |idx| {
            try self.elemNode(idx);
        }
        try self.endArray();
        try self.endObject();
    }

    fn writeFormalParameters(self: *Self, data: ast.FormalParameters, _: ast.Span) !void {
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

    fn writeIfStatement(self: *Self, data: ast.IfStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("IfStatement");
        try self.fieldSpan(span);
        try self.fieldNode("test", data.@"test");
        try self.fieldNode("consequent", data.consequent);
        try self.fieldNode("alternate", data.alternate);
        try self.endObject();
    }

    fn writeSwitchStatement(self: *Self, data: ast.SwitchStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("SwitchStatement");
        try self.fieldSpan(span);
        try self.fieldNode("discriminant", data.discriminant);
        try self.fieldNodeArray("cases", data.cases);
        try self.endObject();
    }

    fn writeSwitchCase(self: *Self, data: ast.SwitchCase, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("SwitchCase");
        try self.fieldSpan(span);
        try self.fieldNode("test", data.@"test");
        try self.fieldNodeArray("consequent", data.consequent);
        try self.endObject();
    }

    fn writeForStatement(self: *Self, data: ast.ForStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ForStatement");
        try self.fieldSpan(span);
        try self.fieldNode("init", data.init);
        try self.fieldNode("test", data.@"test");
        try self.fieldNode("update", data.update);
        try self.fieldNode("body", data.body);
        try self.endObject();
    }

    fn writeForInStatement(self: *Self, data: ast.ForInStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ForInStatement");
        try self.fieldSpan(span);
        try self.fieldNode("left", data.left);
        try self.fieldNode("right", data.right);
        try self.fieldNode("body", data.body);
        try self.endObject();
    }

    fn writeForOfStatement(self: *Self, data: ast.ForOfStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ForOfStatement");
        try self.fieldSpan(span);
        try self.fieldNode("left", data.left);
        try self.fieldNode("right", data.right);
        try self.fieldNode("body", data.body);
        try self.fieldBool("await", data.await);
        try self.endObject();
    }

    fn writeWhileStatement(self: *Self, data: ast.WhileStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("WhileStatement");
        try self.fieldSpan(span);
        try self.fieldNode("test", data.@"test");
        try self.fieldNode("body", data.body);
        try self.endObject();
    }

    fn writeDoWhileStatement(self: *Self, data: ast.DoWhileStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("DoWhileStatement");
        try self.fieldSpan(span);
        try self.fieldNode("body", data.body);
        try self.fieldNode("test", data.@"test");
        try self.endObject();
    }

    fn writeBreakStatement(self: *Self, data: ast.BreakStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("BreakStatement");
        try self.fieldSpan(span);
        try self.fieldNode("label", data.label);
        try self.endObject();
    }

    fn writeContinueStatement(self: *Self, data: ast.ContinueStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ContinueStatement");
        try self.fieldSpan(span);
        try self.fieldNode("label", data.label);
        try self.endObject();
    }

    fn writeLabeledStatement(self: *Self, data: ast.LabeledStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("LabeledStatement");
        try self.fieldSpan(span);
        try self.fieldNode("label", data.label);
        try self.fieldNode("body", data.body);
        try self.endObject();
    }

    fn writeWithStatement(self: *Self, data: ast.WithStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("WithStatement");
        try self.fieldSpan(span);
        try self.fieldNode("object", data.object);
        try self.fieldNode("body", data.body);
        try self.endObject();
    }

    fn writeEmptyStatement(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("EmptyStatement");
        try self.fieldSpan(span);
        try self.endObject();
    }

    fn writeReturnStatement(self: *Self, data: ast.ReturnStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ReturnStatement");
        try self.fieldSpan(span);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeThrowStatement(self: *Self, data: ast.ThrowStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ThrowStatement");
        try self.fieldSpan(span);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeTryStatement(self: *Self, data: ast.TryStatement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("TryStatement");
        try self.fieldSpan(span);
        try self.fieldNode("block", data.block);
        try self.fieldNode("handler", data.handler);
        try self.fieldNode("finalizer", data.finalizer);
        try self.endObject();
    }

    fn writeCatchClause(self: *Self, data: ast.CatchClause, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("CatchClause");
        try self.fieldSpan(span);
        try self.fieldNode("param", data.param);
        try self.fieldNode("body", data.body);
        try self.endObject();
    }

    fn writeDebuggerStatement(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("DebuggerStatement");
        try self.fieldSpan(span);
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

    fn writeConditionalExpression(self: *Self, data: ast.ConditionalExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ConditionalExpression");
        try self.fieldSpan(span);
        try self.fieldNode("test", data.@"test");
        try self.fieldNode("consequent", data.consequent);
        try self.fieldNode("alternate", data.alternate);
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
        const raw = self.tree.getSourceText(data.raw_start, data.raw_len);

        self.scratch.clearRetainingCapacity();

        // normalize line endings: CRLF (\r\n) and standalone CR (\r) -> LF (\n) per spec
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

        try self.beginObject();
        try self.fieldType("TemplateElement");
        try self.fieldSpan(span);
        try self.field("value");
        try self.beginObject();
        try self.fieldString("raw", self.scratch.items);
        try self.field("cooked");
        try self.writeDecodedString(self.scratch.items);
        try self.endObject();
        try self.fieldBool("tail", data.tail);
        try self.endObject();
    }

    fn writeStringLiteral(self: *Self, data: ast.StringLiteral, span: ast.Span) !void {
        const raw = self.tree.getSourceText(data.raw_start, data.raw_len);
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.field("value");
        // JSX attribute strings don't process escapes, regular JS strings do
        if (self.in_jsx_attribute)
            try self.writeString(raw[1 .. raw.len - 1])
        else
            try self.writeDecodedString(raw[1 .. raw.len - 1]);
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeNumericLiteral(self: *Self, data: ast.NumericLiteral, span: ast.Span) !void {
        const raw = self.tree.getSourceText(data.raw_start, data.raw_len);
        var buf: [64]u8 = undefined;
        const num_str: ?[]const u8 = parseJSNumeric(&buf, raw) catch null;

        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
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
        const raw = self.tree.getSourceText(data.raw_start, data.raw_len);
        self.scratch.clearRetainingCapacity();
        try self.scratch.appendSlice(self.allocator, "(BigInt) ");
        try self.scratch.appendSlice(self.allocator, raw);
        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.fieldString("value", self.scratch.items);
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
        const pattern = self.tree.getSourceText(data.pattern_start, data.pattern_len);
        const flags = self.tree.getSourceText(data.flags_start, data.flags_len);

        self.scratch.clearRetainingCapacity();

        try self.scratch.appendSlice(self.allocator, flags);

        std.mem.sort(u8, self.scratch.items, {}, comptime std.sort.asc(u8));

        try self.beginObject();
        try self.fieldType("Literal");
        try self.fieldSpan(span);
        try self.field("value");
        try self.writeTaggedRegExpLiteralValue(pattern, flags);
        try self.field("raw");
        try self.writeRegExpLiteralRaw(pattern, flags);
        try self.field("regex");
        try self.beginObject();
        try self.fieldString("pattern", pattern);
        try self.fieldString("flags", self.scratch.items);
        try self.endObject();
        try self.endObject();
    }

    fn writeRegExpLiteralRaw(self: *Self, pattern: []const u8, flags: []const u8) !void {
        try self.writeByte('"');
        try self.writeByte('/');
        try self.writeJsonEscaped(pattern);
        try self.writeByte('/');
        try self.write(flags);
        try self.writeByte('"');
    }

    fn writeTaggedRegExpLiteralValue(self: *Self, pattern: []const u8, flags: []const u8) !void {
        try self.writeByte('"');
        try self.write("(RegExp) ");
        try self.writeByte('/');
        try self.writeJsonEscaped(pattern);
        try self.writeByte('/');
        try self.write(flags);
        try self.writeByte('"');
    }

    fn writeIdentifier(self: *Self, data: anytype, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Identifier");
        try self.fieldSpan(span);
        try self.field("name");
        try self.writeDecodedString(self.tree.getSourceText(data.name_start, data.name_len));
        try self.endObject();
    }

    fn writePrivateIdentifier(self: *Self, data: ast.PrivateIdentifier, span: ast.Span) !void {
        const name = self.tree.getSourceText(data.name_start, data.name_len);
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

    fn writeClassBody(self: *Self, data: ast.ClassBody, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ClassBody");
        try self.fieldSpan(span);
        try self.fieldNodeArray("body", data.body);
        try self.endObject();
    }

    fn writeMethodDefinition(self: *Self, data: ast.MethodDefinition, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("MethodDefinition");
        try self.fieldSpan(span);
        try self.fieldNodeArray("decorators", data.decorators);
        try self.fieldNode("key", data.key);
        try self.fieldNode("value", data.value);
        try self.fieldString("kind", data.kind.toString());
        try self.fieldBool("computed", data.computed);
        try self.fieldBool("static", data.static);
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

    fn writeStaticBlock(self: *Self, data: ast.StaticBlock, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("StaticBlock");
        try self.fieldSpan(span);
        try self.fieldNodeArray("body", data.body);
        try self.endObject();
    }

    fn writeSuper(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Super");
        try self.fieldSpan(span);
        try self.endObject();
    }

    fn writeImportExpression(self: *Self, data: ast.ImportExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ImportExpression");
        try self.fieldSpan(span);
        try self.fieldNode("source", data.source);
        try self.fieldNode("options", data.options);
        try self.writeImportPhase(data.phase);
        try self.endObject();
    }

    fn writeImportDeclaration(self: *Self, data: ast.ImportDeclaration, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ImportDeclaration");
        try self.fieldSpan(span);
        try self.fieldNodeArray("specifiers", data.specifiers);
        try self.fieldNode("source", data.source);
        try self.writeImportPhase(data.phase);
        try self.field("attributes");
        try self.beginArray();
        for (self.getExtra(data.attributes)) |idx| try self.elemNode(idx);
        try self.endArray();
        try self.endObject();
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

    fn writeImportSpecifier(self: *Self, data: ast.ImportSpecifier, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ImportSpecifier");
        try self.fieldSpan(span);
        try self.fieldNode("imported", data.imported);
        try self.fieldNode("local", data.local);
        try self.endObject();
    }

    fn writeImportDefaultSpecifier(self: *Self, data: ast.ImportDefaultSpecifier, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ImportDefaultSpecifier");
        try self.fieldSpan(span);
        try self.fieldNode("local", data.local);
        try self.endObject();
    }

    fn writeImportNamespaceSpecifier(self: *Self, data: ast.ImportNamespaceSpecifier, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ImportNamespaceSpecifier");
        try self.fieldSpan(span);
        try self.fieldNode("local", data.local);
        try self.endObject();
    }

    fn writeImportAttribute(self: *Self, data: ast.ImportAttribute, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ImportAttribute");
        try self.fieldSpan(span);
        try self.fieldNode("key", data.key);
        try self.fieldNode("value", data.value);
        try self.endObject();
    }

    fn writeExportNamedDeclaration(self: *Self, data: ast.ExportNamedDeclaration, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ExportNamedDeclaration");
        try self.fieldSpan(span);
        try self.fieldNode("declaration", data.declaration);
        try self.fieldNodeArray("specifiers", data.specifiers);
        try self.fieldNode("source", data.source);
        try self.field("attributes");
        try self.beginArray();
        for (self.getExtra(data.attributes)) |idx| try self.elemNode(idx);
        try self.endArray();
        try self.endObject();
    }

    fn writeExportDefaultDeclaration(self: *Self, data: ast.ExportDefaultDeclaration, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ExportDefaultDeclaration");
        try self.fieldSpan(span);
        try self.fieldNode("declaration", data.declaration);
        try self.endObject();
    }

    fn writeExportAllDeclaration(self: *Self, data: ast.ExportAllDeclaration, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ExportAllDeclaration");
        try self.fieldSpan(span);
        try self.fieldNode("exported", data.exported);
        try self.fieldNode("source", data.source);
        try self.field("attributes");
        try self.beginArray();
        for (self.getExtra(data.attributes)) |idx| try self.elemNode(idx);
        try self.endArray();
        try self.endObject();
    }

    fn writeExportSpecifier(self: *Self, data: ast.ExportSpecifier, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ExportSpecifier");
        try self.fieldSpan(span);
        try self.fieldNode("local", data.local);
        try self.fieldNode("exported", data.exported);
        try self.endObject();
    }

    fn writeTSExportAssignment(self: *Self, data: ast.TSExportAssignment, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("TSExportAssignment");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.endObject();
    }

    fn writeTSNamespaceExportDeclaration(self: *Self, data: ast.TSNamespaceExportDeclaration, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("TSNamespaceExportDeclaration");
        try self.fieldSpan(span);
        try self.fieldNode("id", data.id);
        try self.endObject();
    }

    fn writeMemberExpression(self: *Self, data: ast.MemberExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("MemberExpression");
        try self.fieldSpan(span);
        try self.fieldNode("object", data.object);
        try self.fieldNode("property", data.property);
        try self.fieldBool("computed", data.computed);
        try self.fieldBool("optional", data.optional);
        try self.endObject();
    }

    fn writeCallExpression(self: *Self, data: ast.CallExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("CallExpression");
        try self.fieldSpan(span);
        try self.fieldNode("callee", data.callee);
        try self.fieldNodeArray("arguments", data.arguments);
        try self.fieldBool("optional", data.optional);
        try self.endObject();
    }

    fn writeChainExpression(self: *Self, data: ast.ChainExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ChainExpression");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.endObject();
    }

    fn writeTaggedTemplateExpression(self: *Self, data: ast.TaggedTemplateExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("TaggedTemplateExpression");
        try self.fieldSpan(span);
        try self.fieldNode("tag", data.tag);
        try self.fieldNode("quasi", data.quasi);
        try self.endObject();
    }

    fn writeNewExpression(self: *Self, data: ast.NewExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("NewExpression");
        try self.fieldSpan(span);
        try self.fieldNode("callee", data.callee);
        try self.fieldNodeArray("arguments", data.arguments);
        try self.endObject();
    }

    fn writeAwaitExpression(self: *Self, data: ast.AwaitExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("AwaitExpression");
        try self.fieldSpan(span);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeYieldExpression(self: *Self, data: ast.YieldExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("YieldExpression");
        try self.fieldSpan(span);
        try self.fieldBool("delegate", data.delegate);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeMetaProperty(self: *Self, data: ast.MetaProperty, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("MetaProperty");
        try self.fieldSpan(span);
        try self.fieldNode("meta", data.meta);
        try self.fieldNode("property", data.property);
        try self.endObject();
    }

    fn writeDecorator(self: *Self, data: ast.Decorator, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("Decorator");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.endObject();
    }

    fn writeThisExpression(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("ThisExpression");
        try self.fieldSpan(span);
        try self.endObject();
    }

    fn writeJSXElement(self: *Self, data: ast.JSXElement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXElement");
        try self.fieldSpan(span);
        try self.fieldNode("openingElement", data.opening_element);
        try self.fieldNodeArray("children", data.children);
        try self.fieldNode("closingElement", data.closing_element);
        try self.endObject();
    }

    fn writeJSXOpeningElement(self: *Self, data: ast.JSXOpeningElement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXOpeningElement");
        try self.fieldSpan(span);
        try self.fieldNode("name", data.name);
        try self.fieldNodeArray("attributes", data.attributes);
        try self.fieldBool("selfClosing", data.self_closing);
        try self.endObject();
    }

    fn writeJSXClosingElement(self: *Self, data: ast.JSXClosingElement, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXClosingElement");
        try self.fieldSpan(span);
        try self.fieldNode("name", data.name);
        try self.endObject();
    }

    fn writeJSXFragment(self: *Self, data: ast.JSXFragment, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXFragment");
        try self.fieldSpan(span);
        try self.fieldNode("openingFragment", data.opening_fragment);
        try self.fieldNodeArray("children", data.children);
        try self.fieldNode("closingFragment", data.closing_fragment);
        try self.endObject();
    }

    fn writeJSXOpeningFragment(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXOpeningFragment");
        try self.fieldSpan(span);
        try self.fieldEmptyArray("attributes");
        try self.fieldBool("selfClosing", false);
        try self.endObject();
    }

    fn writeJSXClosingFragment(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXClosingFragment");
        try self.fieldSpan(span);
        try self.endObject();
    }

    fn writeJSXIdentifier(self: *Self, data: ast.JSXIdentifier, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXIdentifier");
        try self.fieldSpan(span);
        try self.fieldString("name", self.tree.getSourceText(data.name_start, data.name_len));
        try self.endObject();
    }

    fn writeJSXNamespacedName(self: *Self, data: ast.JSXNamespacedName, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXNamespacedName");
        try self.fieldSpan(span);
        try self.fieldNode("namespace", data.namespace);
        try self.fieldNode("name", data.name);
        try self.endObject();
    }

    fn writeJSXMemberExpression(self: *Self, data: ast.JSXMemberExpression, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXMemberExpression");
        try self.fieldSpan(span);
        try self.fieldNode("object", data.object);
        try self.fieldNode("property", data.property);
        try self.endObject();
    }

    fn writeJSXAttribute(self: *Self, data: ast.JSXAttribute, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXAttribute");
        try self.fieldSpan(span);
        try self.fieldNode("name", data.name);
        self.in_jsx_attribute = true;
        try self.fieldNode("value", data.value);
        self.in_jsx_attribute = false;
        try self.endObject();
    }

    fn writeJSXSpreadAttribute(self: *Self, data: ast.JSXSpreadAttribute, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXSpreadAttribute");
        try self.fieldSpan(span);
        try self.fieldNode("argument", data.argument);
        try self.endObject();
    }

    fn writeJSXExpressionContainer(self: *Self, data: ast.JSXExpressionContainer, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXExpressionContainer");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.endObject();
    }

    fn writeJSXEmptyExpression(self: *Self, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXEmptyExpression");
        try self.fieldSpan(span);
        try self.endObject();
    }

    fn writeJSXText(self: *Self, data: ast.JSXText, span: ast.Span) !void {
        const raw = self.tree.getSourceText(data.raw_start, data.raw_len);
        try self.beginObject();
        try self.fieldType("JSXText");
        try self.fieldSpan(span);
        try self.fieldString("value", raw);
        try self.fieldString("raw", raw);
        try self.endObject();
    }

    fn writeJSXSpreadChild(self: *Self, data: ast.JSXSpreadChild, span: ast.Span) !void {
        try self.beginObject();
        try self.fieldType("JSXSpreadChild");
        try self.fieldSpan(span);
        try self.fieldNode("expression", data.expression);
        try self.endObject();
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
            try self.fieldString("value", comment.getValue(self.tree.source));
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
                        if (util.Utf.parseHexVariable(input, i + 3, 16)) |r| {
                            if (r.has_digits and r.end < input.len and input[r.end] == '}') {
                                try self.writeCodePointAsJson(r.value);
                                i = r.end + 1;
                                continue;
                            }
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
                '\r', '\n' => i += 1 + util.Utf.asciiLineTerminatorLen(input, i + 1),
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

    /// writes a Unicode code point as JSON. ASCII chars are escaped if needed, non-ASCII are UTF-8 encoded.
    fn writeCodePointAsJson(self: *Self, cp: u21) !void {
        if (cp < 0x80) {
            try self.writeJsonEscapedChar(@intCast(cp));
        } else {
            var buf: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(cp, &buf) catch {
                try self.write("\u{FFFD}"); // replacement character for invalid code points
                return;
            };
            try self.write(buf[0..len]);
        }
    }
};

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
    // remove underscores
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

    // output in scientific notation
    return std.fmt.bufPrint(outbuf, "{e}", .{val});
}

pub fn toJSON(tree: *const ast.ParseTree, allocator: std.mem.Allocator, options: EstreeJsonOptions) ![]u8 {
    return Serializer.serialize(tree, allocator, options);
}
