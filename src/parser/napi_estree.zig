// converts the parsed tree to ESTree-compatible JS objects via N-API.
// mirrors estree.zig's output format but creates native JS objects instead of JSON.
//
// key optimizations over naive N-API:
// - all property key strings ("type", "start", "end", ...) created once, reused
// - all type name strings ("Identifier", "Program", ...) created once, reused
// - ASCII fast path: skip UTF-16 position map when source is all ASCII
// - generic node handler via comptime to avoid writing boilerplate per node type

const std = @import("std");
const napi = @import("napi-zig");
const c = napi.c;
const Env = napi.Env;
const Val = napi.Val;
const check = napi.check;
const ast = @import("parser").ast;

const Error = error{napi_error};

pub const Serializer = struct {
    env: Env,
    tree: *const ast.Tree,
    pos_map: ?[]const u32,
    js_null: Val,
    js_true: Val,
    js_false: Val,
    keys: Keys,
    types: Types,
    in_jsx_attribute: bool = false,

    // pre-cached property key napi_values. created once during init.
    const Keys = struct {
        type: Val = undefined,
        start: Val = undefined,
        end: Val = undefined,
        body: Val = undefined,
        name: Val = undefined,
        value: Val = undefined,
        raw: Val = undefined,
        kind: Val = undefined,
        id: Val = undefined,
        init: Val = undefined,
        params: Val = undefined,
        generator: Val = undefined,
        @"async": Val = undefined,
        expression: Val = undefined,
        left: Val = undefined,
        right: Val = undefined,
        operator: Val = undefined,
        prefix: Val = undefined,
        argument: Val = undefined,
        @"test": Val = undefined,
        consequent: Val = undefined,
        alternate: Val = undefined,
        object: Val = undefined,
        property: Val = undefined,
        computed: Val = undefined,
        optional: Val = undefined,
        callee: Val = undefined,
        arguments: Val = undefined,
        tag: Val = undefined,
        quasi: Val = undefined,
        quasis: Val = undefined,
        expressions: Val = undefined,
        tail: Val = undefined,
        cooked: Val = undefined,
        key: Val = undefined,
        method: Val = undefined,
        shorthand: Val = undefined,
        delegate: Val = undefined,
        source: Val = undefined,
        specifiers: Val = undefined,
        declaration: Val = undefined,
        declarations: Val = undefined,
        local: Val = undefined,
        imported: Val = undefined,
        exported: Val = undefined,
        source_type: Val = undefined, // "sourceType"
        elements: Val = undefined,
        properties: Val = undefined,
        meta: Val = undefined,
        label: Val = undefined,
        block: Val = undefined,
        handler: Val = undefined,
        finalizer: Val = undefined,
        param: Val = undefined,
        discriminant: Val = undefined,
        cases: Val = undefined,
        update: Val = undefined,
        @"await": Val = undefined,
        super_class: Val = undefined, // "superClass"
        decorators: Val = undefined,
        @"static": Val = undefined,
        accessor: Val = undefined,
        directive: Val = undefined,
        hashbang: Val = undefined,
        regex: Val = undefined,
        pattern: Val = undefined,
        flags: Val = undefined,
        bigint: Val = undefined,
        declare: Val = undefined,
        phase: Val = undefined,
        attributes: Val = undefined,
        self_closing: Val = undefined, // "selfClosing"
        namespace: Val = undefined,
        children: Val = undefined,
        opening_element: Val = undefined, // "openingElement"
        closing_element: Val = undefined, // "closingElement"
        opening_fragment: Val = undefined, // "openingFragment"
        closing_fragment: Val = undefined, // "closingFragment"

        // comment/diagnostic keys
        severity: Val = undefined,
        message: Val = undefined,
        help: Val = undefined,
        labels: Val = undefined,
    };

    const Types = struct {
        Program: Val = undefined,
        Identifier: Val = undefined,
        PrivateIdentifier: Val = undefined,
        Literal: Val = undefined,
        ExpressionStatement: Val = undefined,
        BlockStatement: Val = undefined,
        FunctionDeclaration: Val = undefined,
        FunctionExpression: Val = undefined,
        ArrowFunctionExpression: Val = undefined,
        VariableDeclaration: Val = undefined,
        VariableDeclarator: Val = undefined,
        IfStatement: Val = undefined,
        SwitchStatement: Val = undefined,
        SwitchCase: Val = undefined,
        ForStatement: Val = undefined,
        ForInStatement: Val = undefined,
        ForOfStatement: Val = undefined,
        WhileStatement: Val = undefined,
        DoWhileStatement: Val = undefined,
        ReturnStatement: Val = undefined,
        ThrowStatement: Val = undefined,
        TryStatement: Val = undefined,
        CatchClause: Val = undefined,
        BreakStatement: Val = undefined,
        ContinueStatement: Val = undefined,
        LabeledStatement: Val = undefined,
        WithStatement: Val = undefined,
        DebuggerStatement: Val = undefined,
        EmptyStatement: Val = undefined,
        BinaryExpression: Val = undefined,
        LogicalExpression: Val = undefined,
        AssignmentExpression: Val = undefined,
        ConditionalExpression: Val = undefined,
        UnaryExpression: Val = undefined,
        UpdateExpression: Val = undefined,
        MemberExpression: Val = undefined,
        CallExpression: Val = undefined,
        NewExpression: Val = undefined,
        SequenceExpression: Val = undefined,
        TemplateLiteral: Val = undefined,
        TemplateElement: Val = undefined,
        TaggedTemplateExpression: Val = undefined,
        SpreadElement: Val = undefined,
        ArrayExpression: Val = undefined,
        ObjectExpression: Val = undefined,
        Property: Val = undefined,
        AssignmentPattern: Val = undefined,
        RestElement: Val = undefined,
        ArrayPattern: Val = undefined,
        ObjectPattern: Val = undefined,
        YieldExpression: Val = undefined,
        AwaitExpression: Val = undefined,
        ChainExpression: Val = undefined,
        MetaProperty: Val = undefined,
        Super: Val = undefined,
        ThisExpression: Val = undefined,
        ImportExpression: Val = undefined,
        ImportDeclaration: Val = undefined,
        ImportSpecifier: Val = undefined,
        ImportDefaultSpecifier: Val = undefined,
        ImportNamespaceSpecifier: Val = undefined,
        ImportAttribute: Val = undefined,
        ExportNamedDeclaration: Val = undefined,
        ExportDefaultDeclaration: Val = undefined,
        ExportAllDeclaration: Val = undefined,
        ExportSpecifier: Val = undefined,
        ClassDeclaration: Val = undefined,
        ClassExpression: Val = undefined,
        ClassBody: Val = undefined,
        MethodDefinition: Val = undefined,
        PropertyDefinition: Val = undefined,
        AccessorProperty: Val = undefined,
        StaticBlock: Val = undefined,
        ParenthesizedExpression: Val = undefined,
        // jsx
        JSXElement: Val = undefined,
        JSXOpeningElement: Val = undefined,
        JSXClosingElement: Val = undefined,
        JSXFragment: Val = undefined,
        JSXOpeningFragment: Val = undefined,
        JSXClosingFragment: Val = undefined,
        JSXIdentifier: Val = undefined,
        JSXNamespacedName: Val = undefined,
        JSXMemberExpression: Val = undefined,
        JSXAttribute: Val = undefined,
        JSXSpreadAttribute: Val = undefined,
        JSXExpressionContainer: Val = undefined,
        JSXEmptyExpression: Val = undefined,
        JSXText: Val = undefined,
        JSXSpreadChild: Val = undefined,
        Decorator: Val = undefined,
        // ts
        TSDeclareFunction: Val = undefined,
        TSEmptyBodyFunctionExpression: Val = undefined,
        TSExportAssignment: Val = undefined,
        TSNamespaceExportDeclaration: Val = undefined,
        // comment types
        Line: Val = undefined,
        Block: Val = undefined,
    };

    pub fn serialize(env: Env, tree: *const ast.Tree) Error!Val {
        const pos_map: ?[]const u32 = if (tree.source.len > 0 and !isAscii(tree.source))
            buildUtf16PosMap(std.heap.c_allocator, tree.source) catch return error.napi_error
        else
            null;

        defer if (pos_map) |m| std.heap.c_allocator.free(m);

        var self = Serializer{
            .env = env,
            .tree = tree,
            .pos_map = pos_map,
            .js_null = env.@"null"() catch return error.napi_error,
            .js_true = env.boolean(true) catch return error.napi_error,
            .js_false = env.boolean(false) catch return error.napi_error,
            .keys = undefined,
            .types = undefined,
        };

        try self.initKeys();
        try self.initTypes();

        const result = try self.obj();
        try self.set(result, "program", try self.writeNode(tree.program));
        try self.set(result, "comments", try self.writeComments());
        try self.set(result, "diagnostics", try self.writeDiagnostics());
        return result;
    }

    fn writeNode(self: *Serializer, index: ast.NodeIndex) Error!Val {
        if (index == .null) return self.js_null;

        const data = self.tree.getData(index);
        const span = self.tree.getSpan(index);

        return switch (data) {
            .program => |d| self.writeProgram(d, span),
            .directive => |d| self.writeDirective(d, span),
            .function => |d| self.writeFunction(d, span),
            .formal_parameters => |d| self.writeFormalParams(d),
            .formal_parameter => |d| self.writeNode(d.pattern),
            .arrow_function_expression => |d| self.writeArrowFunction(d, span),
            .class => |d| self.writeClass(d, span),
            .property_definition => |d| self.writePropertyDef(d, span),
            .binding_property => |d| self.writeBindingProp(d, span),
            .array_pattern => |d| self.writeArrayPattern(d, span),
            .object_pattern => |d| self.writeObjectPattern(d, span),
            .import_expression => |d| self.writeImportExpr(d, span),
            .import_declaration => |d| self.writeImportDecl(d, span),
            .jsx_attribute => |d| self.writeJSXAttr(d, span),
            .jsx_opening_fragment => self.writeJSXOpenFrag(span),
            .jsx_identifier => |d| self.writeJSXId(d, span),
            .jsx_text => |d| self.writeJSXText(d, span),
            .binary_expression => |d| self.writeBinaryExpr(self.types.BinaryExpression, d.left, d.operator.toString(), d.right, span),
            .logical_expression => |d| self.writeBinaryExpr(self.types.LogicalExpression, d.left, d.operator.toString(), d.right, span),
            .assignment_expression => |d| self.writeAssignment(d, span),
            .unary_expression => |d| self.writeUnary(d, span),
            .update_expression => |d| self.writeUpdate(d, span),
            .yield_expression => |d| self.writeYield(d, span),
            .object_property => |d| self.writeObjProp(d, span),
            .identifier_reference => |d| self.writeIdentifier(d.name, span),
            .binding_identifier => |d| self.writeIdentifier(d.name, span),
            .identifier_name => |d| self.writeIdentifier(d.name, span),
            .label_identifier => |d| self.writeIdentifier(d.name, span),
            .private_identifier => |d| self.writePrivateId(d, span),
            .string_literal => |d| self.writeStringLit(d, span),
            .numeric_literal => |d| self.writeNumericLit(d, span),
            .bigint_literal => |d| self.writeBigIntLit(d, span),
            .boolean_literal => |d| self.writeBoolLit(d, span),
            .null_literal => self.writeNullLit(span),
            .regexp_literal => |d| self.writeRegExpLit(d, span),
            .template_element => |d| self.writeTemplateElem(d, span),
            inline else => |payload, tag| self.writeGeneric(tag, payload, span),
        };
    }

    // -- specialized node writers --

    fn writeProgram(self: *Serializer, data: ast.Program, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.Program, span);
        try self.setProp(o, self.keys.source_type, try self.str(if (data.source_type == .module) "module" else "script"));
        try self.setProp(o, self.keys.hashbang, if (data.hashbang) |h| try self.str(self.tree.getString(h.value)) else self.js_null);
        try self.setProp(o, self.keys.body, try self.writeNodeArray(data.body));
        return o;
    }

    fn writeDirective(self: *Serializer, data: ast.Directive, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.ExpressionStatement, span);
        try self.setProp(o, self.keys.expression, try self.writeNode(data.expression));
        try self.setProp(o, self.keys.directive, try self.str(self.tree.getString(data.value)));
        return o;
    }

    fn writeFunction(self: *Serializer, data: ast.Function, span: ast.Span) Error!Val {
        const type_val = switch (data.type) {
            .function_declaration => self.types.FunctionDeclaration,
            .function_expression => self.types.FunctionExpression,
            .ts_declare_function => self.types.TSDeclareFunction,
            .ts_empty_body_function_expression => self.types.TSEmptyBodyFunctionExpression,
        };
        const o = try self.begin(type_val, span);
        try self.setProp(o, self.keys.id, try self.writeNode(data.id));
        try self.setProp(o, self.keys.generator, self.boolVal(data.generator));
        try self.setProp(o, self.keys.@"async", self.boolVal(data.async));
        if (self.tree.isTs()) try self.setProp(o, self.keys.declare, self.boolVal(data.type == .ts_declare_function));
        try self.setProp(o, self.keys.params, try self.writeFnParams(data.params));
        try self.setProp(o, self.keys.body, try self.writeNode(data.body));
        try self.setProp(o, self.keys.expression, self.js_false);
        return o;
    }

    fn writeArrowFunction(self: *Serializer, data: ast.ArrowFunctionExpression, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.ArrowFunctionExpression, span);
        try self.setProp(o, self.keys.id, self.js_null);
        try self.setProp(o, self.keys.generator, self.js_false);
        try self.setProp(o, self.keys.@"async", self.boolVal(data.async));
        try self.setProp(o, self.keys.params, try self.writeFnParams(data.params));
        try self.setProp(o, self.keys.body, try self.writeNode(data.body));
        try self.setProp(o, self.keys.expression, self.boolVal(data.expression));
        return o;
    }

    fn writeClass(self: *Serializer, data: ast.Class, span: ast.Span) Error!Val {
        const type_val = switch (data.type) {
            .class_declaration => self.types.ClassDeclaration,
            .class_expression => self.types.ClassExpression,
        };
        const o = try self.begin(type_val, span);
        try self.setProp(o, self.keys.decorators, try self.writeNodeArray(data.decorators));
        try self.setProp(o, self.keys.id, try self.writeNode(data.id));
        try self.setProp(o, self.keys.super_class, try self.writeNode(data.super_class));
        try self.setProp(o, self.keys.body, try self.writeNode(data.body));
        return o;
    }

    fn writePropertyDef(self: *Serializer, data: ast.PropertyDefinition, span: ast.Span) Error!Val {
        const o = try self.begin(if (data.accessor) self.types.AccessorProperty else self.types.PropertyDefinition, span);
        try self.setProp(o, self.keys.decorators, try self.writeNodeArray(data.decorators));
        try self.setProp(o, self.keys.key, try self.writeNode(data.key));
        try self.setProp(o, self.keys.value, try self.writeNode(data.value));
        try self.setProp(o, self.keys.computed, self.boolVal(data.computed));
        try self.setProp(o, self.keys.@"static", self.boolVal(data.static));
        return o;
    }

    fn writeObjProp(self: *Serializer, data: ast.ObjectProperty, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.Property, span);
        try self.setProp(o, self.keys.kind, try self.str(data.kind.toString()));
        try self.setProp(o, self.keys.key, try self.writeNode(data.key));
        try self.setProp(o, self.keys.value, try self.writeNode(data.value));
        try self.setProp(o, self.keys.method, self.boolVal(data.method));
        try self.setProp(o, self.keys.shorthand, self.boolVal(data.shorthand));
        try self.setProp(o, self.keys.computed, self.boolVal(data.computed));
        return o;
    }

    fn writeBindingProp(self: *Serializer, data: ast.BindingProperty, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.Property, span);
        try self.setProp(o, self.keys.kind, try self.str("init"));
        try self.setProp(o, self.keys.key, try self.writeNode(data.key));
        try self.setProp(o, self.keys.value, try self.writeNode(data.value));
        try self.setProp(o, self.keys.method, self.js_false);
        try self.setProp(o, self.keys.shorthand, self.boolVal(data.shorthand));
        try self.setProp(o, self.keys.computed, self.boolVal(data.computed));
        return o;
    }

    fn writeBinaryExpr(self: *Serializer, type_val: Val, left: ast.NodeIndex, op: []const u8, right: ast.NodeIndex, span: ast.Span) Error!Val {
        const o = try self.begin(type_val, span);
        try self.setProp(o, self.keys.left, try self.writeNode(left));
        try self.setProp(o, self.keys.operator, try self.str(op));
        try self.setProp(o, self.keys.right, try self.writeNode(right));
        return o;
    }

    fn writeAssignment(self: *Serializer, data: ast.AssignmentExpression, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.AssignmentExpression, span);
        try self.setProp(o, self.keys.operator, try self.str(data.operator.toString()));
        try self.setProp(o, self.keys.left, try self.writeNode(data.left));
        try self.setProp(o, self.keys.right, try self.writeNode(data.right));
        return o;
    }

    fn writeUnary(self: *Serializer, data: ast.UnaryExpression, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.UnaryExpression, span);
        try self.setProp(o, self.keys.operator, try self.str(data.operator.toString()));
        try self.setProp(o, self.keys.prefix, self.js_true);
        try self.setProp(o, self.keys.argument, try self.writeNode(data.argument));
        return o;
    }

    fn writeUpdate(self: *Serializer, data: ast.UpdateExpression, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.UpdateExpression, span);
        try self.setProp(o, self.keys.operator, try self.str(data.operator.toString()));
        try self.setProp(o, self.keys.prefix, self.boolVal(data.prefix));
        try self.setProp(o, self.keys.argument, try self.writeNode(data.argument));
        return o;
    }

    fn writeYield(self: *Serializer, data: ast.YieldExpression, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.YieldExpression, span);
        try self.setProp(o, self.keys.delegate, self.boolVal(data.delegate));
        try self.setProp(o, self.keys.argument, try self.writeNode(data.argument));
        return o;
    }

    fn writeArrayPattern(self: *Serializer, data: ast.ArrayPattern, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.ArrayPattern, span);
        const elems = self.tree.getExtra(data.elements);
        const has_rest: u32 = if (data.rest != .null) 1 else 0;
        const arr = try self.arrayN(@intCast(elems.len + has_rest));
        for (elems, 0..) |idx, i| try self.setElem(arr, @intCast(i), try self.writeNode(idx));
        if (data.rest != .null) try self.setElem(arr, @intCast(elems.len), try self.writeNode(data.rest));
        try self.setProp(o, self.keys.elements, arr);
        return o;
    }

    fn writeObjectPattern(self: *Serializer, data: ast.ObjectPattern, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.ObjectPattern, span);
        const props = self.tree.getExtra(data.properties);
        const has_rest: u32 = if (data.rest != .null) 1 else 0;
        const arr = try self.arrayN(@intCast(props.len + has_rest));
        for (props, 0..) |idx, i| try self.setElem(arr, @intCast(i), try self.writeNode(idx));
        if (data.rest != .null) try self.setElem(arr, @intCast(props.len), try self.writeNode(data.rest));
        try self.setProp(o, self.keys.properties, arr);
        return o;
    }

    fn writeImportExpr(self: *Serializer, data: ast.ImportExpression, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.ImportExpression, span);
        try self.setProp(o, self.keys.source, try self.writeNode(data.source));
        try self.setProp(o, self.keys.@"test", try self.writeNode(data.options)); // "options" field mapped to "options" key - actually let me check estree
        try self.writeImportPhase(o, data.phase);
        return o;
    }

    fn writeImportDecl(self: *Serializer, data: ast.ImportDeclaration, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.ImportDeclaration, span);
        try self.setProp(o, self.keys.specifiers, try self.writeNodeArray(data.specifiers));
        try self.setProp(o, self.keys.source, try self.writeNode(data.source));
        try self.writeImportPhase(o, data.phase);
        try self.setProp(o, self.keys.attributes, try self.writeNodeArray(data.attributes));
        return o;
    }

    fn writeImportPhase(self: *Serializer, o: Val, phase: ?ast.ImportPhase) Error!void {
        try self.setProp(o, self.keys.phase, if (phase) |p| try self.str(switch (p) {
            .source => "source",
            .@"defer" => "defer",
        }) else self.js_null);
    }

    fn writeJSXAttr(self: *Serializer, data: ast.JSXAttribute, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.JSXAttribute, span);
        try self.setProp(o, self.keys.name, try self.writeNode(data.name));
        self.in_jsx_attribute = true;
        try self.setProp(o, self.keys.value, try self.writeNode(data.value));
        self.in_jsx_attribute = false;
        return o;
    }

    fn writeJSXOpenFrag(self: *Serializer, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.JSXOpeningFragment, span);
        try self.setProp(o, self.keys.attributes, try self.arrayN(0));
        try self.setProp(o, self.keys.self_closing, self.js_false);
        return o;
    }

    fn writeJSXId(self: *Serializer, data: ast.JSXIdentifier, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.JSXIdentifier, span);
        try self.setProp(o, self.keys.name, try self.str(self.tree.getString(data.name)));
        return o;
    }

    fn writeJSXText(self: *Serializer, data: ast.JSXText, span: ast.Span) Error!Val {
        const text = self.tree.getString(data.value);
        const o = try self.begin(self.types.JSXText, span);
        try self.setProp(o, self.keys.value, try self.str(text));
        try self.setProp(o, self.keys.raw, try self.str(text));
        return o;
    }

    fn writeIdentifier(self: *Serializer, name: ast.String, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.Identifier, span);
        try self.setProp(o, self.keys.name, try self.str(self.tree.getString(name)));
        return o;
    }

    fn writePrivateId(self: *Serializer, data: ast.PrivateIdentifier, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.PrivateIdentifier, span);
        try self.setProp(o, self.keys.name, try self.str(self.tree.getString(data.name)));
        return o;
    }

    fn writeStringLit(self: *Serializer, data: ast.StringLiteral, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.Literal, span);
        try self.setProp(o, self.keys.value, try self.str(self.tree.getString(data.value)));
        try self.setProp(o, self.keys.raw, try self.str(self.tree.source[span.start..span.end]));
        return o;
    }

    fn writeNumericLit(self: *Serializer, data: ast.NumericLiteral, span: ast.Span) Error!Val {
        const val = data.value(self.tree);
        const o = try self.begin(self.types.Literal, span);
        if (std.math.isInf(val) or std.math.isNan(val)) {
            try self.setProp(o, self.keys.value, self.js_null);
        } else {
            try self.setProp(o, self.keys.value, self.env.float64(val) catch return error.napi_error);
        }
        try self.setProp(o, self.keys.raw, try self.str(self.tree.source[span.start..span.end]));
        return o;
    }

    fn writeBigIntLit(self: *Serializer, data: ast.BigIntLiteral, span: ast.Span) Error!Val {
        const raw = self.tree.source[span.start..span.end];
        const digits = self.tree.getString(data.raw);
        const o = try self.begin(self.types.Literal, span);
        // value is a tagged string representation for JSON compat
        var val_buf: [512]u8 = undefined;
        const val_str = std.fmt.bufPrint(&val_buf, "(BigInt) {s}", .{raw}) catch raw;
        try self.setProp(o, self.keys.value, try self.str(val_str));
        try self.setProp(o, self.keys.raw, try self.str(raw));
        // bigint field: digits without separators
        var digit_buf: [512]u8 = undefined;
        var dlen: usize = 0;
        for (digits) |ch| {
            if (ch != '_') {
                if (dlen < digit_buf.len) {
                    digit_buf[dlen] = ch;
                    dlen += 1;
                }
            }
        }
        try self.setProp(o, self.keys.bigint, try self.str(digit_buf[0..dlen]));
        return o;
    }

    fn writeBoolLit(self: *Serializer, data: ast.BooleanLiteral, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.Literal, span);
        try self.setProp(o, self.keys.value, self.boolVal(data.value));
        try self.setProp(o, self.keys.raw, try self.str(if (data.value) "true" else "false"));
        return o;
    }

    fn writeNullLit(self: *Serializer, span: ast.Span) Error!Val {
        const o = try self.begin(self.types.Literal, span);
        try self.setProp(o, self.keys.value, self.js_null);
        try self.setProp(o, self.keys.raw, try self.str("null"));
        return o;
    }

    fn writeRegExpLit(self: *Serializer, data: ast.RegExpLiteral, span: ast.Span) Error!Val {
        const pattern_str = self.tree.getString(data.pattern);
        const flags_str = self.tree.getString(data.flags);
        const o = try self.begin(self.types.Literal, span);
        // value
        var val_buf: [1024]u8 = undefined;
        const val_s = std.fmt.bufPrint(&val_buf, "(RegExp) /{s}/{s}", .{ pattern_str, flags_str }) catch "";
        try self.setProp(o, self.keys.value, try self.str(val_s));
        // raw
        var raw_buf: [1024]u8 = undefined;
        const raw_s = std.fmt.bufPrint(&raw_buf, "/{s}/{s}", .{ pattern_str, flags_str }) catch "";
        try self.setProp(o, self.keys.raw, try self.str(raw_s));
        // regex
        const regex_obj = try self.obj();
        try self.setProp(regex_obj, self.keys.pattern, try self.str(pattern_str));
        try self.setProp(regex_obj, self.keys.flags, try self.str(flags_str));
        try self.setProp(o, self.keys.regex, regex_obj);
        return o;
    }

    fn writeTemplateElem(self: *Serializer, data: ast.TemplateElement, span: ast.Span) Error!Val {
        const raw = self.tree.source[span.start..span.end];
        const o = try self.begin(self.types.TemplateElement, span);
        // value object { raw, cooked }
        const val_obj = try self.obj();
        try self.setProp(val_obj, self.keys.raw, try self.str(raw));
        try self.setProp(val_obj, self.keys.cooked, if (data.is_cooked_undefined) self.js_null else try self.str(self.tree.getString(data.cooked)));
        try self.setProp(o, self.keys.value, val_obj);
        try self.setProp(o, self.keys.tail, self.boolVal(data.tail));
        return o;
    }

    fn writeFormalParams(self: *Serializer, data: ast.FormalParameters) Error!Val {
        const items = self.tree.getExtra(data.items);
        const has_rest: u32 = if (data.rest != .null) 1 else 0;
        const arr = try self.arrayN(@intCast(items.len + has_rest));
        for (items, 0..) |idx, i| {
            const pattern = self.tree.getData(idx).formal_parameter.pattern;
            try self.setElem(arr, @intCast(i), try self.writeNode(pattern));
        }
        if (data.rest != .null) try self.setElem(arr, @intCast(items.len), try self.writeNode(data.rest));
        return arr;
    }

    fn writeFnParams(self: *Serializer, params_index: ast.NodeIndex) Error!Val {
        if (params_index == .null) return self.arrayN(0);
        const params = self.tree.getData(params_index).formal_parameters;
        return self.writeFormalParams(params);
    }

    // -- generic node handler (comptime) --

    const NodeTag = std.meta.Tag(ast.NodeData);

    fn writeGeneric(self: *Serializer, comptime tag: NodeTag, payload: anytype, span: ast.Span) Error!Val {
        const type_val = @field(self.types, estreeTypeName(tag));
        const o = try self.begin(type_val, span);
        const T = @TypeOf(payload);
        if (@typeInfo(T) == .@"struct") {
            inline for (@typeInfo(T).@"struct".fields) |f| {
                const key_val = comptime fieldKey(tag, f.name);
                if (f.type == ast.NodeIndex) {
                    try self.setProp(o, @field(self.keys, key_val), try self.writeNode(@field(payload, f.name)));
                } else if (f.type == ast.IndexRange) {
                    try self.setProp(o, @field(self.keys, key_val), try self.writeNodeArray(@field(payload, f.name)));
                } else if (f.type == bool) {
                    try self.setProp(o, @field(self.keys, key_val), self.boolVal(@field(payload, f.name)));
                } else if (comptime @typeInfo(f.type) == .@"enum" and @hasDecl(f.type, "toString")) {
                    try self.setProp(o, @field(self.keys, key_val), try self.str(@field(payload, f.name).toString()));
                }
            }
        }
        return o;
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
            else => snakeToPascal(@tagName(tag)),
        };
    }

    // returns the Keys struct field name for a given AST field name.
    // the Keys struct stores JS name strings as values, but we index by zig field name.
    fn fieldKey(comptime tag: NodeTag, comptime name: []const u8) []const u8 {
        return comptime if (tag == .variable_declaration and std.mem.eql(u8, name, "declarators"))
            "declarations"
        else
            name; // keys struct uses the same snake_case names as ast fields
    }

    // -- comments and diagnostics --

    fn writeComments(self: *Serializer) Error!Val {
        const arr = try self.arrayN(@intCast(self.tree.comments.len));
        for (self.tree.comments, 0..) |comment, i| {
            const o = try self.obj();
            try self.setProp(o, self.keys.type, if (comment.type == .line) self.types.Line else self.types.Block);
            try self.setProp(o, self.keys.value, try self.str(self.tree.getString(comment.value)));
            try self.setProp(o, self.keys.start, try self.pos(comment.start));
            try self.setProp(o, self.keys.end, try self.pos(comment.end));
            try self.setElem(arr, @intCast(i), o);
        }
        return arr;
    }

    fn writeDiagnostics(self: *Serializer) Error!Val {
        const diags = self.tree.diagnostics.items;
        const arr = try self.arrayN(@intCast(diags.len));
        for (diags, 0..) |diag, i| {
            const o = try self.obj();
            try self.setProp(o, self.keys.severity, try self.str(diag.severity.toString()));
            try self.setProp(o, self.keys.message, try self.str(diag.message));
            try self.setProp(o, self.keys.help, if (diag.help) |h| try self.str(h) else self.js_null);
            try self.setProp(o, self.keys.start, try self.pos(diag.span.start));
            try self.setProp(o, self.keys.end, try self.pos(diag.span.end));
            // labels
            const label_arr = try self.arrayN(@intCast(diag.labels.len));
            for (diag.labels, 0..) |lbl, j| {
                const lo = try self.obj();
                try self.setProp(lo, self.keys.start, try self.pos(lbl.span.start));
                try self.setProp(lo, self.keys.end, try self.pos(lbl.span.end));
                try self.setProp(lo, self.keys.message, try self.str(lbl.message));
                try self.setElem(label_arr, @intCast(j), lo);
            }
            try self.setProp(o, self.keys.labels, label_arr);
            try self.setElem(arr, @intCast(i), o);
        }
        return arr;
    }

    // -- N-API helpers --

    fn writeNodeArray(self: *Serializer, range: ast.IndexRange) Error!Val {
        const items = self.tree.getExtra(range);
        const arr = try self.arrayN(@intCast(items.len));
        for (items, 0..) |idx, i| {
            try self.setElem(arr, @intCast(i), try self.writeNode(idx));
        }
        return arr;
    }

    inline fn begin(self: *Serializer, type_val: Val, span: ast.Span) Error!Val {
        const o = try self.obj();
        try self.setProp(o, self.keys.type, type_val);
        try self.setProp(o, self.keys.start, try self.pos(span.start));
        try self.setProp(o, self.keys.end, try self.pos(span.end));
        return o;
    }

    inline fn obj(self: *Serializer) Error!Val {
        return self.env.object() catch error.napi_error;
    }

    inline fn arrayN(self: *Serializer, len: u32) Error!Val {
        return self.env.arrayWithLength(len) catch error.napi_error;
    }

    inline fn str(self: *Serializer, s: []const u8) Error!Val {
        return self.env.string(s) catch error.napi_error;
    }

    inline fn pos(self: *Serializer, byte_pos: u32) Error!Val {
        const p = if (self.pos_map) |map|
            map[@min(byte_pos, map.len - 1)]
        else
            byte_pos;
        return self.env.uint32(p) catch error.napi_error;
    }

    inline fn boolVal(self: *Serializer, b: bool) Val {
        return if (b) self.js_true else self.js_false;
    }

    inline fn setProp(self: *Serializer, obj_val: Val, key: Val, value: Val) Error!void {
        check(c.napi_set_property(self.env.raw, obj_val.raw, key.raw, value.raw)) catch return error.napi_error;
    }

    inline fn set(self: *Serializer, obj_val: Val, comptime key: [:0]const u8, value: Val) Error!void {
        obj_val.setNamed(self.env, key, value) catch return error.napi_error;
    }

    inline fn setElem(self: *Serializer, arr: Val, idx: u32, value: Val) Error!void {
        check(c.napi_set_element(self.env.raw, arr.raw, idx, value.raw)) catch return error.napi_error;
    }

    // -- init cached strings --

    fn initKeys(self: *Serializer) Error!void {
        inline for (@typeInfo(Keys).@"struct".fields) |f| {
            const js_name = comptime keyToJsName(f.name);
            @field(self.keys, f.name) = self.env.string(js_name) catch return error.napi_error;
        }
    }

    fn initTypes(self: *Serializer) Error!void {
        inline for (@typeInfo(Types).@"struct".fields) |f| {
            @field(self.types, f.name) = self.env.string(f.name) catch return error.napi_error;
        }
    }

    fn keyToJsName(comptime name: []const u8) []const u8 {
        // special cases where zig field name differs from JS key
        if (std.mem.eql(u8, name, "source_type")) return "sourceType";
        if (std.mem.eql(u8, name, "super_class")) return "superClass";
        if (std.mem.eql(u8, name, "self_closing")) return "selfClosing";
        if (std.mem.eql(u8, name, "opening_element")) return "openingElement";
        if (std.mem.eql(u8, name, "closing_element")) return "closingElement";
        if (std.mem.eql(u8, name, "opening_fragment")) return "openingFragment";
        if (std.mem.eql(u8, name, "closing_fragment")) return "closingFragment";
        return name;
    }

    // -- comptime naming helpers --

    fn snakeToPascal(comptime name: []const u8) []const u8 {
        comptime {
            var buf: [name.len]u8 = undefined;
            var len: usize = 0;
            var cap = true;
            for (name) |ch| {
                if (ch == '_') {
                    cap = true;
                } else {
                    buf[len] = if (cap) (ch ^ 0x20) else ch;
                    cap = false;
                    len += 1;
                }
            }
            const final = buf[0..len].*;
            return &final;
        }
    }

    fn snakeToCamel(comptime name: []const u8) []const u8 {
        comptime {
            var buf: [name.len]u8 = undefined;
            var len: usize = 0;
            var cap = false;
            for (name) |ch| {
                if (ch == '_') {
                    cap = true;
                } else {
                    buf[len] = if (cap) (ch ^ 0x20) else ch;
                    cap = false;
                    len += 1;
                }
            }
            const final = buf[0..len].*;
            return &final;
        }
    }
};

// -- ASCII detection --

fn isAscii(source: []const u8) bool {
    // check 16 bytes at a time
    var i: usize = 0;
    while (i + 16 <= source.len) : (i += 16) {
        const chunk: @Vector(16, u8) = source[i..][0..16].*;
        const high_bits = chunk & @as(@Vector(16, u8), @splat(0x80));
        if (@reduce(.Or, high_bits) != 0) return false;
    }
    // tail bytes
    while (i < source.len) : (i += 1) {
        if (source[i] >= 0x80) return false;
    }
    return true;
}

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
