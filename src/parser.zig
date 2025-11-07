const std = @import("std");
const token = @import("token.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");

pub const Error = struct {
    message: []const u8,
    span: token.Span,
    help: ?[]const u8 = null,
};

pub const ParseResult = struct {
    program: ?ast.Program,
    errors: []Error,

    pub inline fn hasErrors(self: ParseResult) bool {
        return self.errors.len > 0;
    }
};

pub const Parser = struct {
    source: []const u8,
    lexer: lexer.Lexer,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),

    current_token: token.Token,

    scratch_declarators: std.ArrayList(*ast.VariableDeclarator),
    scratch_expressions: std.ArrayList(*ast.Expression),
    scratch_template_elements: std.ArrayList(*ast.TemplateElement),
    scratch_array_pattern_elements: std.ArrayList(?*ast.ArrayPatternElement),
    scratch_object_pattern_properties: std.ArrayList(*ast.ObjectPatternProperty),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        var lex = try lexer.Lexer.init(allocator, source);

        const first_token = lex.nextToken() catch token.Token.eof(0);

        return .{
            .source = source,
            .lexer = lex,
            .allocator = allocator,

            .current_token = first_token,

            .errors = std.ArrayList(Error).empty,

            .scratch_declarators = std.ArrayList(*ast.VariableDeclarator).empty,
            .scratch_expressions = std.ArrayList(*ast.Expression).empty,
            .scratch_template_elements = std.ArrayList(*ast.TemplateElement).empty,
            .scratch_array_pattern_elements = std.ArrayList(?*ast.ArrayPatternElement).empty,
            .scratch_object_pattern_properties = std.ArrayList(*ast.ObjectPatternProperty).empty,
        };
    }

    pub fn parse(self: *Parser) !ParseResult {
        const start = self.current_token.span.start;

        var body_list = std.ArrayList(*ast.Body).empty;

        while (self.current_token.type != .EOF) {
            const stmt = self.parseStatement() orelse {
                self.synchronize();
                continue;
            };

            const body_item = self.createNode(ast.Body, .{ .statement = stmt });
            self.appendItem(&body_list, body_item);
        }

        const end = if (body_list.items.len > 0)
            body_list.items[body_list.items.len - 1].statement.getSpan().end
        else
            start;

        const program = ast.Program{
            .body = self.dupeSlice(*ast.Body, body_list.items),
            .span = .{ .start = start, .end = end },
        };

        return ParseResult{
            .program = program,
            .errors = self.toOwnedSlice(&self.errors),
        };
    }

    fn parseStatement(self: *Parser) ?*ast.Statement {
        return switch (self.current_token.type) {
            .Var, .Const, .Let, .Using => self.parseVariableDeclaration(),
            .Await => {
                const await_token = self.current_token;
                const next = self.peek() orelse {
                    const span_start = await_token.span.start;
                    const span_end = await_token.span.end;
                    self.err(
                        span_start,
                        span_end,
                        "Expected 'using' after 'await'",
                        "Add 'using' after 'await' to create an 'await using' declaration",
                    );
                    return null;
                };

                if (next.type == .Using) {
                    return self.parseVariableDeclaration();
                }

                const span_start = await_token.span.start;
                const span_end = await_token.span.end;
                self.err(
                    span_start,
                    span_end + 1,
                    "Expected 'using' after 'await'",
                    "Add 'using' after 'await' to create an 'await using' declaration",
                );
                return null;
            },
            else => {
                const bad_token = self.current_token;
                self.err(
                    bad_token.span.start,
                    bad_token.span.end,
                    "Unexpected token in statement position",
                    "Expected a statement keyword like 'var', 'let', 'const', 'if', 'for', 'while', 'function', or 'class'",
                );
                return null;
            },
        };
    }

    fn parseVariableDeclaration(self: *Parser) ?*ast.Statement {
        const start = self.current_token.span.start;
        const kind = self.parseVariableDeclarationKind() orelse return null;

        self.clearRetainingCapacity(&self.scratch_declarators);
        self.ensureCapacity(&self.scratch_declarators, 4);

        // parse first declarator
        const first_decl = self.parseVariableDeclarator(kind) orelse return null;
        self.appendItem(&self.scratch_declarators, first_decl);

        // parse additional declarators
        while (self.current_token.type == .Comma) {
            self.advance();
            const decl = self.parseVariableDeclarator(kind) orelse return null;
            self.appendItem(&self.scratch_declarators, decl);
        }

        self.eatSemi();

        const end = if (self.scratch_declarators.items.len > 0)
            self.scratch_declarators.items[self.scratch_declarators.items.len - 1].span.end
        else
            self.current_token.span.start;

        const declarations = self.dupeSlice(*ast.VariableDeclarator, self.scratch_declarators.items);

        const var_decl = ast.VariableDeclaration{
            .kind = kind,
            .declarations = declarations,
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.Statement, .{ .variable_declaration = var_decl });
    }

    inline fn parseVariableDeclarationKind(self: *Parser) ?ast.VariableDeclaration.VariableDeclarationKind {
        const tok = self.current_token.type;
        self.advance();

        return switch (tok) {
            .Let => .let,
            .Const => .@"const",
            .Var => .@"var",
            .Using => .using,
            .Await => blk: {
                if (self.current_token.type == .Using) {
                    self.advance();
                    break :blk .@"await using";
                }
                return null;
            },
            else => null,
        };
    }

    fn parseVariableDeclarator(
        self: *Parser,
        kind: ast.VariableDeclaration.VariableDeclarationKind,
    ) ?*ast.VariableDeclarator {
        const start = self.current_token.span.start;
        const id = self.parseBindingPattern() orelse return null;

        var init_expr: ?*ast.Expression = null;
        var end = id.getSpan().end;

        if (self.current_token.type == .Assign) {
            self.advance();
            if (self.parseExpression()) |expr| {
                init_expr = expr;
                end = expr.getSpan().end;
            }
        } else if (kind == .@"const" or kind == .using or kind == .@"await using") {
            const id_span = id.getSpan();
            self.err(
                id_span.start,
                id_span.end + 1,
                "Missing initializer in declaration",
                "Add '= <value>' after the variable name to initialize it",
            );
            return null;
        }

        return self.createNode(ast.VariableDeclarator, .{
            .id = id,
            .init = init_expr,
            .span = .{ .start = start, .end = end },
        });
    }

    fn parseExpression(self: *Parser) ?*ast.Expression {
        return self.parsePrimaryExpression();
    }

    fn parsePrimaryExpression(self: *Parser) ?*ast.Expression {
        return switch (self.current_token.type) {
            .Identifier => self.parseIdentifierReference(),
            .StringLiteral => self.parseStringLiteral(),
            .True, .False => self.parseBooleanLiteral(),
            .NullLiteral => self.parseNullLiteral(),
            .NumericLiteral, .HexLiteral, .OctalLiteral, .BinaryLiteral => self.parseNumericLiteral(),
            .BigIntLiteral => self.parseBigIntLiteral(),
            .Slash => self.parseRegExpLiteral(),
            .TemplateHead => self.parseTemplateLiteral(),
            .NoSubstitutionTemplate => self.parseNoSubstitutionTemplateLiteral(),
            else => {
                const bad_token = self.current_token;
                self.err(
                    bad_token.span.start,
                    bad_token.span.end,
                    "Unexpected token in expression position",
                    "Expected an expression like a variable name, number, string, or other literal value",
                );
                return null;
            },
        };
    }

    fn parseIdentifierReference(self: *Parser) ?*ast.Expression {
        const name = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const identifier = ast.IdentifierReference{
            .name = name,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .identifier_reference = identifier });
    }

    fn parseStringLiteral(self: *Parser) ?*ast.Expression {
        const value = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const literal = ast.StringLiteral{
            .value = value, // TODO: handle escape sequences and remove quotes
            .raw = value,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .string_literal = literal });
    }

    fn parseBooleanLiteral(self: *Parser) ?*ast.Expression {
        const value = self.current_token.type == .True;
        const raw = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const literal = ast.BooleanLiteral{
            .value = value,
            .raw = raw,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .boolean_literal = literal });
    }

    fn parseNullLiteral(self: *Parser) ?*ast.Expression {
        const raw = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const literal = ast.NullLiteral{
            .value = null,
            .raw = raw,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .null_literal = literal });
    }

    fn parseNumericLiteral(self: *Parser) ?*ast.Expression {
        const value = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const literal = ast.NumericLiteral{
            .value = std.fmt.parseFloat(f64, value) catch unreachable,
            .raw = value,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .numeric_literal = literal });
    }

    fn parseBigIntLiteral(self: *Parser) ?*ast.Expression {
        const raw = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const bigint = raw[0..(raw.len - 1)];

        const literal = ast.BigIntLiteral{
            .value = raw,
            .raw = raw,
            .bigint = bigint,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .bigint_literal = literal });
    }

    fn parseRegExpLiteral(self: *Parser) ?*ast.Expression {
        const slash_token = self.current_token;

        const regex = self.lexer.reScanAsRegex(slash_token) catch |_error| {
            self.err(
                slash_token.span.start,
                slash_token.span.end,
                lexer.getLexicalErrorMessage(_error),
                lexer.getLexicalErrorHelp(_error),
            );
            return null;
        };

        const start = regex.span.start;
        const end = regex.span.end;

        const regex_token = self.lexer.createToken(.RegexLiteral, self.source[start..end], start, end);

        self.replaceTokenAndAdvance(regex_token);

        const literal = ast.RegExpLiteral{
            .value = regex.lexeme,
            .raw = regex.lexeme,
            .regex = .{
                .pattern = regex.pattern,
                .flags = regex.flags,
            },
            .span = regex.span,
        };

        return self.createNode(ast.Expression, .{ .regex_literal = literal });
    }

    fn parseNoSubstitutionTemplateLiteral(self: *Parser) ?*ast.Expression {
        const tok = self.current_token;
        self.advance();

        const element = self.createTemplateElement(tok, true);

        const template_literal = ast.TemplateLiteral{
            .quasis = self.dupeSlice(*ast.TemplateElement, &[_]*ast.TemplateElement{element}),
            .expressions = self.dupeSlice(*ast.Expression, &[_]*ast.Expression{}),
            .span = .{
                .start = tok.span.start - 1, // -1 include starting backtick
                .end = tok.span.end + 1, // +1 include closing backtick
            },
        };

        return self.createNode(ast.Expression, .{ .template_literal = template_literal });
    }

    fn parseTemplateLiteral(self: *Parser) ?*ast.Expression {
        const template_literal_start = self.current_token.span.start - 1; // -1 include starting backtick

        self.clearRetainingCapacity(&self.scratch_template_elements);
        self.clearRetainingCapacity(&self.scratch_expressions);
        self.ensureCapacity(&self.scratch_template_elements, 4);
        self.ensureCapacity(&self.scratch_expressions, 4);

        // parse head element
        const head_token = self.current_token;
        self.appendItem(&self.scratch_template_elements, self.createTemplateElement(head_token, false));
        self.advance();

        var template_literal_end: usize = undefined;

        // parse expressions and middle/tail elements
        while (true) {
            const expr_start = self.current_token.span.start;
            const expr = self.parseExpression() orelse return null;
            self.appendItem(&self.scratch_expressions, expr);

            const template_token = self.current_token;
            const is_tail = template_token.type == .TemplateTail;

            switch (template_token.type) {
                .TemplateMiddle, .TemplateTail => {
                    self.appendItem(&self.scratch_template_elements, self.createTemplateElement(template_token, is_tail));

                    if (is_tail) {
                        template_literal_end = template_token.span.end + 1; // +1 include closing backtick
                        self.advance();
                        break;
                    }

                    self.advance();
                },
                else => {
                    self.err(
                        expr_start,
                        template_token.span.start,
                        "Expected template continuation after expression",
                        "Add '}' here to close the template expression",
                    );
                    return null;
                },
            }
        }

        const template_literal = ast.TemplateLiteral{
            .quasis = self.dupeSlice(*ast.TemplateElement, self.scratch_template_elements.items),
            .expressions = self.dupeSlice(*ast.Expression, self.scratch_expressions.items),
            .span = .{ .start = template_literal_start, .end = template_literal_end },
        };

        return self.createNode(ast.Expression, .{ .template_literal = template_literal });
    }

    inline fn createTemplateElement(self: *Parser, tok: token.Token, is_tail: bool) *ast.TemplateElement {
        const element = ast.TemplateElement{
            .value = .{
                .raw = tok.lexeme,
                .cooked = tok.lexeme, // TODO: process escape sequences
            },
            .tail = is_tail,
            .span = tok.span,
        };
        return self.createNode(ast.TemplateElement, element);
    }

    fn parseBindingPattern(self: *Parser) ?*ast.BindingPattern {
        return switch (self.current_token.type) {
            .Identifier => self.parseBindingIdentifierPattern(),
            .LeftBracket => self.parseArrayPattern(),
            .LeftBrace => self.parseObjectPattern(),
            else => {
                const bad_token = self.current_token;
                self.err(
                    bad_token.span.start,
                    bad_token.span.end,
                    "Expected binding pattern",
                    "Use an identifier, array pattern [...], or object pattern {...}",
                );
                return null;
            },
        };
    }

    fn parseBindingIdentifierPattern(self: *Parser) ?*ast.BindingPattern {
        if (self.current_token.type != .Identifier) {
            const bad_token = self.current_token;
            self.err(
                bad_token.span.start,
                bad_token.span.end,
                "Expected identifier for variable name",
                "Use a valid identifier (letters, digits, _, or $$ - must start with letter, _ or $$)",
            );
            return null;
        }

        const name = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const binding_id = ast.BindingIdentifier{
            .name = name,
            .span = span,
        };

        return self.createNode(ast.BindingPattern, .{ .binding_identifier = binding_id });
    }

    fn parseArrayPattern(self: *Parser) ?*ast.BindingPattern {
        const opening_bracket = self.current_token;
        const start = opening_bracket.span.start;

        if (!self.expect(.LeftBracket, "Expected '[' to start array pattern", "Add '[' here to begin the array destructuring pattern")) {
            return null;
        }

        self.clearRetainingCapacity(&self.scratch_array_pattern_elements);
        self.ensureCapacity(&self.scratch_array_pattern_elements, 4);

        var last_end = self.current_token.span.start;

        // parse array elements
        while (self.current_token.type != .RightBracket and self.current_token.type != .EOF) {
            // check for rest element
            if (self.current_token.type == .Spread) {
                const rest_elem = self.parseRestElement() orelse return null;
                self.appendItem(&self.scratch_array_pattern_elements, rest_elem);
                last_end = rest_elem.getSpan().end;

                // Rest element must be last
                if (self.current_token.type == .Comma) {
                    const comma_token = self.current_token;
                    self.err(
                        rest_elem.getSpan().start,
                        comma_token.span.end,
                        "Rest element must be last in array pattern",
                        "Remove this comma, or move the rest element to the end",
                    );
                    return null;
                }
                break;
            }

            // parse regular element or empty slot
            if (self.current_token.type == .Comma) {
                // empty slot: [a, , b]
                self.appendItem(&self.scratch_array_pattern_elements, null);
                last_end = self.current_token.span.end;
                self.advance();
            } else {
                const elem = self.parseArrayPatternElement() orelse return null;
                self.appendItem(&self.scratch_array_pattern_elements, elem);
                last_end = elem.getSpan().end;

                if (self.current_token.type == .Comma) {
                    last_end = self.current_token.span.end;
                    self.advance();
                } else {
                    break;
                }
            }
        }

        const end = if (self.current_token.type == .RightBracket) blk: {
            const right_bracket_end = self.current_token.span.end;
            self.advance();
            break :blk right_bracket_end;
        } else blk: {
            self.err(
                start,
                last_end + 1,
                "Expected ']' to close array pattern",
                "Add ']' here to close the array destructuring pattern",
            );
            break :blk last_end;
        };

        const array_pattern = ast.ArrayPattern{
            .elements = self.dupeSlice(?*ast.ArrayPatternElement, self.scratch_array_pattern_elements.items),
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.BindingPattern, .{ .array_pattern = array_pattern });
    }

    fn parseArrayPatternElement(self: *Parser) ?*ast.ArrayPatternElement {
        const pattern = self.parseBindingPattern() orelse return null;

        const elem = ast.ArrayPatternElement{ .binding_pattern = pattern };
        return self.createNode(ast.ArrayPatternElement, elem);
    }

    fn parseRestElement(self: *Parser) ?*ast.ArrayPatternElement {
        const spread_token = self.current_token;
        const start = spread_token.span.start;

        if (!self.expect(.Spread, "Expected '...' for rest element", "Add '...' here for the rest element")) {
            return null;
        }

        const argument = self.parseBindingPattern() orelse return null;
        const end = argument.getSpan().end;

        const rest_elem = ast.BindingRestElement{
            .argument = argument,
            .span = .{ .start = start, .end = end },
        };

        const rest_elem_ptr = self.createNode(ast.BindingRestElement, rest_elem);
        const elem = ast.ArrayPatternElement{ .rest_element = rest_elem_ptr };
        return self.createNode(ast.ArrayPatternElement, elem);
    }

    fn parseObjectPattern(self: *Parser) ?*ast.BindingPattern {
        const opening_brace = self.current_token;
        const start = opening_brace.span.start;

        if (!self.expect(.LeftBrace, "Expected '{' to start object pattern", "Add '{' here to begin the object destructuring pattern")) {
            return null;
        }

        self.clearRetainingCapacity(&self.scratch_object_pattern_properties);
        self.ensureCapacity(&self.scratch_object_pattern_properties, 4);

        var last_end = self.current_token.span.start;

        // parse object properties
        while (self.current_token.type != .RightBrace and self.current_token.type != .EOF) {
            // check for rest element
            if (self.current_token.type == .Spread) {
                const rest_prop = self.parseObjectRestElement() orelse return null;
                self.appendItem(&self.scratch_object_pattern_properties, rest_prop);
                last_end = rest_prop.getSpan().end;

                // rest element must be last
                if (self.current_token.type == .Comma) {
                    const comma_token = self.current_token;
                    self.err(
                        rest_prop.getSpan().start,
                        comma_token.span.end,
                        "Rest element must be last in object pattern",
                        "Remove this comma, or move the rest element to the end",
                    );
                    return null;
                }
                break;
            }

            // parse regular property
            const prop = self.parseObjectPatternProperty() orelse return null;
            self.appendItem(&self.scratch_object_pattern_properties, prop);
            last_end = prop.getSpan().end;

            if (self.current_token.type == .Comma) {
                last_end = self.current_token.span.end;
                self.advance();
            } else {
                break;
            }
        }

        const end = if (self.current_token.type == .RightBrace) blk: {
            const right_brace_end = self.current_token.span.end;
            self.advance();
            break :blk right_brace_end;
        } else blk: {
            self.err(
                start,
                last_end,
                "Expected '}' to close object pattern",
                "Add '}' here to close the object destructuring pattern",
            );
            break :blk last_end;
        };

        const object_pattern = ast.ObjectPattern{
            .properties = self.dupeSlice(*ast.ObjectPatternProperty, self.scratch_object_pattern_properties.items),
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.BindingPattern, .{ .object_pattern = object_pattern });
    }

    fn parseObjectPatternProperty(self: *Parser) ?*ast.ObjectPatternProperty {
        const start = self.current_token.span.start;

        var computed = false;
        var key: ast.PropertyKey = undefined;
        var key_span: token.Span = undefined;

        // check for computed property: [expression]
        if (self.current_token.type == .LeftBracket) {
            computed = true;
            const bracket_start = self.current_token.span.start;
            self.advance();

            const key_expr = self.parseExpression() orelse return null;
            key = ast.PropertyKey{ .expression = key_expr };
            key_span = .{ .start = bracket_start, .end = key_expr.getSpan().end };

            if (self.current_token.type != .RightBracket) {
                self.err(
                    bracket_start,
                    self.current_token.span.start,
                    "Expected ']' to close computed property key",
                    "Add ']' here to close the computed property name",
                );
                return null;
            }

            key_span.end = self.current_token.span.end;
            self.advance();
        } else if (self.current_token.type == .Identifier) {
            // identifier key
            const name = self.current_token.lexeme;
            key_span = self.current_token.span;
            self.advance();

            const identifier = ast.IdentifierReference{
                .name = name,
                .span = key_span,
            };

            key = ast.PropertyKey{ .identifier_reference = identifier };
        } else {
            const bad_token = self.current_token;
            self.err(
                bad_token.span.start,
                bad_token.span.end,
                "Expected property key in object pattern",
                "Use an identifier or computed property key [expression]",
            );
            return null;
        }

        // check for shorthand property: { x } is shorthand for { x: x }
        const is_shorthand = self.current_token.type == .Comma or self.current_token.type == .RightBrace;
        var value: *ast.BindingPattern = undefined;

        if (is_shorthand) {
            // shorthand: use the identifier as both key and value
            const identifier_ref = switch (key) {
                .identifier_reference => |id| id,
                .expression => {
                    const key_start = key_span.start;
                    const key_end = key_span.end;
                    self.err(
                        key_start,
                        key_end,
                        "Computed properties cannot use shorthand syntax",
                        "Add ': <pattern>' after the computed key to specify the binding",
                    );
                    return null;
                },
            };

            const binding_id = ast.BindingIdentifier{
                .name = identifier_ref.name,
                .span = key_span,
            };

            value = self.createNode(ast.BindingPattern, .{ .binding_identifier = binding_id });
        } else {
            // regular property: { x: y }
            if (self.current_token.type != .Colon) {
                self.err(
                    key_span.start,
                    self.current_token.span.start,
                    "Expected ':' after property key",
                    "Add ':' here to separate the key from the value pattern",
                );
                return null;
            }
            self.advance();
            value = self.parseBindingPattern() orelse return null;
        }

        const end = value.getSpan().end;
        const prop = ast.BindingProperty{
            .key = key,
            .value = value,
            .shorthand = is_shorthand,
            .computed = computed,
            .span = .{ .start = start, .end = end },
        };

        const prop_ptr = self.createNode(ast.BindingProperty, prop);
        return self.createNode(ast.ObjectPatternProperty, .{ .binding_property = prop_ptr });
    }

    fn parseObjectRestElement(self: *Parser) ?*ast.ObjectPatternProperty {
        const spread_token = self.current_token;
        const start = spread_token.span.start;

        self.advance(); // consume '...'

        const argument = self.parseBindingPattern() orelse return null;
        const end = argument.getSpan().end;

        const rest_elem = ast.BindingRestElement{
            .argument = argument,
            .span = .{ .start = start, .end = end },
        };

        const rest_elem_ptr = self.createNode(ast.BindingRestElement, rest_elem);
        return self.createNode(ast.ObjectPatternProperty, .{ .rest_element = rest_elem_ptr });
    }

    fn peek(self: *Parser) ?token.Token {
        return self.current_token;
    }

    inline fn advance(self: *Parser) void {
        self.current_token = self.lexer.nextToken() catch |_error| blk: {
            self.appendItem(&self.errors, Error{
                .message = lexer.getLexicalErrorMessage(_error),
                .span = .{ .start = self.lexer.token_start, .end = self.lexer.cursor },
                .help = lexer.getLexicalErrorHelp(_error),
            });
            break :blk token.Token.eof(0);
        };
    }

    inline fn replaceTokenAndAdvance(self: *Parser, tok: token.Token) void {
        self.current_token = tok;
        self.advance();
    }

    inline fn expect(
        self: *Parser,
        token_type: token.TokenType,
        message: []const u8,
        help: ?[]const u8,
    ) bool {
        if (self.current_token.type == token_type) {
            self.advance();
            return true;
        }

        const tok = self.current_token;
        self.err(tok.span.start, tok.span.end, message, help);
        return false;
    }

    inline fn eatSemi(self: *Parser) void {
        if (self.current_token.type == .Semicolon) {
            self.advance();
        }
    }

    inline fn err(
        self: *Parser,
        start: usize,
        end: usize,
        message: []const u8,
        help: ?[]const u8,
    ) void {
        self.appendItem(&self.errors, Error{
            .message = message,
            .span = .{ .start = start, .end = end },
            .help = help,
        });
    }

    fn synchronize(self: *Parser) void {
        while (self.current_token.type != .EOF) {
            if (self.current_token.type == .Semicolon) {
                self.advance();
                return;
            }

            switch (self.current_token.type) {
                .Class,
                .Function,
                .Var,
                .For,
                .If,
                .While,
                .Return,
                .Let,
                .Const,
                .Using,
                => return,
                else => {},
            }

            self.advance();
        }
    }

    inline fn createNode(self: *Parser, comptime T: type, value: T) *T {
        const ptr = self.allocator.create(T) catch unreachable;
        ptr.* = value;
        return ptr;
    }

    inline fn ensureCapacity(self: *Parser, list: anytype, capacity: usize) void {
        list.ensureTotalCapacity(self.allocator, capacity) catch unreachable;
    }

    inline fn appendItem(self: *Parser, list: anytype, item: anytype) void {
        list.append(self.allocator, item) catch unreachable;
    }

    inline fn clearRetainingCapacity(self: *Parser, list: anytype) void {
        _ = self;
        list.clearRetainingCapacity();
    }

    inline fn dupeSlice(self: *Parser, comptime T: type, items: []const T) []T {
        return self.allocator.dupe(T, items) catch unreachable;
    }

    inline fn toOwnedSlice(self: *Parser, list: anytype) @TypeOf(list.*.toOwnedSlice(self.allocator) catch unreachable) {
        return list.toOwnedSlice(self.allocator) catch unreachable;
    }
};
