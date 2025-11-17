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

    in_async: bool = false,
    in_generator: bool = false,
    in_function: bool = false,
    allow_in: bool = true,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Parser {
        var lex = try lexer.Lexer.init(allocator, source);

        const first_token = lex.nextToken() catch token.Token.eof(0);

        return .{
            .source = source,
            .lexer = lex,
            .allocator = allocator,

            .current_token = first_token,

            .errors = std.ArrayList(Error).empty,

            .scratch_declarators = .empty,
            .scratch_expressions = .empty,
            .scratch_template_elements = .empty,
            .scratch_array_pattern_elements = .empty,
            .scratch_object_pattern_properties = .empty,
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
            self.append(&body_list, body_item);
        }

        const end = self.current_token.span.start;

        const program = ast.Program{
            .body = self.dupe(*ast.Body, body_list.items),
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
                const next = self.lookAhead() orelse return null;

                if (next.type == .Using) {
                    return self.parseVariableDeclaration();
                }

                const span_start = await_token.span.start;
                const span_end = await_token.span.end;
                self.err(
                    span_start,
                    span_end,
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

        self.clear(&self.scratch_declarators);
        self.ensureCapacity(&self.scratch_declarators, 4);

        // parse first declarator
        const first_decl = self.parseVariableDeclarator(kind) orelse return null;
        self.append(&self.scratch_declarators, first_decl);

        var end = first_decl.span.end;

        // parse additional declarators
        while (self.current_token.type == .Comma) {
            self.advance();
            const decl = self.parseVariableDeclarator(kind) orelse return null;
            end = decl.span.end;
            self.append(&self.scratch_declarators, decl);
        }

        if (self.eatSemi()) {
            end += 1;
        }

        const declarations = self.dupe(*ast.VariableDeclarator, self.scratch_declarators.items);

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
            if (self.parseExpression(0)) |expr| {
                init_expr = expr;
                end = expr.getSpan().end;
            }
        } else if (self.isDestructuringPattern(id)) {
            const id_span = id.getSpan();
            self.err(
                id_span.start,
                id_span.end,
                "Missing initializer in destructuring declaration",
                "Destructuring patterns must be initialized. Add '= <value>' after the pattern",
            );
            return null;
        } else if (kind == .@"const" or kind == .using or kind == .@"await using") {
            const id_span = id.getSpan();

            const kind_str = switch (kind) {
                .@"const" => "'const'",
                .using => "'using'",
                .@"await using" => "'await using'",
                else => unreachable,
            };

            self.err(
                id_span.start,
                id_span.end,
                self.formatMessage(
                    "Missing initializer in {s} declaration",
                    .{kind_str},
                ),
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

    fn parseExpression(self: *Parser, prec: u5) ?*ast.Expression {
        var left: *ast.Expression = self.parseExpressionPrefix() orelse return null;

        while (self.current_token.type != .EOF) {
            const current_prec = self.current_token.type.precedence();

            if (prec > current_prec or current_prec == 0) break;

            left = self.parseExpressionInfix(current_prec, left) orelse return null;
        }

        return left;
    }

    fn parseExpressionInfix(self: *Parser, prec: u5, left: *ast.Expression) ?*ast.Expression {
        const current_token = self.current_token;

        // (x++, x--)
        if (current_token.type == .Increment or current_token.type == .Decrement) {
            return self.parseUpdateExpression(false, left);
        }

        if (current_token.type.isBinaryOperator()) {
            return self.parseBinaryExpression(prec, left);
        }

        if (current_token.type.isLogicalOperator()) {
            return self.parseLogicalExpression(prec, left);
        }

        unreachable;
    }

    fn parseExpressionPrefix(self: *Parser) ?*ast.Expression {
        // (++x, --x)
        if (self.current_token.type == .Increment or self.current_token.type == .Decrement) {
            return self.parseUpdateExpression(true, undefined);
        }

        if (self.current_token.type.isUnaryOperator()) {
            return self.parseUnaryExpression();
        }

        return self.parsePrimaryExpression();
    }

    fn parsePrimaryExpression(self: *Parser) ?*ast.Expression {
        return switch (self.current_token.type) {
            .Identifier => self.parseIdentifierReference(),
            .PrivateIdentifier => self.parsePrivateIdentifier(),
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
                    bad_token.span.start - 1,
                    bad_token.span.end,
                    "Unexpected token in expression position",
                    "Expected an expression like a variable name, number, string, or other literal value",
                );
                return null;
            },
        };
    }

    fn parseUnaryExpression(self: *Parser) ?*ast.Expression {
        const operator_token = self.current_token;
        const operator = ast.UnaryOperator.fromToken(operator_token.type);
        const start = operator_token.span.start;

        self.advance();

        const argument = self.parseExpression(14) orelse return null;

        const unary_expression = ast.UnaryExpression{
            .span = .{
                .start = start,
                .end = argument.getSpan().end,
            },
            .operator = operator,
            .argument = argument,
        };

        return self.createNode(ast.Expression, .{ .unary_expression = unary_expression });
    }

    // TODO: ensure no line terminator after lhs in postifx
    fn parseUpdateExpression(self: *Parser, is_prefix: bool, left: ?*ast.Expression) ?*ast.Expression {
        const operator_token = self.current_token;
        const operator = ast.UpdateOperator.fromToken(operator_token.type);

        const start = if (is_prefix) operator_token.span.start else left.?.getSpan().start;

        self.advance();

        var argument: *ast.Expression = undefined;
        var end: u32 = undefined;

        if (is_prefix) {
            // ++x, --x
            argument = self.parseExpression(14) orelse return null;
            const arg_span = argument.getSpan();
            end = arg_span.end;

            if (!self.isValidAssignmentTarget(argument)) {
                self.err(
                    arg_span.start,
                    arg_span.end,
                    "Invalid left-hand side expression in prefix operation",
                    "Prefix increment/decrement requires a variable or property, not an expression result",
                );
                return null;
            }
        } else {
            // x++, x--
            argument = left orelse unreachable;

            end = operator_token.span.end;

            if (!self.isValidAssignmentTarget(argument)) {
                const arg_span = argument.getSpan();
                self.err(
                    arg_span.start,
                    arg_span.end,
                    "Invalid left-hand side expression in postfix operation",
                    "Postfix increment/decrement requires a variable or property, not an expression result",
                );
                return null;
            }
        }

        const update_expression = ast.UpdateExpression{
            .span = .{
                .start = start,
                .end = end,
            },
            .operator = operator,
            .prefix = is_prefix,
            .argument = argument,
        };

        return self.createNode(ast.Expression, .{ .update_expression = update_expression });
    }

    fn parseBinaryExpression(self: *Parser, prec: u5, left: *ast.Expression) ?*ast.Expression {
        const operator_token = self.current_token;
        const operator = ast.BinaryOperator.fromToken(operator_token.type);

        self.advance();

        // ** is right assosiative
        const next_prec = if (operator == .Exponent) prec else prec + 1;

        const right = self.parseExpression(next_prec) orelse return null;

        const binary_expression = ast.BinaryExpression{
            .span = .{
                .start = left.getSpan().start,
                .end = right.getSpan().end,
            },
            .operator = operator,
            .left = left,
            .right = right,
        };

        return self.createNode(ast.Expression, .{ .binary_expression = binary_expression });
    }

    fn parseLogicalExpression(self: *Parser, prec: u5, left: *ast.Expression) ?*ast.Expression {
        const operator_token = self.current_token;

        const operator = ast.LogicalOperator.fromToken(operator_token.type);

        self.advance();

        const right = self.parseExpression(prec + 1) orelse return null;

        const logical_expression = ast.LogicalExpression{
            .span = .{
                .start = left.getSpan().start,
                .end = right.getSpan().end,
            },
            .operator = operator,
            .left = left,
            .right = right,
        };

        return self.createNode(ast.Expression, .{ .logical_expression = logical_expression });
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

    fn parsePrivateIdentifier(self: *Parser) ?*ast.Expression {
        const name = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const private_id = ast.PrivateIdentifier{
            .name = name,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .private_identifier = private_id });
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
            .value = std.fmt.parseFloat(f64, value) catch unreachable, // safety: lexer only tokenizes valid numeric literals
            .raw = value,
            .span = span,
        };

        return self.createNode(ast.Expression, .{ .numeric_literal = literal });
    }

    fn parseBigIntLiteral(self: *Parser) ?*ast.Expression {
        const raw = self.current_token.lexeme;
        const span = self.current_token.span;
        self.advance();

        const bigint = raw[0..(raw.len - 1)]; // lexer only produces BigInt tokens for valid literals

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

        const regex = self.lexer.reScanAsRegex(slash_token) catch |err_| {
            self.err(
                slash_token.span.start,
                slash_token.span.end,
                lexer.getLexicalErrorMessage(err_),
                lexer.getLexicalErrorHelp(err_),
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
            .quasis = self.dupe(*ast.TemplateElement, &[_]*ast.TemplateElement{element}),
            .expressions = self.dupe(*ast.Expression, &[_]*ast.Expression{}),
            .span = .{
                .start = tok.span.start - 1, // -1 include starting backtick
                .end = tok.span.end + 1, // +1 include closing backtick
            },
        };

        return self.createNode(ast.Expression, .{ .template_literal = template_literal });
    }

    fn parseTemplateLiteral(self: *Parser) ?*ast.Expression {
        const template_literal_start = self.current_token.span.start - 1; // -1 include starting backtick

        self.clear(&self.scratch_template_elements);
        self.clear(&self.scratch_expressions);
        self.ensureCapacity(&self.scratch_template_elements, 4);
        self.ensureCapacity(&self.scratch_expressions, 4);

        // parse head element
        const head_token = self.current_token;
        self.append(&self.scratch_template_elements, self.createTemplateElement(head_token, false));
        self.advance();

        var template_literal_end: u32 = undefined;

        // parse expressions and middle/tail elements
        while (true) {
            const expr_start = self.current_token.span.start;
            const expr = self.parseExpression(0) orelse return null;
            self.append(&self.scratch_expressions, expr);

            const template_token = self.current_token;
            const is_tail = template_token.type == .TemplateTail;

            switch (template_token.type) {
                .TemplateMiddle, .TemplateTail => {
                    self.append(&self.scratch_template_elements, self.createTemplateElement(template_token, is_tail));

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
            .quasis = self.dupe(*ast.TemplateElement, self.scratch_template_elements.items),
            .expressions = self.dupe(*ast.Expression, self.scratch_expressions.items),
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

        self.clear(&self.scratch_array_pattern_elements);
        self.ensureCapacity(&self.scratch_array_pattern_elements, 4);

        var last_end = self.current_token.span.start;

        // parse array elements
        while (self.current_token.type != .RightBracket and self.current_token.type != .EOF) {
            // check for rest element
            if (self.current_token.type == .Spread) {
                const rest_elem = self.parseRestElement() orelse return null;
                self.append(&self.scratch_array_pattern_elements, rest_elem);
                last_end = rest_elem.getSpan().end;

                // rest element must be last
                if (self.current_token.type == .Comma) {
                    const comma_token = self.current_token;
                    self.err(
                        rest_elem.getSpan().start,
                        comma_token.span.end,
                        "Rest element must be last in array pattern",
                        "Move the rest element to the end. Example: { a, b, ...rest } instead of { ...rest, a, b }",
                    );
                    return null;
                }
                break;
            }

            // parse regular element or empty slot
            if (self.current_token.type == .Comma) {
                // empty slot: [a, , b]
                self.append(&self.scratch_array_pattern_elements, null);
                last_end = self.current_token.span.end;
                self.advance();
            } else {
                const elem = self.parseArrayPatternElement() orelse return null;
                self.append(&self.scratch_array_pattern_elements, elem);
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
                last_end,
                "Expected ']' to close array pattern",
                "Add ']' to close the array destructuring pattern",
            );
            break :blk last_end;
        };

        const array_pattern = ast.ArrayPattern{
            .elements = self.dupe(?*ast.ArrayPatternElement, self.scratch_array_pattern_elements.items),
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.BindingPattern, .{ .array_pattern = array_pattern });
    }

    fn parseArrayPatternElement(self: *Parser) ?*ast.ArrayPatternElement {
        const pattern = self.parseBindingPattern() orelse return null;

        // check for default value
        const final_pattern = if (self.current_token.type == .Assign)
            self.parseAssignmentPatternDefault(pattern) orelse return null
        else
            pattern;

        const elem = ast.ArrayPatternElement{ .binding_pattern = final_pattern };
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

        self.clear(&self.scratch_object_pattern_properties);
        self.ensureCapacity(&self.scratch_object_pattern_properties, 4);

        var last_end = self.current_token.span.start;

        // parse object properties
        while (self.current_token.type != .RightBrace and self.current_token.type != .EOF) {
            // check for rest element
            if (self.current_token.type == .Spread) {
                const rest_prop = self.parseObjectRestElement() orelse return null;
                self.append(&self.scratch_object_pattern_properties, rest_prop);
                last_end = rest_prop.getSpan().end;

                // rest element must be last
                if (self.current_token.type == .Comma) {
                    const comma_token = self.current_token;
                    self.err(
                        rest_prop.getSpan().start,
                        comma_token.span.end,
                        "Rest element must be last in object pattern",
                        "Move the rest element to the end. Example: { a, b, ...rest } instead of { ...rest, a, b }",
                    );
                    return null;
                }
                break;
            }

            // parse regular property
            const prop = self.parseObjectPatternProperty() orelse return null;
            self.append(&self.scratch_object_pattern_properties, prop);
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
            .properties = self.dupe(*ast.ObjectPatternProperty, self.scratch_object_pattern_properties.items),
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.BindingPattern, .{ .object_pattern = object_pattern });
    }

    fn parseObjectPatternProperty(self: *Parser) ?*ast.ObjectPatternProperty {
        const start = self.current_token.span.start;

        var computed = false;
        var key: *ast.PropertyKey = undefined;
        var key_span: token.Span = undefined;

        // check for computed property: [expression]
        if (self.current_token.type == .LeftBracket) {
            computed = true;
            const bracket_start = self.current_token.span.start;
            self.advance();

            const key_expr = self.parseExpression(0) orelse return null;
            key = self.createNode(ast.PropertyKey, .{ .expression = key_expr });
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
            const span = self.current_token.span;
            const name = self.current_token.lexeme;

            self.advance();

            const identifier_name = ast.IdentifierName{
                .name = name,
                .span = span,
            };

            key_span = span;

            key = self.createNode(ast.PropertyKey, .{ .identifier_name = identifier_name });
        } else if (self.current_token.type.isNumericLiteral()) {
            const numeric_literal = self.parseNumericLiteral() orelse return null;

            key_span = numeric_literal.getSpan();

            key = self.createNode(ast.PropertyKey, .{ .expression = numeric_literal });
        } else if (self.current_token.type == .StringLiteral) {
            const string_literal = self.parseStringLiteral() orelse return null;

            key_span = string_literal.getSpan();

            key = self.createNode(ast.PropertyKey, .{ .expression = string_literal });
        } else {
            self.err(
                self.current_token.span.start,
                self.current_token.span.end,
                "Expected property key",
                "Property key must be an identifier, string, number, or computed property ([expression])",
            );
            return null;
        }

        const is_shorthand = self.current_token.type == .Comma or self.current_token.type == .RightBrace or self.current_token.type == .Assign;
        var value: *ast.BindingPattern = undefined;

        if (is_shorthand) {
            // shorthand: only allowed with identifier_name keys
            const identifier_name = switch (key.*) {
                .identifier_name => |id| id,
                .expression => {
                    const key_start = key_span.start;
                    const key_end = key_span.end;
                    self.err(
                        key_start,
                        key_end,
                        "Cannot use computed property as shorthand property",
                        "Computed properties require explicit binding. Use ': <pattern>' after the key",
                    );
                    return null;
                },
                else => return null,
            };

            const binding_id = ast.BindingIdentifier{
                .name = identifier_name.name,
                .span = key_span,
            };

            value = self.createNode(ast.BindingPattern, .{ .binding_identifier = binding_id });

            // check for default value in shorthand: { x = 5 }
            if (self.current_token.type == .Assign) {
                value = self.parseAssignmentPatternDefault(value) orelse return null;
            }
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

            // check for default value: { x: y = 5 }
            if (self.current_token.type == .Assign) {
                value = self.parseAssignmentPatternDefault(value) orelse return null;
            }
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

    fn parseAssignmentPatternDefault(self: *Parser, left: *ast.BindingPattern) ?*ast.BindingPattern {
        const start = left.getSpan().start;

        // consume '='
        if (self.current_token.type != .Assign) {
            return left;
        }

        self.advance();

        // parse the default expression
        const right = self.parseExpression(0) orelse return null;

        const end = right.getSpan().end;

        const assignment_pattern = ast.AssignmentPattern{
            .left = left,
            .right = right,
            .span = .{ .start = start, .end = end },
        };

        return self.createNode(ast.BindingPattern, .{ .assignment_pattern = assignment_pattern });
    }

    // validators

    inline fn isValidAssignmentTarget(self: *Parser, expr: *ast.Expression) bool {
        _ = self;
        return switch (expr.*) {
            .identifier_reference => true,
            // TODO: uncomment when add member_expression
            // .member_expression => true,

            else => false,
        };
    }

    fn isDestructuringPattern(self: *Parser, pattern: *ast.BindingPattern) bool {
        return switch (pattern.*) {
            .array_pattern, .object_pattern => true,
            .assignment_pattern => |ap| self.isDestructuringPattern(ap.left),
            else => false,
        };
    }

    // utils

    fn formatMessage(self: *Parser, comptime fmt: []const u8, args: anytype) []u8 {
        return std.fmt.allocPrint(self.allocator, fmt, args) catch unreachable;
    }

    fn lookAhead(self: *Parser) ?token.Token {
        // TODO: add lookahead support, rewind etc.
        return self.current_token;
    }

    inline fn advance(self: *Parser) void {
        self.current_token = self.lexer.nextToken() catch |err_| blk: {
            self.append(&self.errors, Error{
                .message = lexer.getLexicalErrorMessage(err_),
                .span = .{ .start = self.lexer.token_start, .end = self.lexer.cursor },
                .help = lexer.getLexicalErrorHelp(err_),
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

    inline fn eatSemi(self: *Parser) bool {
        if (self.current_token.type == .Semicolon) {
            self.advance();
            return true;
        }

        return false;
    }

    inline fn err(
        self: *Parser,
        start: u32,
        end: u32,
        message: []const u8,
        help: ?[]const u8,
    ) void {
        self.append(&self.errors, Error{
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

    // memory helpers

    inline fn createNode(self: *Parser, comptime T: type, value: T) *T {
        const ptr = self.allocator.create(T) catch unreachable;
        ptr.* = value;
        return ptr;
    }

    inline fn ensureCapacity(self: *Parser, list: anytype, capacity: u32) void {
        list.ensureUnusedCapacity(self.allocator, capacity) catch unreachable;
    }

    inline fn append(self: *Parser, list: anytype, item: anytype) void {
        list.append(self.allocator, item) catch unreachable;
    }

    inline fn clear(self: *Parser, list: anytype) void {
        _ = self;
        list.items.len = 0;
    }

    inline fn dupe(self: *Parser, comptime T: type, items: []const T) []T {
        return self.allocator.dupe(T, items) catch unreachable;
    }

    inline fn toOwnedSlice(self: *Parser, list: anytype) @TypeOf(list.*.toOwnedSlice(self.allocator) catch unreachable) {
        return list.toOwnedSlice(self.allocator) catch unreachable;
    }
};
