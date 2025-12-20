const std = @import("std");
const token = @import("token.zig");
const ast = @import("ast.zig");
const util = @import("util");
const parser = @import("parser.zig");

pub const LexicalError = error{
    UnterminatedString,
    UnterminatedRegex,
    NonTerminatedTemplateLiteral,
    UnterminatedRegexLiteral,
    InvalidRegexLineTerminator,
    InvalidRegex,
    InvalidIdentifierStart,
    InvalidIdentifierContinue,
    UnterminatedMultiLineComment,
    InvalidUnicodeEscape,
    InvalidHexEscape,
    InvalidOctalEscape,
    OctalEscapeInStrict,
    OctalLiteralInStrict,
    InvalidBinaryLiteral,
    InvalidOctalLiteralDigit,
    InvalidHexLiteral,
    InvalidExponentPart,
    NumericSeparatorMisuse,
    ConsecutiveNumericSeparators,
    MultipleDecimalPoints,
    InvalidBigIntSuffix,
    IdentifierAfterNumericLiteral,
    InvalidUtf8,
    OutOfMemory,
};

// TODO:
// [ ] some simd optimizations

pub const Lexer = struct {
    strict_mode: bool,
    source: []const u8,
    source_len: u32,
    /// token start position, retained for lexical error recovery if scan fails
    token_start: u32,
    /// current byte index being scanned in the source
    cursor: u32,

    template_depth: u32,
    /// stack of brace depths for each template nesting level.
    /// each entry tracks nested braces within that template's ${} expression.
    brace_depth_stack: [16]u32,

    has_line_terminator_before: bool,
    comments: std.ArrayList(ast.Comment),
    allocator: std.mem.Allocator,
    source_type: parser.SourceType,

    pub fn init(source: []const u8, allocator: std.mem.Allocator, source_type: parser.SourceType, strict_mode: bool) error{OutOfMemory}!Lexer {
        return .{
            .strict_mode = strict_mode,
            .source = source,
            .source_len = @intCast(source.len),
            .token_start = 0,
            .cursor = 0,
            .template_depth = 0,
            .brace_depth_stack = .{0} ** 16,
            .has_line_terminator_before = false,
            .comments = try .initCapacity(allocator, source.len / 3),
            .allocator = allocator,
            .source_type = source_type,
        };
    }

    inline fn peek(self: *const Lexer, offset: u32) u8 {
        if (offset > self.source_len or self.cursor >= self.source_len - offset) {
            return 0;
        }
        return self.source[self.cursor + offset];
    }

    pub fn nextToken(self: *Lexer) LexicalError!token.Token {
        try self.skipSkippable();

        if (self.cursor >= self.source_len) {
            return self.createToken(.eof, "", self.cursor, self.cursor);
        }

        self.token_start = self.cursor;
        const current_char = self.source[self.cursor];

        return switch (current_char) {
            '+', '*', '-', '!', '<', '>', '=', '|', '&', '^', '%', '/', '?' => self.scanPunctuation(),
            '.' => self.scanDot(),
            '0'...'9' => try self.scanNumber(),
            '"', '\'' => self.scanString(),
            '`' => self.scanTemplateLiteral(),
            '~', '(', ')', '{', '[', ']', ';', ',', ':' => self.scanSimplePunctuation(),
            '}' => self.handleRightBrace(),
            else => try self.scanIdentifierOrKeyword(),
        };
    }

    inline fn scanSimplePunctuation(self: *Lexer) token.Token {
        const start = self.cursor;
        const c = self.source[self.cursor];
        self.cursor += 1;

        const token_type: token.TokenType = switch (c) {
            '~' => .bitwise_not,
            '(' => .left_paren,
            ')' => .right_paren,
            '{' => blk: {
                // track nested braces inside template expressions
                if (self.template_depth > 0) {
                    self.brace_depth_stack[self.template_depth - 1] += 1;
                }
                break :blk .left_brace;
            },
            '[' => .left_bracket,
            ']' => .right_bracket,
            ';' => .semicolon,
            ',' => .comma,
            ':' => .colon,
            else => unreachable,
        };

        return self.createToken(token_type, self.source[start..self.cursor], start, self.cursor);
    }

    inline fn makePunctuationToken(self: *Lexer, len: u32, token_type: token.TokenType, start: u32) token.Token {
        self.cursor += len;
        return self.createToken(token_type, self.source[start..self.cursor], start, self.cursor);
    }

    fn scanPunctuation(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        const c0 = self.source[self.cursor];
        const c1 = self.peek(1);
        const c2 = self.peek(2);
        const c3 = self.peek(3);

        return switch (c0) {
            '+' => switch (c1) {
                '+' => self.makePunctuationToken(2, .increment, start),
                '=' => self.makePunctuationToken(2, .plus_assign, start),
                else => self.makePunctuationToken(1, .plus, start),
            },
            '-' => switch (c1) {
                '-' => self.makePunctuationToken(2, .decrement, start),
                '=' => self.makePunctuationToken(2, .minus_assign, start),
                else => self.makePunctuationToken(1, .minus, start),
            },
            '*' => if (c1 == '*' and c2 == '=')
                self.makePunctuationToken(3, .exponent_assign, start)
            else switch (c1) {
                '*' => self.makePunctuationToken(2, .exponent, start),
                '=' => self.makePunctuationToken(2, .star_assign, start),
                else => self.makePunctuationToken(1, .star, start),
            },
            '/' => if (c1 == '=') self.makePunctuationToken(2, .slash_assign, start) else self.makePunctuationToken(1, .slash, start),
            '%' => switch (c1) {
                '=' => self.makePunctuationToken(2, .percent_assign, start),
                else => self.makePunctuationToken(1, .percent, start),
            },
            '<' => if (c1 == '<' and c2 == '=')
                self.makePunctuationToken(3, .left_shift_assign, start)
            else switch (c1) {
                '<' => self.makePunctuationToken(2, .left_shift, start),
                '=' => self.makePunctuationToken(2, .less_than_equal, start),
                else => self.makePunctuationToken(1, .less_than, start),
            },
            '>' => if (c1 == '>' and c2 == '=')
                self.makePunctuationToken(3, .right_shift_assign, start)
            else if (c1 == '>' and c2 == '>')
                if (c3 == '=') self.makePunctuationToken(4, .unsigned_right_shift_assign, start) else self.makePunctuationToken(3, .unsigned_right_shift, start)
            else switch (c1) {
                '>' => self.makePunctuationToken(2, .right_shift, start),
                '=' => self.makePunctuationToken(2, .greater_than_equal, start),
                else => self.makePunctuationToken(1, .greater_than, start),
            },
            '=' => if (c1 == '=' and c2 == '=')
                self.makePunctuationToken(3, .strict_equal, start)
            else switch (c1) {
                '=' => self.makePunctuationToken(2, .equal, start),
                '>' => self.makePunctuationToken(2, .arrow, start),
                else => self.makePunctuationToken(1, .assign, start),
            },
            '!' => if (c1 == '=' and c2 == '=')
                self.makePunctuationToken(3, .strict_not_equal, start)
            else switch (c1) {
                '=' => self.makePunctuationToken(2, .not_equal, start),
                else => self.makePunctuationToken(1, .logical_not, start),
            },
            '&' => if (c1 == '&' and c2 == '=')
                self.makePunctuationToken(3, .logical_and_assign, start)
            else switch (c1) {
                '&' => self.makePunctuationToken(2, .logical_and, start),
                '=' => self.makePunctuationToken(2, .bitwise_and_assign, start),
                else => self.makePunctuationToken(1, .bitwise_and, start),
            },
            '|' => if (c1 == '|' and c2 == '=')
                self.makePunctuationToken(3, .logical_or_assign, start)
            else switch (c1) {
                '|' => self.makePunctuationToken(2, .logical_or, start),
                '=' => self.makePunctuationToken(2, .bitwise_or_assign, start),
                else => self.makePunctuationToken(1, .bitwise_or, start),
            },
            '^' => switch (c1) {
                '=' => self.makePunctuationToken(2, .bitwise_xor_assign, start),
                else => self.makePunctuationToken(1, .bitwise_xor, start),
            },
            '?' => if (c1 == '?' and c2 == '=')
                self.makePunctuationToken(3, .nullish_assign, start)
            else switch (c1) {
                '?' => self.makePunctuationToken(2, .nullish_coalescing, start),
                '.' => if (std.ascii.isDigit(c2))
                    self.makePunctuationToken(1, .question, start)
                else
                    self.makePunctuationToken(2, .optional_chaining, start),
                else => self.makePunctuationToken(1, .question, start),
            },
            else => unreachable,
        };
    }

    fn scanString(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        const quote = self.source[start];
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }

            if (c == quote) {
                self.cursor += 1;
                return self.createToken(.string_literal, self.source[start..self.cursor], start, self.cursor);
            }

            if (c == '\n' or c == '\r') {
                return error.UnterminatedString;
            }

            self.cursor += 1;
        }

        return error.UnterminatedString;
    }

    fn scanTemplateLiteral(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }

            if (c == '`') {
                self.cursor += 1;
                const end = self.cursor;
                return self.createToken(.no_substitution_template, self.source[start..end], start, end);
            }

            if (c == '$' and self.peek(1) == '{') {
                self.cursor += 2;
                const end = self.cursor;
                // initialize brace depth for this template level before incrementing depth
                self.brace_depth_stack[self.template_depth] = 0;
                self.template_depth += 1;
                return self.createToken(.template_head, self.source[start..end], start, end);
            }

            self.cursor += 1;
        }

        return error.NonTerminatedTemplateLiteral;
    }

    fn scanTemplateMiddleOrTail(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        self.cursor += 1;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (c == '\\') {
                try self.consumeEscape();
                continue;
            }
            if (c == '`') {
                self.cursor += 1;
                const end = self.cursor;
                if (self.template_depth > 0) {
                    self.template_depth -= 1;
                }
                return self.createToken(.template_tail, self.source[start..end], start, end);
            }
            if (c == '$' and self.peek(1) == '{') {
                self.cursor += 2;
                const end = self.cursor;
                return self.createToken(.template_middle, self.source[start..end], start, end);
            }
            self.cursor += 1;
        }
        return error.NonTerminatedTemplateLiteral;
    }

    fn consumeEscape(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip backslash

        if (self.cursor >= self.source_len) {
            return error.UnterminatedString;
        }

        const c = self.source[self.cursor];

        // LineTerminatorSequence :: <LF> | <CR> [lookahead != <LF>] | <LS> | <PS> | <CR><LF>
        if (c == '\r') {
            self.cursor += 1;
            // consume optional \n after \r
            if (self.cursor < self.source_len and self.source[self.cursor] == '\n') {
                self.cursor += 1;
            }
            return;
        }
        if (c == '\n') {
            self.cursor += 1;
            return;
        }

        // multi-byte line terminators (LS U+2028, PS U+2029)
        const lt_len = util.Utf.lineTerminatorLen(self.source, self.cursor);
        if (lt_len > 0) {
            self.cursor += lt_len;
            return;
        }

        brk: switch (c) {
            '0' => {
                const c1 = self.peek(1);

                if (!util.Utf.isOctalDigit(c1)) {
                    self.cursor += 1; // null escape
                    break :brk;
                }

                if (self.strict_mode) return error.OctalEscapeInStrict;
                try self.consumeOctal();
            },
            'x' => {
                try self.consumeHex();
            },
            'u' => {
                try self.consumeUnicodeEscape();
            },
            '1'...'7' => {
                if (self.strict_mode) return error.OctalEscapeInStrict;
                try self.consumeOctal();
            },
            else => {
                self.cursor += 1;
            },
        }
    }

    fn consumeOctal(self: *Lexer) LexicalError!void {
        const result = util.Utf.parseOctal(self.source, self.cursor);
        if (result.end == self.cursor) {
            return error.InvalidOctalEscape;
        }
        self.cursor = @intCast(result.end);
    }

    fn consumeHex(self: *Lexer) LexicalError!void {
        if (util.Utf.parseHex2(self.source, self.cursor + 1)) |r| {
            self.cursor = @intCast(r.end);
        } else {
            return error.InvalidHexEscape;
        }
    }

    fn consumeUnicodeEscape(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip 'u'

        if (self.cursor < self.source_len and self.source[self.cursor] == '{') {
            // \u{XXXXX}
            self.cursor += 1;
            const start = self.cursor;
            const end = std.mem.indexOfScalarPos(u8, self.source, self.cursor, '}') orelse
                return error.InvalidUnicodeEscape;

            if (util.Utf.parseHexVariable(self.source, start, end - start)) |r| {
                if (r.has_digits and r.end == end) {
                    self.cursor = @intCast(end + 1); // skip past '}'
                } else {
                    return error.InvalidUnicodeEscape;
                }
            } else {
                return error.InvalidUnicodeEscape;
            }
        } else {
            // \uXXXX format
            if (util.Utf.parseHex4(self.source, self.cursor)) |r| {
                self.cursor = @intCast(r.end);
            } else {
                return error.InvalidUnicodeEscape;
            }
        }
    }

    fn handleRightBrace(self: *Lexer) LexicalError!token.Token {
        // inside a template expression, check if this } closes a nested brace
        // or the template substitution itself
        if (self.template_depth > 0) {
            const depth_idx = self.template_depth - 1;
            if (self.brace_depth_stack[depth_idx] > 0) {
                // this } closes a nested brace (e.g., arrow function body, object literal)
                self.brace_depth_stack[depth_idx] -= 1;
            } else {
                // this } closes the template substitution ${...}
                return self.scanTemplateMiddleOrTail();
            }
        }

        const start = self.cursor;
        self.cursor += 1;
        return self.createToken(.right_brace, self.source[start..self.cursor], start, self.cursor);
    }

    pub fn reScanAsRegex(self: *Lexer, slash_token: token.Token) LexicalError!struct { span: token.Span, pattern: []const u8, flags: []const u8, lexeme: []const u8 } {
        self.cursor = slash_token.span.start;

        const start = self.cursor;
        var closing_delimeter_pos: u32 = 0;
        self.cursor += 1; // consume '/'
        var in_class = false;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (util.Utf.isLineTerminator(self.source, self.cursor)) {
                return error.InvalidRegexLineTerminator;
            }

            if (c == '\\') {
                self.cursor += 1; // skip backslash
                if (self.cursor < self.source_len) {
                    self.cursor += 1; // skip escaped char
                }
                continue;
            }
            if (c == '[') {
                in_class = true;
                self.cursor += 1;
                continue;
            }
            if (c == ']' and in_class) {
                in_class = false;
                self.cursor += 1;
                continue;
            }
            if (c == '/' and !in_class) {
                self.cursor += 1;

                closing_delimeter_pos = self.cursor;

                while (self.cursor < self.source_len and
                    std.ascii.isAlphabetic(self.source[self.cursor]))
                {
                    self.cursor += 1;
                }

                const end = self.cursor;

                const pattern = self.source[start + 1 .. closing_delimeter_pos - 1];
                const flags = self.source[closing_delimeter_pos..end];

                return .{ .span = .{ .start = start, .end = end }, .lexeme = self.source[start..end], .pattern = pattern, .flags = flags };
            }

            self.cursor += 1;
        }
        return error.UnterminatedRegexLiteral;
    }

    fn scanDot(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        const c1 = self.peek(1);
        const c2 = self.peek(2);

        if (std.ascii.isDigit(c1)) {
            return self.scanNumber();
        }
        if (c1 == '.' and c2 == '.') {
            return self.makePunctuationToken(3, .spread, start);
        }
        return self.makePunctuationToken(1, .dot, start);
    }

    inline fn scanIdentifierBody(self: *Lexer) !void {
        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (std.ascii.isAscii(c)) {
                @branchHint(.likely);
                if (c == '\\') {
                    @branchHint(.cold);
                    if (self.peek(1) != 'u') {
                        return error.InvalidUnicodeEscape;
                    }
                    const c2 = self.peek(2);
                    if (c2 != '{') {
                        // \uXXXX format - decode and validate
                        if (util.Utf.parseHex4(self.source, self.cursor + 2)) |r| {
                            if (!util.UnicodeId.canContinueIdentifier(r.value)) {
                                return error.InvalidIdentifierContinue;
                            }
                        }
                    }
                    self.cursor += 1; // consume backslash to get to 'u'
                    try self.consumeUnicodeEscape();
                } else {
                    if ((c >= 'a' and c <= 'z') or
                        (c >= 'A' and c <= 'Z') or
                        (c >= '0' and c <= '9') or
                        c == '_' or c == '$')
                    {
                        self.cursor += 1;
                    } else {
                        break;
                    }
                }
            } else {
                @branchHint(.cold);
                const cp = try util.Utf.codePointAt(self.source, self.cursor);
                if (util.UnicodeId.canContinueIdentifier(cp.value)) {
                    self.cursor += cp.len;
                } else {
                    break;
                }
            }
        }
    }

    fn scanIdentifierOrKeyword(self: *Lexer) !token.Token {
        const start = self.cursor;

        const is_private = self.source[self.cursor] == '#';
        if (is_private) {
            self.cursor += 1;
        }

        const first_char = self.source[self.cursor];
        if (std.ascii.isAscii(first_char)) {
            @branchHint(.likely);
            if (first_char == '\\') {
                if (self.peek(1) != 'u') {
                    return error.InvalidUnicodeEscape;
                }
                const c2 = self.peek(2);
                if (c2 != '{') {
                    if (util.Utf.parseHex4(self.source, self.cursor + 2)) |r| {
                        if (!util.UnicodeId.canStartIdentifier(r.value)) {
                            return error.InvalidIdentifierStart;
                        }
                    }
                }
                self.cursor += 1; // consume backslash to get to 'u'
                try self.consumeUnicodeEscape();
            } else {
                if (!((first_char >= 'a' and first_char <= 'z') or
                    (first_char >= 'A' and first_char <= 'Z') or
                    first_char == '_' or first_char == '$'))
                {
                    @branchHint(.cold);
                    return error.InvalidIdentifierStart;
                }
                self.cursor += 1;
            }
            try self.scanIdentifierBody();
        } else {
            @branchHint(.cold);
            const c_cp = try util.Utf.codePointAt(self.source, self.cursor);
            if (!util.UnicodeId.canStartIdentifier(c_cp.value)) {
                return error.InvalidIdentifierStart;
            }
            self.cursor += c_cp.len;
            try self.scanIdentifierBody();
        }

        const lexeme = self.source[start..self.cursor];
        const token_type: token.TokenType = if (is_private) .private_identifier else self.getKeywordType(lexeme);
        return self.createToken(token_type, lexeme, start, self.cursor);
    }

    fn getKeywordType(self: *Lexer, lexeme: []const u8) token.TokenType {
        _ = self;
        switch (lexeme.len) {
            2 => {
                switch (lexeme[1]) {
                    'f' => {
                        return switch (lexeme[0]) {
                            'i' => .@"if",
                            'o' => .of,
                            else => .identifier,
                        };
                    },
                    'n' => if (lexeme[0] == 'i') return .in,
                    'o' => if (lexeme[0] == 'd') return .do,
                    's' => if (lexeme[0] == 'a') return .as,
                    else => {},
                }
            },
            3 => {
                switch (lexeme[0]) {
                    'f' => if (lexeme[1] == 'o' and lexeme[2] == 'r') return .@"for",
                    'l' => if (lexeme[1] == 'e' and lexeme[2] == 't') return .let,
                    'n' => if (lexeme[1] == 'e' and lexeme[2] == 'w') return .new,
                    't' => if (lexeme[1] == 'r' and lexeme[2] == 'y') return .@"try",
                    'v' => if (lexeme[1] == 'a' and lexeme[2] == 'r') return .@"var",
                    else => {},
                }
            },
            4 => {
                switch (lexeme[1]) {
                    'a' => if (lexeme[0] == 'c' and lexeme[2] == 's' and lexeme[3] == 'e') return .case,
                    'h' => if (lexeme[0] == 't' and lexeme[2] == 'i' and lexeme[3] == 's') return .this,
                    'l' => if (lexeme[0] == 'e' and lexeme[2] == 's' and lexeme[3] == 'e') return .@"else",
                    'n' => if (lexeme[0] == 'e' and lexeme[2] == 'u' and lexeme[3] == 'm') return .@"enum",
                    'o' => if (lexeme[0] == 'v' and lexeme[2] == 'i' and lexeme[3] == 'd') return .void,
                    'i' => if (lexeme[0] == 'w' and lexeme[2] == 't' and lexeme[3] == 'h') return .with,
                    'u' => if (lexeme[0] == 'n' and lexeme[2] == 'l' and lexeme[3] == 'l') return .null_literal,
                    'r' => {
                        if (lexeme[0] == 't' and lexeme[2] == 'u' and lexeme[3] == 'e') return .true;
                        if (lexeme[0] == 'f' and lexeme[2] == 'o' and lexeme[3] == 'm') return .from;
                    },
                    else => {},
                }
            },
            5 => {
                switch (lexeme[0]) {
                    'a' => {
                        if (lexeme[1] == 'w' and lexeme[2] == 'a' and lexeme[3] == 'i' and lexeme[4] == 't')
                            return .await;
                        if (lexeme[1] == 's' and lexeme[2] == 'y' and lexeme[3] == 'n' and lexeme[4] == 'c')
                            return .async;
                    },
                    'b' => if (lexeme[1] == 'r' and lexeme[2] == 'e' and lexeme[3] == 'a' and lexeme[4] == 'k')
                        return .@"break",
                    'c' => {
                        if (lexeme[1] == 'o' and lexeme[2] == 'n' and lexeme[3] == 's' and lexeme[4] == 't')
                            return .@"const";
                        if (lexeme[1] == 'l' and lexeme[2] == 'a' and lexeme[3] == 's' and lexeme[4] == 's')
                            return .class;
                        if (lexeme[1] == 'a' and lexeme[2] == 't' and lexeme[3] == 'c' and lexeme[4] == 'h')
                            return .@"catch";
                    },
                    'd' => if (lexeme[1] == 'e' and lexeme[2] == 'f' and lexeme[3] == 'e' and lexeme[4] == 'r')
                        return .@"defer",
                    'f' => if (lexeme[1] == 'a' and lexeme[2] == 'l' and lexeme[3] == 's' and lexeme[4] == 'e')
                        return .false,
                    's' => if (lexeme[1] == 'u' and lexeme[2] == 'p' and lexeme[3] == 'e' and lexeme[4] == 'r')
                        return .super,
                    't' => if (lexeme[1] == 'h' and lexeme[2] == 'r' and lexeme[3] == 'o' and lexeme[4] == 'w')
                        return .throw,
                    'u' => if (lexeme[1] == 's' and lexeme[2] == 'i' and lexeme[3] == 'n' and lexeme[4] == 'g')
                        return .using,
                    'w' => if (lexeme[1] == 'h' and lexeme[2] == 'i' and lexeme[3] == 'l' and lexeme[4] == 'e')
                        return .@"while",
                    'y' => if (lexeme[1] == 'i' and lexeme[2] == 'e' and lexeme[3] == 'l' and lexeme[4] == 'd')
                        return .yield,
                    else => {},
                }
            },
            6 => {
                switch (lexeme[0]) {
                    'a' => if (lexeme[1] == 's' and lexeme[2] == 's' and lexeme[3] == 'e' and lexeme[4] == 'r' and lexeme[5] == 't')
                        return .assert,
                    'd' => if (lexeme[1] == 'e' and lexeme[2] == 'l' and lexeme[3] == 'e' and lexeme[4] == 't' and lexeme[5] == 'e')
                        return .delete,
                    'e' => if (lexeme[1] == 'x' and lexeme[2] == 'p' and lexeme[3] == 'o' and lexeme[4] == 'r' and lexeme[5] == 't')
                        return .@"export",
                    'i' => if (lexeme[1] == 'm' and lexeme[2] == 'p' and lexeme[3] == 'o' and lexeme[4] == 'r' and lexeme[5] == 't')
                        return .import,
                    'p' => if (lexeme[1] == 'u' and lexeme[2] == 'b' and lexeme[3] == 'l' and lexeme[4] == 'i' and lexeme[5] == 'c')
                        return .public,
                    'r' => if (lexeme[1] == 'e' and lexeme[2] == 't' and lexeme[3] == 'u' and lexeme[4] == 'r' and lexeme[5] == 'n')
                        return .@"return",
                    's' => {
                        if (lexeme[1] == 'w' and lexeme[2] == 'i' and lexeme[3] == 't' and lexeme[4] == 'c' and lexeme[5] == 'h')
                            return .@"switch";
                        if (lexeme[1] == 't' and lexeme[2] == 'a' and lexeme[3] == 't' and lexeme[4] == 'i' and lexeme[5] == 'c')
                            return .static;
                        if (lexeme[1] == 'o' and lexeme[2] == 'u' and lexeme[3] == 'r' and lexeme[4] == 'c' and lexeme[5] == 'e')
                            return .source;
                    },
                    't' => if (lexeme[1] == 'y' and lexeme[2] == 'p' and lexeme[3] == 'e' and lexeme[4] == 'o' and lexeme[5] == 'f')
                        return .typeof,
                    else => {},
                }
            },
            7 => {
                switch (lexeme[0]) {
                    'd' => {
                        if (std.mem.eql(u8, lexeme, "default")) return .default;
                        if (std.mem.eql(u8, lexeme, "declare")) return .declare;
                    },
                    'e' => if (std.mem.eql(u8, lexeme, "extends")) return .extends,
                    'f' => if (std.mem.eql(u8, lexeme, "finally")) return .finally,
                    'p' => {
                        if (std.mem.eql(u8, lexeme, "private")) return .private;
                        if (std.mem.eql(u8, lexeme, "package")) return .package;
                    },
                    else => {},
                }
            },
            8 => {
                switch (lexeme[0]) {
                    'c' => if (std.mem.eql(u8, lexeme, "continue")) return .@"continue",
                    'd' => if (std.mem.eql(u8, lexeme, "debugger")) return .debugger,
                    'f' => if (std.mem.eql(u8, lexeme, "function")) return .function,
                    else => {},
                }
            },
            9 => {
                switch (lexeme[0]) {
                    'i' => if (std.mem.eql(u8, lexeme, "interface")) return .interface,
                    'n' => if (std.mem.eql(u8, lexeme, "namespace")) return .namespace,
                    'p' => if (std.mem.eql(u8, lexeme, "protected")) return .protected,
                    else => {},
                }
            },
            10 => {
                switch (lexeme[0]) {
                    'i' => {
                        if (std.mem.eql(u8, lexeme, "instanceof")) return .instanceof;
                        if (std.mem.eql(u8, lexeme, "implements")) return .implements;
                    },
                    else => {},
                }
            },
            else => {},
        }
        return .identifier;
    }

    fn scanNumber(self: *Lexer) LexicalError!token.Token {
        const start = self.cursor;
        var token_type: token.TokenType = .numeric_literal;
        var is_legacy_octal = false;

        // handle prefixes: 0x, 0o, 0b
        if (self.source[self.cursor] == '0') {
            const prefix = std.ascii.toLower(self.peek(1));

            switch (prefix) {
                'x' => {
                    token_type = .hex_literal;
                    self.cursor += 2;
                    const hex_start = self.cursor;
                    try self.consumeHexDigits();
                    if (self.cursor == hex_start) return error.InvalidHexLiteral;
                },
                'o' => {
                    token_type = .octal_literal;
                    self.cursor += 2;
                    const oct_start = self.cursor;
                    try self.consumeOctalDigits();
                    if (self.cursor == oct_start) return error.InvalidOctalLiteralDigit;
                },
                'b' => {
                    token_type = .binary_literal;
                    self.cursor += 2;
                    const bin_start = self.cursor;
                    try self.consumeBinaryDigits();
                    if (self.cursor == bin_start) return error.InvalidBinaryLiteral;
                },
                '0'...'7' => {
                    // potential legacy octal: 01, 07, etc.
                    is_legacy_octal = true;
                    try self.consumeDecimalDigits();

                    for (self.source[start..self.cursor]) |c| {
                        if (c == '8' or c == '9') {
                            is_legacy_octal = false;
                            break;
                        }
                    }

                    if (is_legacy_octal and self.strict_mode) {
                        return error.OctalLiteralInStrict;
                    }
                },
                else => {
                    try self.consumeDecimalDigits();
                },
            }
        } else {
            try self.consumeDecimalDigits();
        }

        // handle decimal point only for regular numbers, not legacy octals
        if (token_type == .numeric_literal and
            self.cursor < self.source_len and self.source[self.cursor] == '.')
        {
            const next = self.peek(1);
            if (next == '_') return error.NumericSeparatorMisuse;

            if (is_legacy_octal and !std.ascii.isDigit(next)) {
                // don't consume the '.', it's member access (e.g., 01.toString())
            } else {
                self.cursor += 1;
                if (std.ascii.isDigit(next)) try self.consumeDecimalDigits();
            }
        }

        // handle exponent (only for regular numbers)
        if (token_type == .numeric_literal and self.cursor < self.source_len) {
            const exp_char = std.ascii.toLower(self.source[self.cursor]);
            if (exp_char == 'e') {
                try self.consumeExponent();
            }
        }

        // handle bigint suffix 'n'
        if (self.cursor < self.source_len and self.source[self.cursor] == 'n') {
            // bigint cannot have decimal point or exponent
            if (token_type == .numeric_literal) {
                const lexeme = self.source[start..self.cursor];
                for (lexeme) |c| {
                    if (c == '.' or std.ascii.toLower(c) == 'e') {
                        return error.InvalidBigIntSuffix;
                    }
                }
            }

            self.cursor += 1;
            token_type = .bigint_literal;
        }

        // identifier cannot immediately follow a numeric literal
        if (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (std.ascii.isAlphabetic(c) or c == '_' or c == '$' or c == '\\') return error.IdentifierAfterNumericLiteral;
        }

        return self.createToken(token_type, self.source[start..self.cursor], start, self.cursor);
    }

    inline fn consumeDigits(self: *Lexer, comptime isValidDigit: fn (u8) bool) LexicalError!void {
        var last_was_separator = false;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];
            if (isValidDigit(c)) {
                self.cursor += 1;
                last_was_separator = false;
            } else if (c == '_') {
                if (last_was_separator) {
                    return error.ConsecutiveNumericSeparators;
                }
                self.cursor += 1;
                last_was_separator = true;
            } else {
                break;
            }
        }

        if (last_was_separator) {
            return error.NumericSeparatorMisuse;
        }
    }

    inline fn consumeDecimalDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(std.ascii.isDigit);
    }

    inline fn consumeHexDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(std.ascii.isHex);
    }

    inline fn consumeOctalDigits(self: *Lexer) LexicalError!void {
        return self.consumeDigits(util.Utf.isOctalDigit);
    }

    inline fn consumeBinaryDigits(self: *Lexer) LexicalError!void {
        const isBinary = struct {
            fn check(c: u8) bool {
                return c == '0' or c == '1';
            }
        }.check;
        return self.consumeDigits(isBinary);
    }

    fn consumeExponent(self: *Lexer) LexicalError!void {
        self.cursor += 1; // skip 'e' or 'E'

        if (self.cursor >= self.source_len) {
            return error.InvalidExponentPart;
        }

        // handle optional sign: + or -
        const c = self.source[self.cursor];
        if (c == '+' or c == '-') {
            self.cursor += 1;
        }

        const exp_start = self.cursor;
        try self.consumeDecimalDigits();

        if (self.cursor == exp_start) {
            return error.InvalidExponentPart;
        }
    }

    inline fn skipSkippable(self: *Lexer) LexicalError!void {
        // track if we're at a logical line start (start of file OR after newline)
        // this persists through whitespace/comment skipping for HTML close comment detection
        var at_line_start = self.cursor == 0 or self.has_line_terminator_before;

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (std.ascii.isAscii(c)) {
                @branchHint(.likely);
                switch (c) {
                    ' ', '\t', '\u{000B}', '\u{000C}' => {
                        self.cursor += 1;
                        continue;
                    },
                    '\n', '\r' => {
                        self.has_line_terminator_before = true;
                        at_line_start = true;
                        self.cursor += 1;
                        continue;
                    },
                    '/' => {
                        const next = self.peek(1);
                        if (next == '/') {
                            try self.scanLineComment();
                            continue;
                        } else if (next == '*') {
                            try self.scanBlockComment();
                            continue;
                        }
                        break;
                    },
                    '<' => {
                        // HTML-style comments (<!-- ... -->) are only valid in script mode
                        if (self.source_type == .script) {
                            const c1 = self.peek(1);
                            const c2 = self.peek(2);
                            const c3 = self.peek(3);
                            if (c1 == '!' and c2 == '-' and c3 == '-') {
                                try self.scanHtmlComment();
                                continue;
                            }
                        }
                        break;
                    },
                    '-' => {
                        // HTML-style close comment --> is only valid at line start in script mode
                        // "line start" means start of file or after newline, with only whitespace/comments before
                        if (self.source_type == .script and at_line_start) {
                            const c1 = self.peek(1);
                            const c2 = self.peek(2);
                            if (c1 == '-' and c2 == '>') {
                                try self.scanHtmlCloseComment();
                                continue;
                            }
                        }
                        break;
                    },
                    else => break,
                }
            } else {
                @branchHint(.unlikely);

                const lt_len = util.Utf.lineTerminatorLen(self.source, self.cursor);
                if (lt_len > 0) {
                    self.has_line_terminator_before = true;
                    self.cursor += lt_len;
                    continue;
                }

                // multi-byte space character
                const cp = try util.Utf.codePointAt(self.source, self.cursor);
                if (util.Utf.isMultiByteSpace(cp.value)) {
                    self.cursor += cp.len;
                    continue;
                }
            }

            break;
        }
    }

    /// scans a single-line comment (// ...)
    fn scanLineComment(self: *Lexer) LexicalError!void {
        const start = self.cursor;
        self.cursor += 2; // skip '//'

        while (self.cursor < self.source_len) {
            if (util.Utf.isLineTerminator(self.source, self.cursor)) {
                break;
            }
            self.cursor += 1;
        }

        self.comments.append(self.allocator, .{
            .type = .line,
            .start = start,
            .end = self.cursor,
        }) catch return error.OutOfMemory;
    }

    /// scans a multi-line comment (/* ... */)
    fn scanBlockComment(self: *Lexer) LexicalError!void {
        const start = self.cursor;
        self.cursor += 2; // skip '/*'

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            if (util.Utf.isLineTerminator(self.source, self.cursor)) {
                self.has_line_terminator_before = true;
            }

            if (c == '*' and self.peek(1) == '/') {
                self.cursor += 2; // skip '*/'
                self.comments.append(self.allocator, .{
                    .type = .block,
                    .start = start,
                    .end = self.cursor,
                }) catch return error.OutOfMemory;
                return;
            }

            self.cursor += 1;
        }

        return error.UnterminatedMultiLineComment;
    }

    /// scans an HTML-style comment (<!-- ... --> or <!-- ... end of line)
    fn scanHtmlComment(self: *Lexer) LexicalError!void {
        const start = self.cursor;
        self.cursor += 4; // skip '<!--'

        while (self.cursor < self.source_len) {
            const c = self.source[self.cursor];

            // check for early termination with -->
            if (c == '-' and self.peek(1) == '-' and self.peek(2) == '>') {
                self.cursor += 3; // skip '-->'
                self.comments.append(self.allocator, .{
                    .type = .line,
                    .start = start,
                    .end = self.cursor,
                }) catch return error.OutOfMemory;
                return;
            }

            if (util.Utf.isLineTerminator(self.source, self.cursor)) {
                break;
            }

            self.cursor += 1;
        }

        // comment ends at end of line
        self.comments.append(self.allocator, .{
            .type = .line,
            .start = start,
            .end = self.cursor,
        }) catch return error.OutOfMemory;
    }

    /// scans an HTML-style close comment (--> ... end of line)
    fn scanHtmlCloseComment(self: *Lexer) LexicalError!void {
        const start = self.cursor;
        self.cursor += 3; // skip '-->'

        while (self.cursor < self.source_len) {
            if (util.Utf.isLineTerminator(self.source, self.cursor)) {
                break;
            }
            self.cursor += 1;
        }

        self.comments.append(self.allocator, .{
            .type = .line,
            .start = start,
            .end = self.cursor,
        }) catch return error.OutOfMemory;
    }

    pub inline fn createToken(self: *Lexer, token_type: token.TokenType, lexeme: []const u8, start: u32, end: u32) token.Token {
        const tok = token.Token{
            .type = token_type,
            .lexeme = lexeme,
            .span = .{ .start = start, .end = end },
            .has_line_terminator_before = self.has_line_terminator_before,
        };

        self.has_line_terminator_before = false;
        return tok;
    }
};

pub fn getLexicalErrorMessage(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Invalid hexadecimal escape sequence",
        error.UnterminatedString => "Unterminated string literal",
        error.UnterminatedRegex => "Unterminated regular expression",
        error.NonTerminatedTemplateLiteral => "Unterminated template literal",
        error.UnterminatedRegexLiteral => "Unterminated regular expression literal",
        error.InvalidRegexLineTerminator => "Line terminator not allowed in regular expression literal",
        error.InvalidRegex => "Invalid regular expression",
        error.InvalidIdentifierStart => "Invalid character at start of identifier",
        error.InvalidIdentifierContinue => "Invalid character in identifier",
        error.UnterminatedMultiLineComment => "Unterminated multi-line comment",
        error.InvalidUnicodeEscape => "Invalid Unicode escape sequence",
        error.InvalidOctalEscape => "Invalid octal escape sequence",
        error.OctalEscapeInStrict => "Octal escape sequences are not allowed in strict mode",
        error.OctalLiteralInStrict => "Octal literals are not allowed in strict mode",
        error.InvalidBinaryLiteral => "Binary literal must contain at least one binary digit",
        error.InvalidOctalLiteralDigit => "Octal literal must contain at least one octal digit",
        error.InvalidHexLiteral => "Hexadecimal literal must contain at least one hex digit",
        error.InvalidExponentPart => "Exponent part is missing a number",
        error.NumericSeparatorMisuse => "Numeric separator cannot appear at the end of a numeric literal",
        error.ConsecutiveNumericSeparators => "Numeric literal cannot contain consecutive separators",
        error.MultipleDecimalPoints => "Numeric literal cannot contain multiple decimal points",
        error.InvalidBigIntSuffix => "BigInt literal cannot contain decimal point or exponent",
        error.IdentifierAfterNumericLiteral => "Identifier cannot immediately follow a numeric literal",
        error.InvalidUtf8 => "Invalid UTF-8 byte sequence",
        error.OutOfMemory => "Out of memory",
    };
}

pub fn getLexicalErrorHelp(error_type: LexicalError) []const u8 {
    return switch (error_type) {
        error.InvalidHexEscape => "Try adding two hexadecimal digits here (e.g., \\x41 for 'A')",
        error.UnterminatedString => "Try adding a closing quote here to complete the string",
        error.UnterminatedRegex => "Try adding a closing slash (/) here to complete the regex",
        error.NonTerminatedTemplateLiteral => "Try adding a closing backtick (`) here to complete the template",
        error.UnterminatedRegexLiteral => "Try adding a closing slash (/) here, optionally followed by flags (g, i, m, etc.)",
        error.InvalidRegexLineTerminator => "Try removing the line break here or escaping it within the regex pattern",
        error.InvalidRegex => "Try checking the regex syntax here for unclosed groups, invalid escapes, or malformed patterns",
        error.InvalidIdentifierStart => "Try starting the identifier here with a letter (a-z, A-Z), underscore (_), or dollar sign ($)",
        error.InvalidIdentifierContinue => "Try using a valid identifier character here (letters, digits, underscore, or dollar sign)",
        error.UnterminatedMultiLineComment => "Try adding the closing delimiter (*/) here to complete the comment",
        error.InvalidUnicodeEscape => "Try using \\uHHHH (4 hex digits) or \\u{HHHHHH} (1-6 hex digits) here",
        error.InvalidOctalEscape => "Try using a valid octal sequence here (\\0-7, \\00-77, or \\000-377)",
        error.OctalEscapeInStrict => "Try replacing this octal escape with \\xHH (hex) or \\uHHHH (unicode) instead",
        error.OctalLiteralInStrict => "Try replacing this octal literal with a decimal number, or use 0xHH (hex) or 0bBB (binary) instead",
        error.InvalidBinaryLiteral => "Try adding at least one binary digit (0 or 1) here after '0b'",
        error.InvalidOctalLiteralDigit => "Try adding at least one octal digit (0-7) here after '0o'",
        error.InvalidHexLiteral => "Try adding at least one hex digit (0-9, a-f, A-F) here after '0x'",
        error.InvalidExponentPart => "Try adding digits here after the exponent (e.g., e10, e-5, E+2)",
        error.NumericSeparatorMisuse => "Try removing the trailing underscore here or adding more digits after it",
        error.ConsecutiveNumericSeparators => "Try removing one of the consecutive underscores here",
        error.MultipleDecimalPoints => "Try removing the extra decimal point here",
        error.InvalidBigIntSuffix => "Try removing the 'n' suffix here, or remove the decimal point/exponent from the number",
        error.IdentifierAfterNumericLiteral => "Try adding whitespace here between the number and identifier",
        error.InvalidUtf8 => "The source file contains invalid UTF-8 encoding. Ensure the file is saved with valid UTF-8 encoding",
        error.OutOfMemory => "The system ran out of memory while parsing",
    };
}
