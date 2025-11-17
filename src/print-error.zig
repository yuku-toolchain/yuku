// this will be a really bad code since this is AI generated for quick testing of errors while development
// after the parser work done, we will start working on error formatter without which will be much better.
const std = @import("std");
const ParserError = @import("parser.zig").Error;

fn isKeyword(word: []const u8) bool {
    return switch (word.len) {
        2 => std.mem.eql(u8, word, "if") or std.mem.eql(u8, word, "of") or std.mem.eql(u8, word, "in") or std.mem.eql(u8, word, "do") or std.mem.eql(u8, word, "as"),
        3 => std.mem.eql(u8, word, "for") or std.mem.eql(u8, word, "let") or std.mem.eql(u8, word, "new") or std.mem.eql(u8, word, "try") or std.mem.eql(u8, word, "var"),
        4 => std.mem.eql(u8, word, "case") or std.mem.eql(u8, word, "this") or std.mem.eql(u8, word, "else") or std.mem.eql(u8, word, "enum") or std.mem.eql(u8, word, "void") or std.mem.eql(u8, word, "with") or std.mem.eql(u8, word, "null") or std.mem.eql(u8, word, "true") or std.mem.eql(u8, word, "from"),
        5 => std.mem.eql(u8, word, "await") or std.mem.eql(u8, word, "async") or std.mem.eql(u8, word, "break") or std.mem.eql(u8, word, "const") or std.mem.eql(u8, word, "class") or std.mem.eql(u8, word, "catch") or std.mem.eql(u8, word, "false") or std.mem.eql(u8, word, "super") or std.mem.eql(u8, word, "throw") or std.mem.eql(u8, word, "using") or std.mem.eql(u8, word, "while") or std.mem.eql(u8, word, "yield"),
        6 => std.mem.eql(u8, word, "delete") or std.mem.eql(u8, word, "export") or std.mem.eql(u8, word, "import") or std.mem.eql(u8, word, "public") or std.mem.eql(u8, word, "return") or std.mem.eql(u8, word, "switch") or std.mem.eql(u8, word, "static") or std.mem.eql(u8, word, "typeof"),
        7 => std.mem.eql(u8, word, "default") or std.mem.eql(u8, word, "extends") or std.mem.eql(u8, word, "finally") or std.mem.eql(u8, word, "private"),
        8 => std.mem.eql(u8, word, "continue") or std.mem.eql(u8, word, "debugger") or std.mem.eql(u8, word, "function"),
        9 => std.mem.eql(u8, word, "interface") or std.mem.eql(u8, word, "protected") or std.mem.eql(u8, word, "undefined"),
        10 => std.mem.eql(u8, word, "instanceof") or std.mem.eql(u8, word, "implements"),
        else => false,
    };
}

fn printHighlightedLine(line: []const u8) void {
    var i: usize = 0;
    while (i < line.len) {
        const c = line[i];
        // Handle string literals
        if (c == '"' or c == '\'') {
            const quote_char = c;
            std.debug.print("\x1b[32m{c}", .{c}); // green
            i += 1;
            var escaped = false;
            while (i < line.len) {
                const next = line[i];
                if (escaped) {
                    std.debug.print("\x1b[32m{c}", .{next});
                    escaped = false;
                    i += 1;
                } else if (next == '\\') {
                    std.debug.print("\x1b[32m{c}", .{next});
                    escaped = true;
                    i += 1;
                } else if (next == quote_char) {
                    std.debug.print("\x1b[32m{c}\x1b[0m", .{next});
                    i += 1;
                    break;
                } else {
                    std.debug.print("\x1b[32m{c}", .{next});
                    i += 1;
                }
            }
            continue;
        }
        // Handle template literals
        if (c == '`') {
            std.debug.print("\x1b[32m`", .{});
            i += 1;
            var escaped = false;
            while (i < line.len) {
                const next = line[i];
                if (escaped) {
                    std.debug.print("\x1b[32m{c}", .{next});
                    escaped = false;
                    i += 1;
                } else if (next == '\\') {
                    std.debug.print("\x1b[32m{c}", .{next});
                    escaped = true;
                    i += 1;
                } else if (next == '`') {
                    std.debug.print("\x1b[32m`\x1b[0m", .{});
                    i += 1;
                    break;
                } else {
                    std.debug.print("\x1b[32m{c}", .{next});
                    i += 1;
                }
            }
            continue;
        }
        // Handle comments
        if (i + 1 < line.len and c == '/' and line[i + 1] == '/') {
            std.debug.print("\x1b[2;37m", .{}); // dim gray
            while (i < line.len) {
                std.debug.print("{c}", .{line[i]});
                i += 1;
            }
            std.debug.print("\x1b[0m", .{});
            break;
        }
        // Handle numeric literals
        if ((c >= '0' and c <= '9') or (c == '.' and i + 1 < line.len and line[i + 1] >= '0' and line[i + 1] <= '9')) {
            std.debug.print("\x1b[33m", .{}); // yellow
            var has_dot = c == '.';
            var has_exp = false;
            std.debug.print("{c}", .{c});
            i += 1;
            while (i < line.len) {
                const next = line[i];
                if (next >= '0' and next <= '9') {
                    std.debug.print("{c}", .{next});
                    i += 1;
                } else if (next == '.' and !has_dot) {
                    std.debug.print("{c}", .{next});
                    has_dot = true;
                    i += 1;
                } else if ((next == 'e' or next == 'E') and !has_exp) {
                    std.debug.print("{c}", .{next});
                    has_exp = true;
                    i += 1;
                    if (i < line.len and (line[i] == '+' or line[i] == '-')) {
                        std.debug.print("{c}", .{line[i]});
                        i += 1;
                    }
                } else if (next == 'n' and has_exp == false) {
                    std.debug.print("{c}\x1b[0m", .{next});
                    i += 1;
                    break;
                } else if (std.ascii.isAlphanumeric(next) or next == '_') {
                    std.debug.print("\x1b[0m{c}", .{next});
                    i += 1;
                    break;
                } else {
                    break;
                }
            }
            std.debug.print("\x1b[0m", .{});
            continue;
        }
        // Handle identifiers and keywords
        if (std.ascii.isAlphabetic(c) or c == '_' or c == '$') {
            const start = i;
            while (i < line.len) {
                const next = line[i];
                if (std.ascii.isAlphanumeric(next) or next == '_' or next == '$') {
                    i += 1;
                } else {
                    break;
                }
            }
            const word = line[start..i];
            if (isKeyword(word)) {
                std.debug.print("\x1b[35m{s}\x1b[0m", .{word}); // magenta for keywords
            } else {
                std.debug.print("{s}", .{word}); // plain for identifiers
            }
            continue;
        }
        // Handle operators and punctuation (light blue/cyan)
        if (c == '+' or c == '-' or c == '*' or c == '/' or c == '%' or
            c == '=' or c == '<' or c == '>' or c == '!' or c == '&' or
            c == '|' or c == '^' or c == '?' or c == '~')
        {
            std.debug.print("\x1b[36m{c}\x1b[0m", .{c}); // cyan
            i += 1;
            continue;
        }
        // Handle brackets and delimiters
        if (c == '(' or c == ')' or c == '[' or c == ']' or c == '{' or c == '}' or
            c == ',' or c == ';' or c == ':' or c == '.')
        {
            std.debug.print("\x1b[90m{c}\x1b[0m", .{c}); // bright black/gray
            i += 1;
            continue;
        }
        // Default: print as-is
        std.debug.print("{c}", .{c});
        i += 1;
    }
}

fn getVisualColumn(line: []const u8, byte_pos: usize) usize {
    var col: usize = 0;
    var i: usize = 0;
    while (i < byte_pos and i < line.len) : (i += 1) {
        if (line[i] == '\t') {
            col = (col + 4) & ~@as(usize, 3);
        } else {
            col += 1;
        }
    }
    return col;
}

pub fn printError(source: []const u8, err: ParserError) void {
    var line_start_positions = std.ArrayList(usize).empty;
    defer line_start_positions.deinit(std.heap.page_allocator);
    line_start_positions.append(std.heap.page_allocator, 0) catch return;

    for (source, 0..) |c, i| {
        if (c == '\n') {
            line_start_positions.append(std.heap.page_allocator, i + 1) catch return;
        }
    }

    // Find which lines contain the error span
    var start_line: usize = 0;
    var end_line: usize = 0;

    for (line_start_positions.items, 0..) |line_start, line_num| {
        if (err.span.start >= line_start) {
            start_line = line_num;
        }
        if (err.span.end > line_start) {
            end_line = line_num;
        }
    }

    const max_line_num = line_start_positions.items.len;
    const line_num_width = blk: {
        var width: usize = 1;
        var num = max_line_num;
        while (num >= 10) : (num /= 10) {
            width += 1;
        }
        break :blk width;
    };

    // Get the starting line information for error location
    const first_line_start = line_start_positions.items[start_line];
    const start_col = err.span.start - first_line_start;
    const first_line_end = if (start_line + 1 < line_start_positions.items.len)
        line_start_positions.items[start_line + 1] - 1
    else
        source.len;
    const first_line_content = source[first_line_start..first_line_end];
    const visual_start_col = getVisualColumn(first_line_content, start_col);

    std.debug.print("\x1b[1;31merror\x1b[0m: {s}\n", .{err.message});
    std.debug.print("  \x1b[1;34m-->\x1b[0m src/test.js:{}:{}\n\n", .{ start_line + 1, visual_start_col + 1 });

    // Print all affected lines
    var current_line = start_line;
    while (current_line <= end_line) : (current_line += 1) {
        const line_start = line_start_positions.items[current_line];
        const line_end = if (current_line + 1 < line_start_positions.items.len)
            line_start_positions.items[current_line + 1] - 1
        else
            source.len;
        const line_content = source[line_start..line_end];

        // Print line number and content
        var line_num_buf: [16]u8 = undefined;
        const line_num_str = std.fmt.bufPrint(&line_num_buf, "{}", .{current_line + 1}) catch "1";
        var padding: [16]u8 = undefined;
        const padding_len = if (line_num_str.len < line_num_width) line_num_width - line_num_str.len else 0;
        @memset(padding[0..padding_len], ' ');
        const line_num_padded = std.fmt.bufPrint(&padding, "{s}{s}", .{ padding[0..padding_len], line_num_str }) catch line_num_str;

        std.debug.print(" \x1b[2;36m{s}\x1b[0m \x1b[2m|\x1b[0m ", .{line_num_padded});
        printHighlightedLine(line_content);
        std.debug.print("\n", .{});

        // Print underline for this line
        var empty_padding: [16]u8 = undefined;
        @memset(empty_padding[0..line_num_width], ' ');
        const empty_padded = empty_padding[0..line_num_width];
        std.debug.print(" \x1b[2;36m{s}\x1b[0m \x1b[2m|\x1b[0m ", .{empty_padded});

        // Calculate where to start and end the underline for this line
        const line_error_start = if (current_line == start_line) err.span.start - line_start else 0;
        const line_error_end = if (current_line == end_line)
            @min(err.span.end - line_start, line_content.len)
        else
            line_content.len;

        const visual_error_start = getVisualColumn(line_content, line_error_start);
        const visual_error_end = getVisualColumn(line_content, line_error_end);

        // Print spaces up to error start
        var i: usize = 0;
        while (i < visual_error_start) : (i += 1) {
            std.debug.print(" ", .{});
        }

        // Print underline with only ~ characters
        while (i < visual_error_end) : (i += 1) {
            std.debug.print("\x1b[1;31m~\x1b[0m", .{});
        }

        std.debug.print("\n", .{});
    }

    std.debug.print("\n", .{});

    if (err.help) |help| {
        std.debug.print("\x1b[1;36mhelp\x1b[0m: {s}\n\n", .{help});
    }
}
