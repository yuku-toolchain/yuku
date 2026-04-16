// typescript type parsing
//
// parses type annotations, type expressions, and type-level constructs.
// called from expression and statement parsing when typescript syntax is encountered.

const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;
