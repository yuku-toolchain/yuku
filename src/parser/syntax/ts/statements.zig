// typescript declaration and statement parsing
//
// parses typescript-specific declarations such as type aliases, interfaces,
// enums, namespaces, and module declarations. called from the main statement
// parser when typescript syntax is encountered.

const std = @import("std");
const ast = @import("../../ast.zig");
const Parser = @import("../../parser.zig").Parser;
const Error = @import("../../parser.zig").Error;
const TokenTag = @import("../../token.zig").TokenTag;
