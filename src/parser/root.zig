const parser = @import("parser.zig");
const ast = @import("ast.zig");

pub const parse = parser.parse;
pub const ParseTree = ast.ParseTree;
pub const Options = parser.Options;
pub const Diagnostic = ast.Diagnostic;
pub const Severity = ast.Severity;
pub const Label = ast.Label;
pub const SourceType = ast.SourceType;
pub const Lang = ast.Lang;

pub const estree = @import("estree.zig");
