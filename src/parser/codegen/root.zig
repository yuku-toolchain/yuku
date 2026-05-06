const printer = @import("printer.zig");

/// Codegen options. See `printer.Options` for field-by-field reference.
pub const Options = printer.Options;

/// Output of a codegen run.
pub const Result = printer.Result;

/// A codegen-detected problem in the input tree.
pub const Diagnostic = printer.Diagnostic;

/// Whitespace mode, `pretty` (indented) or `compact` (no discretionary whitespace).
pub const Format = printer.Format;

/// Source map mode. Currently `none`; `v3` lands in a later phase.
pub const SourceMap = printer.SourceMap;

/// String literal quoting style.
pub const Quotes = printer.Quotes;

/// All codegen errors are allocation errors.
pub const Error = printer.Error;

/// Renders a `Tree` to source code.
pub const print = printer.print;

/// Renders a TypeScript `Tree` to JavaScript by erasing TS-only syntax.
/// Equivalent to `print(...)` with `strip_ts = true`.
pub const strip = printer.strip;
