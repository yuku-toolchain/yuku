const printer = @import("printer.zig");

pub const Options = printer.Options;
pub const Result = printer.Result;
pub const Diagnostic = printer.Diagnostic;
pub const Format = printer.Format;
pub const Quotes = printer.Quotes;
pub const Error = printer.Error;

pub const print = printer.print;
pub const strip = printer.strip;
pub const minify = printer.minify;

test {
    _ = printer;
}
