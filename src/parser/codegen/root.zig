const printer = @import("printer.zig");

pub const Options = printer.Options;
pub const Result = printer.Result;
pub const Format = printer.Format;
pub const Quotes = printer.Quotes;
pub const Comments = printer.Comments;
pub const SourceMap = printer.SourceMap;
pub const SourceMapOptions = printer.SourceMapOptions;

pub const generate = printer.generate;

test {
    _ = printer;
    _ = @import("sourcemap.zig");
}
