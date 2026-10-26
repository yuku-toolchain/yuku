//! The bits of the `flags` argument that the `yuku` package passes to `parse` and `analyze`.

const parser = @import("parser");

// packed by npm/yuku/core/read.ts
pub const source_type_mask = 0b11;
pub const lang_shift = 2;
pub const preserve_parens = 1 << 5;
pub const semantic_errors = 1 << 6;
pub const attach_comments = 1 << 7;
pub const tokens = 1 << 8;
pub const dialect = 1 << 9;
pub const utf16 = 1 << 10;

pub fn parseOptions(flags: u32) parser.Options {
    return .{
        .source_type = @fromBackingInt(@truncate(flags & source_type_mask)),
        .lang = @fromBackingInt(@truncate(flags >> lang_shift)),
        .preserve_parens = flags & preserve_parens != 0,
        .comments = if (flags & attach_comments != 0) .attach else .collect,
        .tokens = flags & tokens != 0,
        .dialect = flags & dialect != 0,
    };
}
