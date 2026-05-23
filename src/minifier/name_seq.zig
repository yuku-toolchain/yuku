const std = @import("std");

pub const NameSeq = struct {
    counter: usize = 0,
    buf: [16]u8 = undefined,

    /// Characters ordered by frequency in real-world JS bundles. Slot 0 gets
    /// `e`, slot 1 `t`, and so on. These letters already dominate the
    /// surrounding source, so mangled identifiers blend in and gzip
    /// back-references win. First 54 entries are valid IdentifierStart
    /// (letters, `_`, `$`), the trailing 10 are digits, used only from the
    /// second position onward. Frequency-ordered idea borrowed from nanoid.
    const BASE_CHARS = "etnriaoscludfpmhg_vybxSCwTEDOkAjMNPFILRzBVHUWGKqJYXZQ$1024368579";
    const FIRST_BASE: usize = 54;
    const REST_BASE: usize = 64;

    /// Pure encoding of `counter` into the buffer. Does not touch
    /// `self.counter`, so callers can probe arbitrary positions
    /// (e.g. when advancing a per-scope cursor) without disturbing
    /// the running sequence.
    pub fn nameAt(counter: usize, buf: []u8) []const u8 {
        std.debug.assert(buf.len > 0);
        var n = counter;
        var len: usize = 0;

        buf[len] = BASE_CHARS[n % FIRST_BASE];
        len += 1;
        n /= FIRST_BASE;

        while (n > 0) {
            std.debug.assert(len < buf.len);
            n -= 1;
            buf[len] = BASE_CHARS[n % REST_BASE];
            len += 1;
            n /= REST_BASE;
        }

        std.debug.assert(len > 0 and len <= buf.len);
        return buf[0..len];
    }
};
