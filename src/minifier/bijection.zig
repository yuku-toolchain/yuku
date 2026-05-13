pub const Bijection = struct {
    counter: usize = 0,
    buf: [16]u8 = undefined,

    const head_alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";
    const tail_alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$0123456789";

    pub fn next(self: *Bijection) []const u8 {
        const out = nameAt(self.counter, &self.buf);
        self.counter += 1;
        return out;
    }

    pub fn reset(self: *Bijection) void {
        self.counter = 0;
    }

    /// Pure encoding of `counter` into the buffer. Does not touch
    /// `self.counter`, so callers can probe arbitrary positions
    /// (e.g. when advancing a per-scope cursor) without disturbing
    /// the running sequence.
    pub fn nameAt(counter: usize, buf: []u8) []const u8 {
        var n = counter;

        const head_len: usize = head_alpha.len;
        if (n < head_len) {
            buf[0] = head_alpha[n];
            return buf[0..1];
        }
        n -= head_len;

        const tail_len: usize = tail_alpha.len;
        var len: usize = 1;
        var place: usize = head_len * tail_len;
        while (n >= place) : (len += 1) {
            n -= place;
            place *= tail_len;
        }
        var rem = n;
        for (0..len) |i| {
            buf[len - i] = tail_alpha[rem % tail_len];
            rem /= tail_len;
        }
        buf[0] = head_alpha[rem % head_len];
        return buf[0 .. len + 1];
    }
};
