pub const Bijection = struct {
    counter: usize = 0,
    buf: [16]u8 = undefined,

    const head_alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$";
    const tail_alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$0123456789";

    pub fn next(self: *Bijection) []const u8 {
        var n = self.counter;
        self.counter += 1;

        const head_len: usize = head_alpha.len;
        if (n < head_len) {
            self.buf[0] = head_alpha[n];
            return self.buf[0..1];
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
            self.buf[len - i] = tail_alpha[rem % tail_len];
            rem /= tail_len;
        }
        self.buf[0] = head_alpha[rem % head_len];
        return self.buf[0 .. len + 1];
    }
};
