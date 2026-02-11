const std = @import("std");
const codspeed = @import("codspeed");

pub fn main() !void {
    var session = try codspeed.initSession(std.heap.c_allocator);
    defer session.deinit();

    try session.bench("ci/smoke", struct {
        fn run() void {
            var acc: u64 = 0;
            var i: u64 = 0;
            while (i < 1000) : (i += 1) acc += i;
            std.mem.doNotOptimizeAway(acc);
        }
    }.run);
}
