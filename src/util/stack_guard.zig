const StackGuard = @This();

floor: usize,

pub const default_bytes: usize = 3 * 1024 * 1024;

pub inline fn init(bytes: usize) StackGuard {
    return .{ .floor = @frameAddress() -| bytes };
}

pub inline fn exhausted(self: StackGuard) bool {
    return @frameAddress() < self.floor;
}
