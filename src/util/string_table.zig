const std = @import("std");

pub const StringId = enum(u32) { empty = std.math.maxInt(u32), _ };

/// Compact string storage. Strings are stored as length-prefixed entries
/// in a flat byte buffer. StringId is the byte offset of the entry.
pub const StringTable = struct {
    buffer: std.ArrayList(u8) = .empty,

    pub inline fn intern(self: *StringTable, alloc: std.mem.Allocator, str: []const u8) error{OutOfMemory}!StringId {
        const offset: u32 = @intCast(self.buffer.items.len);
        const len: u32 = @intCast(str.len);
        const needed = 4 + str.len;
        if (self.buffer.items.len + needed > self.buffer.capacity) {
            try self.buffer.ensureUnusedCapacity(alloc, needed);
        }
        self.buffer.appendSliceAssumeCapacity(std.mem.asBytes(&len));
        self.buffer.appendSliceAssumeCapacity(str);
        return @enumFromInt(offset);
    }

    pub inline fn get(self: *const StringTable, id: StringId) []const u8 {
        return resolve(self.buffer.items, id);
    }

    /// Resolve a StringId from a finalized string buffer.
    pub inline fn resolve(buffer: []const u8, id: StringId) []const u8 {
        if (id == .empty) return "";
        const offset = @intFromEnum(id);
        const len: u32 = @bitCast(buffer[offset..][0..4].*);
        return buffer[offset + 4 ..][0..len];
    }
};
