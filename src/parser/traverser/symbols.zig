const std = @import("std");
const ast = @import("../ast.zig");
const ScopeId = @import("scoped.zig").ScopeId;

const Allocator = std.mem.Allocator;

pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };

pub const Symbol = struct {
    name_start: u32,
    name_len: u16,
    node: ast.NodeIndex,
    scope: ScopeId,
    kind: Kind,
    flags: Flags,

    pub const Kind = enum(u8) { variable, function, parameter, import, class, type_alias };

    pub const Flags = packed struct(u8) {
        @"const": bool = false,
        exported: bool = false,
        hoisted: bool = false,
        _pad: u5 = 0,
    };

    pub inline fn name(self: Symbol, source: []const u8) []const u8 {
        return source[self.name_start..][0..self.name_len];
    }
};

pub const SymbolTable = struct {
    symbols: std.ArrayList(Symbol) = .{},
    names: std.StringHashMap(SymbolId),
    shadows: std.ArrayList(ShadowEntry) = .{},
    save_points: std.ArrayList(u32) = .{},
    allocator: Allocator,

    const ShadowEntry = struct {
        name: []const u8,
        prev: SymbolId,
    };

    pub fn init(allocator: Allocator) SymbolTable {
        var self = SymbolTable{ .allocator = allocator, .names = .init(allocator) };
        self.symbols.ensureTotalCapacity(allocator, 256) catch {};
        self.names.ensureTotalCapacity(256) catch {};
        self.shadows.ensureTotalCapacity(allocator, 256) catch {};
        self.save_points.ensureTotalCapacity(allocator, 64) catch {};
        return self;
    }

    /// Mark scope entry. Must be paired with `popScope`.
    pub fn pushScope(self: *SymbolTable) void {
        self.save_points.append(self.allocator, @intCast(self.shadows.items.len)) catch unreachable;
    }

    /// Restore all names shadowed since the matching `pushScope`.
    pub fn popScope(self: *SymbolTable) void {
        const base = self.save_points.pop().?;
        while (self.shadows.items.len > base) {
            const entry = self.shadows.pop().?;
            if (entry.prev == .none) {
                _ = self.names.remove(entry.name);
            } else {
                self.names.putAssumeCapacity(entry.name, entry.prev);
            }
        }
    }

    /// Declare a symbol in the current scope. Returns its SymbolId.
    pub fn declare(self: *SymbolTable, sym: Symbol, name_slice: []const u8) SymbolId {
        const id: SymbolId = @enumFromInt(@as(u32, @intCast(self.symbols.items.len)));

        self.symbols.append(self.allocator, sym) catch unreachable;

        const prev = self.names.get(name_slice) orelse .none;
        self.shadows.append(self.allocator, .{ .name = name_slice, .prev = prev }) catch unreachable;
        self.names.put(name_slice, id) catch unreachable;

        return id;
    }

    /// Returns the innermost visible symbol.
    pub inline fn resolve(self: *const SymbolTable, name_slice: []const u8) ?SymbolId {
        return self.names.get(name_slice);
    }

    /// Get symbol data by ID.
    pub inline fn get(self: *const SymbolTable, id: SymbolId) Symbol {
        return self.symbols.items[@intFromEnum(id)];
    }

    /// Get mutable pointer to symbol data by ID.
    pub inline fn getPtr(self: *SymbolTable, id: SymbolId) *Symbol {
        return &self.symbols.items[@intFromEnum(id)];
    }

    /// All declared symbols as a flat array.
    pub inline fn all(self: *const SymbolTable) []const Symbol {
        return self.symbols.items;
    }

    /// Number of declared symbols.
    pub inline fn count(self: *const SymbolTable) u32 {
        return @intCast(self.symbols.items.len);
    }
};
