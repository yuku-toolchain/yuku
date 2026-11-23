const std = @import("std");

pub fn Result(comptime Return: type) type {
    const Payload = @typeInfo(Return).error_union.payload;
    const is_optional = @typeInfo(Payload) == .optional;
    const Value = if (is_optional) @typeInfo(Payload).optional.child else Payload;
    const Bits = if (@sizeOf(Value) <= 4) u32 else u64;
    comptime std.debug.assert(@sizeOf(Value) <= 8);

    return packed struct(if (Bits == u32) u64 else u128) {
        bits: Bits = 0,
        some: bool = false,
        _reserved: u15 = 0,
        err: u16 = 0,
        _wide: if (Bits == u32) u0 else u32 = 0,

        const Self = @This();

        pub inline fn of(result: Return) Self {
            const payload = result catch |err| return .{ .err = @intFromError(err) };
            if (!is_optional) return .{ .bits = toBits(payload), .some = true };
            return if (payload) |value| .{ .bits = toBits(value), .some = true } else .{};
        }

        pub inline fn get(self: Self) Return {
            if (self.err != 0) return @errorCast(@errorFromInt(self.err));
            if (!is_optional) return fromBits(self.bits);
            var value: ?Value = null;
            if (self.some) value = fromBits(self.bits);
            return value;
        }

        inline fn toBits(value: Value) Bits {
            return switch (@typeInfo(Value)) {
                .@"enum" => @backingInt(value),
                .int => value,
                .bool => @intFromBool(value),
                .@"struct" => |info| bits: {
                    var bits: Bits = 0;
                    comptime var shift = 0;
                    inline for (info.field_names, info.field_types) |name, Field| {
                        bits |= @as(Bits, @field(value, name)) << shift;
                        shift += @bitSizeOf(Field);
                    }
                    break :bits bits;
                },
                else => @compileError("no register form for " ++ @typeName(Value)),
            };
        }

        inline fn fromBits(bits: Bits) Value {
            return switch (@typeInfo(Value)) {
                .@"enum" => @fromBackingInt(bits),
                .int => @intCast(bits),
                .bool => bits != 0,
                .@"struct" => |info| value: {
                    var value: Value = undefined;
                    comptime var shift = 0;
                    inline for (info.field_names, info.field_types) |name, Field| {
                        @field(value, name) = @truncate(bits >> shift);
                        shift += @bitSizeOf(Field);
                    }
                    break :value value;
                },
                else => @compileError("no register form for " ++ @typeName(Value)),
            };
        }
    };
}

pub inline fn inRegister(comptime body: anytype, args: anytype) ReturnOf(body) {
    const Call = Shim(body);
    return switch (args.len) {
        1 => Call.call1(args[0]),
        2 => Call.call2(args[0], args[1]),
        3 => Call.call3(args[0], args[1], args[2]),
        4 => Call.call4(args[0], args[1], args[2], args[3]),
        5 => Call.call5(args[0], args[1], args[2], args[3], args[4]),
        6 => Call.call6(args[0], args[1], args[2], args[3], args[4], args[5]),
        else => @compileError("inRegister takes one to six arguments"),
    }.get();
}

fn ReturnOf(comptime body: anytype) type {
    return @typeInfo(@TypeOf(body)).@"fn".return_type.?;
}

fn Shim(comptime body: anytype) type {
    const Packed = Result(ReturnOf(body));
    const Param = struct {
        fn at(comptime index: usize) type {
            return @typeInfo(@TypeOf(body)).@"fn".param_types[index].?;
        }
    };
    return struct {
        fn call1(a: Param.at(0)) Packed {
            return .of(@call(.always_inline, body, .{a}));
        }

        fn call2(a: Param.at(0), b: Param.at(1)) Packed {
            return .of(@call(.always_inline, body, .{ a, b }));
        }

        fn call3(a: Param.at(0), b: Param.at(1), c: Param.at(2)) Packed {
            return .of(@call(.always_inline, body, .{ a, b, c }));
        }

        fn call4(a: Param.at(0), b: Param.at(1), c: Param.at(2), d: Param.at(3)) Packed {
            return .of(@call(.always_inline, body, .{ a, b, c, d }));
        }

        fn call5(
            a: Param.at(0),
            b: Param.at(1),
            c: Param.at(2),
            d: Param.at(3),
            e: Param.at(4),
        ) Packed {
            return .of(@call(.always_inline, body, .{ a, b, c, d, e }));
        }

        fn call6(
            a: Param.at(0),
            b: Param.at(1),
            c: Param.at(2),
            d: Param.at(3),
            e: Param.at(4),
            f: Param.at(5),
        ) Packed {
            return .of(@call(.always_inline, body, .{ a, b, c, d, e, f }));
        }
    };
}

const testing = std.testing;

const Failure = error{ Refused, Exhausted };
const Id = enum(u32) { first, last = std.math.maxInt(u32), _ };
const Range = struct { start: u32, len: u32 };

fn pick(id: Id, fail: bool) Failure!?Id {
    if (fail) return error.Refused;
    return if (id == .first) null else id;
}

fn span(start: u32, len: u32, fail: bool) Failure!Range {
    if (fail) return error.Exhausted;
    return .{ .start = start, .len = len };
}

fn isLast(id: Id) Failure!bool {
    return id == .last;
}

test "a result in a register is the result of the body" {
    try testing.expectEqual(@as(?Id, null), try inRegister(pick, .{ Id.first, false }));
    try testing.expectEqual(@as(?Id, .last), try inRegister(pick, .{ Id.last, false }));
    try testing.expectError(error.Refused, inRegister(pick, .{ Id.last, true }));

    const range = try inRegister(span, .{ 7, std.math.maxInt(u32), false });
    try testing.expectEqual(Range{ .start = 7, .len = std.math.maxInt(u32) }, range);
    try testing.expectError(error.Exhausted, inRegister(span, .{ 7, 9, true }));

    try testing.expect(try inRegister(isLast, .{Id.last}));
    try testing.expect(!try inRegister(isLast, .{Id.first}));
}

test "a result is no wider than two registers" {
    try testing.expectEqual(8, @sizeOf(Result(Failure!?Id)));
    try testing.expectEqual(8, @sizeOf(Result(Failure!bool)));
    try testing.expectEqual(16, @sizeOf(Result(Failure!Range)));
}
