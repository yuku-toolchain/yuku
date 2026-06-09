const std = @import("std");
const core = @import("core.zig");

const iterations = 3_000_000;
const progress_every = 250_000;
const oom_every = 20_000;
const oom_max_len = 256;

// the input currently under test, published for the panic handler.
var current_input: []const u8 = &.{};
var current_mode: core.Mode = .{ .lang = .js, .source_type = .module };
var current_iter: u64 = 0;
var current_seed: u64 = 0;

pub const panic = std.debug.FullPanic(onPanic);

fn onPanic(message: []const u8, first_trace_addr: ?usize) noreturn {
    dumpReproducer(message);
    std.debug.defaultPanic(message, first_trace_addr);
}

fn dumpReproducer(message: []const u8) void {
    std.debug.print(
        \\
        \\================= FUZZ FAILURE =================
        \\ message : {s}
        \\ seed    : 0x{x}
        \\ iter    : {d}
        \\ mode    : {s} / {s}
        \\ length  : {d}
        \\
    , .{
        message,                            current_seed,
        current_iter,                       @tagName(current_mode.lang),
        @tagName(current_mode.source_type), current_input.len,
    });

    std.debug.print(" escaped : \"", .{});
    for (current_input) |b| switch (b) {
        '\\' => std.debug.print("\\\\", .{}),
        '"' => std.debug.print("\\\"", .{}),
        '\n' => std.debug.print("\\n", .{}),
        '\r' => std.debug.print("\\r", .{}),
        '\t' => std.debug.print("\\t", .{}),
        0x20...0x21, 0x23...0x5b, 0x5d...0x7e => std.debug.print("{c}", .{b}),
        else => std.debug.print("\\x{x:0>2}", .{b}),
    };
    std.debug.print("\"\n hex     :", .{});
    for (current_input) |b| std.debug.print(" {x:0>2}", .{b});
    std.debug.print("\n===============================================\n", .{});
}

pub fn main() void {
    var gpa_state: std.heap.DebugAllocator(.{}) = .init;
    defer if (gpa_state.deinit() == .leak) std.process.exit(1);
    const gpa = gpa_state.allocator();

    current_seed = @intFromPtr(&gpa_state) *% 0x9e3779b97f4a7c15;
    var prng = std.Random.DefaultPrng.init(current_seed);

    std.debug.print(
        "yuku parser fuzzer: seed 0x{x}, {d} iterations, {d} modes\n",
        .{ current_seed, iterations, core.modes.len },
    );

    // exhaustive oom + invariant pass over the curated corpus before mutating.
    for (core.seeds ++ core.fragments ++ core.regressions) |s| for (core.modes) |m| {
        current_input = s;
        current_mode = m;
        core.check(gpa, s, m);
        core.oomSweep(s, m);
    };

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(gpa);
    var mutator: core.Mutator = .{ .rng = prng.random() };

    var iter: u64 = 0;
    while (iter < iterations) : (iter += 1) {
        current_iter = iter;
        const input = mutator.produce(&buf, gpa) catch {
            // transient oom in the mutator itself is not a finding.
            continue;
        };
        const mode = core.modes[mutator.rng.uintLessThan(usize, core.modes.len)];
        current_input = input;
        current_mode = mode;

        core.check(gpa, input, mode);
        if (iter % oom_every == 0 and input.len <= oom_max_len) core.oomSweep(input, mode);

        if (iter % progress_every == 0 and iter != 0)
            std.debug.print("  {d}/{d} iterations clean\n", .{ iter, iterations });
    }

    std.debug.print("done: {d} iterations, no findings (seed 0x{x})\n", .{ iterations, current_seed });
}
