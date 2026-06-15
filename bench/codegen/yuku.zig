//! Standalone codegen benchmark for yuku's `src/parser/codegen/`.
//!
//! Parses a file once, then runs `codegen.print` in a self-calibrating
//! loop, timing the codegen step only (the parse cost is excluded). This
//! isolates the code generator so the result is directly comparable to the
//! oxc_codegen harness in ./oxc, which uses the identical methodology.
//!
//! Usage: yuku-codegen-bench <file> [min_seconds]
//! Output: one JSON object on stdout.

const std = @import("std");
const parser = @import("parser");

fn nowNs(io: std.Io) i128 {
    return @intCast(std.Io.Timestamp.now(io, .awake).nanoseconds);
}

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const gpa = init.gpa;
    const arena = init.arena.allocator();

    const argv = try init.minimal.args.toSlice(arena);
    var path: ?[]const u8 = null;
    var min_seconds: f64 = 3.0;
    var dump_path: ?[]const u8 = null;
    {
        var i: usize = 1;
        while (i < argv.len) : (i += 1) {
            const arg = argv[i];
            if (std.mem.eql(u8, arg, "--dump")) {
                i += 1;
                if (i < argv.len) dump_path = argv[i];
            } else if (path == null) {
                path = arg;
            } else {
                min_seconds = try std.fmt.parseFloat(f64, arg);
            }
        }
    }
    if (path == null) {
        std.debug.print("usage: yuku-codegen-bench <file> [min_seconds] [--dump <out>]\n", .{});
        std.process.exit(2);
    }
    const min_ns: i128 = @intFromFloat(min_seconds * std.time.ns_per_s);
    const file_path = path.?;

    const source = try std.Io.Dir.cwd().readFileAlloc(io, file_path, gpa, .unlimited);
    defer gpa.free(source);

    // Parse once, outside the measured loop.
    var tree = try parser.parse(gpa, source, .{
        .lang = parser.ast.Lang.fromPath(file_path),
        .source_type = parser.ast.SourceType.fromPath(file_path),
    });
    defer tree.deinit();

    const options: parser.codegen.Options = .{
        .format = .pretty,
        // oxc's default Codegen emits no comments; match that so the
        // comparison measures core code generation, not comment passthrough.
        .comments = .none,
    };

    // Allocate codegen output through system malloc, matching oxc's global
    // allocator (and yuku's own FFI usage), so the comparison isolates code
    // generation rather than allocator choice.
    const cg = std.heap.c_allocator;

    // Warm up and capture output size on the first run.
    var output_bytes: usize = 0;
    {
        const r = try parser.codegen.print(cg, &tree, options);
        output_bytes = r.code.len;
        // Optional: dump generated code for output-parity verification.
        if (dump_path) |dp| {
            try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = dp, .data = r.code });
        }
        r.deinit(cg);
    }

    var best_ns: i128 = std.math.maxInt(i128);
    var iters: u64 = 0;
    const loop_start = nowNs(io);
    while (nowNs(io) - loop_start < min_ns) {
        const t0 = nowNs(io);
        const r = try parser.codegen.print(cg, &tree, options);
        const elapsed = nowNs(io) - t0;
        r.deinit(cg);
        if (elapsed < best_ns) best_ns = elapsed;
        iters += 1;
    }
    const total_ns = nowNs(io) - loop_start;
    const mean_ns: i128 = @divTrunc(total_ns, @as(i128, iters));

    const bytes_f: f64 = @floatFromInt(source.len);
    const mean_mb_s = bytes_f / (@as(f64, @floatFromInt(mean_ns)) / std.time.ns_per_s) / (1024.0 * 1024.0);
    const best_mb_s = bytes_f / (@as(f64, @floatFromInt(best_ns)) / std.time.ns_per_s) / (1024.0 * 1024.0);

    const stdout = std.Io.File.stdout();
    var buf: [4096]u8 = undefined;
    var fw = stdout.writer(io, &buf);
    try fw.interface.print(
        \\{{"tool":"yuku","file":"{s}","input_bytes":{d},"output_bytes":{d},"iters":{d},"mean_ns":{d},"best_ns":{d},"mean_mb_s":{d:.2},"best_mb_s":{d:.2}}}
    ++ "\n", .{
        std.fs.path.basename(file_path),
        source.len,
        output_bytes,
        iters,
        mean_ns,
        best_ns,
        mean_mb_s,
        best_mb_s,
    });
    try fw.flush();
}
