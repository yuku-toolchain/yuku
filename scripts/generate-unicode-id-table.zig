const std = @import("std");

const spec_url = "https://www.unicode.org/Public/17.0.0/ucd/UCD.zip";
const zip_dest = "/tmp/ucd.zip";
const extracted_dir = "/tmp/ucd";

const Codes = std.AutoHashMapUnmanaged(u32, void);

const Kind = enum {
    Start,
    Continue
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    try downloadAndExtractSpec(allocator);

    var id_start_codes, var id_continue_codes = try readSpecToCodes(allocator);

    defer id_start_codes.deinit(allocator);
    defer id_continue_codes.deinit(allocator);

    std.debug.print("{d} {d}", .{id_start_codes.items.len, id_continue_codes.items.len});
}

fn bitsToRootAndLeaf() !struct {
    root: []u32,
    leaf: []u64
} {

}

fn readSpecToCodes(allocator: std.mem.Allocator) !struct {Codes, Codes} {
    const file_path: []const u8 = "DerivedCoreProperties.txt";

    var dir = try std.fs.openDirAbsolute(extracted_dir, .{});
    defer dir.close();

    const content = try dir.readFileAlloc(file_path, allocator, .limited(2 * 1024 * 1024));
    defer allocator.free(content);

    const delim: u8 = '\n';

    var id_start_codes: Codes = .empty;
    var id_continue_codes: Codes = .empty;

    var lines = std.mem.splitScalar(u8, content, delim);

    while (lines.next()) |line| {
        if (line.len == 0 or std.mem.startsWith(u8, line, "#")) {
            continue;
        }

        const kind: Kind = if(std.mem.indexOf(u8, line, "ID_Start")) |_| .Start else if (std.mem.indexOf(u8, line, "ID_Continue")) |_| .Continue else continue;

        const parsed = try parseStartEnd(line) orelse continue;

            for (parsed.start..parsed.end) |c| {
                if(kind == .Start){
                    try id_start_codes.append(allocator, @intCast(c));
                } else if(kind == .Continue){
                    try id_continue_codes.append(allocator, @intCast(c));
                } else {
                    break;
                }
            }
    }

    return .{ id_start_codes, id_continue_codes };
}

const Parsed = struct {
    start: u32,
    end: u32
};

fn parseStartEnd(line: []const u8) !?Parsed {
    const space_index = std.mem.indexOfScalar(u8, line, ' ') orelse line.len;

    const to_parse = line[0..space_index];

    if(std.mem.indexOf(u8, line, "..")) |i| {
        const start = try std.fmt.parseInt(u32, to_parse[0..i], 16);
        const end = try std.fmt.parseInt(u32, to_parse[i + 2 ..], 16);

        return .{.start = start, .end = end + 1};
    } else {
        const x = try std.fmt.parseInt(u32, to_parse, 16);

        return .{.start = x, .end = x + 1};
    }
}

pub fn downloadAndExtractSpec(allocator: std.mem.Allocator) !void {
    var client: std.http.Client = .{ .allocator = allocator };
    defer client.deinit();

    const uri = try std.Uri.parse(spec_url);
    var req = try client.request(.GET, uri, .{ .redirect_behavior = .unhandled, .keep_alive = false });
    defer req.deinit();

    try req.sendBodiless();
    var response = try req.receiveHead(&.{});

    const file = try std.fs.createFileAbsolute(zip_dest, .{});
    defer file.close();
    defer std.fs.deleteFileAbsolute(zip_dest) catch {};

    var file_writer = file.writer(&.{});
    const writer = &file_writer.interface;

    var response_reader_buf: [1024]u8 = undefined;
    const reader = response.reader(&response_reader_buf);
    _ = try reader.streamRemaining(writer);

    try std.fs.deleteTreeAbsolute(extracted_dir);

    try std.fs.makeDirAbsolute(extracted_dir);

    var dir = try std.fs.openDirAbsolute(extracted_dir, .{});
    defer dir.close();

    const zip = try std.fs.openFileAbsolute(zip_dest, .{});
    defer zip.close();

    var zip_reader_buf: [1024]u8 = undefined;
    var zip_reader = zip.reader(&zip_reader_buf);

    try std.zip.extract(dir, &zip_reader, .{});

    std.log.info("Extracted successfully to {s}", .{extracted_dir});
}
