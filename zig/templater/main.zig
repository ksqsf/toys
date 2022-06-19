const std = @import("std");
const fs = std.fs;
const json = std.json;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

const MAX_FILE: usize = 4 * 1024 * 1024 * 1024;
const MAX_VARNAME: usize = 256;
const BUFSIZE: usize = 4096;

const Context = std.StringHashMap([]const u8);

pub fn main() !void {
    defer _ = gpa.deinit();
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    if (args.len < 2) {
        try help();
        std.process.exit(0);
    }

    var ctx = Context.init(alloc);
    defer ctx.deinit();

    // Add default argument: DIRBASE
    var buf: [1024]u8 = undefined;
    var cwd = try std.os.getcwd(&buf);
    try ctx.put("DIRBASE", fs.path.basename(cwd));

    // Add user-provided arguments
    for (args[2..]) |pair| {
        if (std.mem.indexOf(u8, pair, "=")) |eq_pos| {
            var key = pair[0..eq_pos];
            var val = pair[eq_pos + 1 ..];
            try ctx.put(key, val);
        }
    }

    try doTemplate(args[1], &ctx);
}

fn getTemplaterDir() ![]u8 {
    const state = struct {
        var buf: [2048]u8 = undefined;
        var result: ?[]u8 = null;
    };
    if (state.result == null) {
        if (std.os.getenv("HOME")) |home| {
            state.result = try std.fmt.bufPrint(&state.buf, "{s}/.config/templater", .{home});
        } else {
            return error.CannotGetHome;
        }
    }
    return state.result.?;
}

fn help() !void {
    var stdout = std.io.getStdOut().writer();
    try stdout.print("USAGE: templater TEMPLATE_NAME\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("Available templates:\n", .{});
    const templaterDir = try getTemplaterDir();
    var dir: fs.Dir = fs.openDirAbsolute(templaterDir, .{}) catch |err| switch (err) {
        error.FileNotFound => {
            std.log.warn("You don't have the templater directory: {s}\n", .{templaterDir});
            return err;
        },
        else => return err,
    };
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        try stdout.print("{s}\n", .{entry.name});
    }
}

const Template = struct { files: [][]u8, args: [][]u8 };

fn doTemplate(template: []u8, ctx: *const Context) !void {
    std.log.info("Processing template {s}...", .{template});
    const template_dir = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ getTemplaterDir(), template });
    defer alloc.free(template_dir);

    // Read template info
    const info_path = try std.fmt.allocPrint(alloc, "{s}/info.json", .{template_dir});
    defer alloc.free(info_path);
    var info_file = fs.openFileAbsolute(info_path, .{}) catch |err| switch (err) {
        error.FileNotFound => {
            std.log.err("Template {s} does not exist", .{template});
            return err;
        },
        else => return err,
    };
    var info_contents = try info_file.readToEndAlloc(alloc, MAX_FILE);
    defer alloc.free(info_contents);
    var tokens = json.TokenStream.init(info_contents);
    var info: Template = try json.parse(Template, &tokens, .{
        .allocator = alloc,
        .ignore_unknown_fields = true,
        .allow_trailing_data = true,
    });
    defer json.parseFree(Template, info, .{.allocator = alloc});

    // Check if arguments are provided.
    var vars_not_found = std.ArrayList([]u8).init(alloc);
    defer vars_not_found.deinit();
    for (info.args) |variable| {
        if (ctx.get(variable) == null) {
            try vars_not_found.append(variable);
        }
    }
    if (vars_not_found.items.len > 0) {
        var msg = try std.mem.join(alloc, ", ", vars_not_found.items);
        std.log.err("Missing values for arguments: {s}", .{msg});
        return error.MissingArguments;
    }

    // Process templated files one by one
    for (info.files) |filename| {
        std.log.info("Processing file {s}...", .{filename});
        var filepath = try std.fmt.allocPrint(alloc, "{s}/{s}", .{ template_dir, filename });
        defer alloc.free(filepath);
        doFile(filename, filepath, ctx) catch |err| {
            std.log.err("Failed to use file {s}: {s}", .{ filename, err });
            continue;
        };
    }
}

fn doFile(filename: []u8, filepath: []u8, ctx: *const Context) !void {
    var cwd = fs.cwd();
    var infile = try fs.openFileAbsolute(filepath, .{});
    defer infile.close();
    var outfile = try cwd.createFile(filename, .{});
    defer outfile.close();

    // Read the entire infile
    const infile_metadata = try infile.metadata();
    const infile_size = @intCast(usize, infile_metadata.size());
    var buf = try alloc.alloc(u8, infile_size);
    defer alloc.free(buf);
    _ = try infile.readAll(buf);

    // Split by @@
    var iter = std.mem.split(u8, buf, "@@");

    // Write out things...
    var inside_varname = false;
    while (iter.next()) |part| {
        if (inside_varname) {
            var val = ctx.get(part).?;
            try outfile.writeAll(val);
        } else {
            try outfile.writeAll(part);
        }
        inside_varname = !inside_varname;
    }
}
