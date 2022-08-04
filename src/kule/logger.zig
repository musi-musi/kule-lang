const std = @import("std");
const compiler = @import("compiler.zig");

const Source = compiler.Source;
const SourceLocation = compiler.SourceLocation;

pub const LogType = enum {
    err,
    info,
};

const ansi = struct {
    const clear = "\x1b[0m";
    const bold = "\x1b[1m";
    const faint = "\x1b[2m";
    const black = "\x1b[30m";
    const red = "\x1b[31m";
    const green = "\x1b[32m";
    const yellow = "\x1b[33m";
    const blue = "\x1b[34m";
    const magenta = "\x1b[35m";
    const cyan = "\x1b[36m";
    const white = "\x1b[37m";
    const bright_black = "\x1b[90m";
    const bright_red = "\x1b[91m";
    const bright_green = "\x1b[92m";
    const bright_yellow = "\x1b[93m";
    const bright_blue = "\x1b[94m";
    const bright_magenta = "\x1b[95m";
    const bright_cyan = "\x1b[96m";
    const bright_white = "\x1b[97m";
    const clear_color = "\x1b[39m";
};

fn writePrefix(writer: anytype, log_type: LogType,) !void {
    switch (log_type) {
        .err => try writer.print("[{s}error{s}]", .{ansi.bright_red, ansi.clear_color}),
        .info => try writer.print("[{s}info{s}]", .{ansi.bright_blue, ansi.clear_color}),
    }
}

fn panic(e: std.fs.File.WriteError) noreturn {
    std.debug.panic("error writing to stdout: {s}", .{ @errorName(e)});
}

pub fn err(comptime format: []const u8, args: anytype) void {
    log(.err, format, args);
}

pub fn info(comptime format: []const u8, args: anytype) void {
    log(.info, format, args);
}


pub fn log(log_type: LogType, comptime format: []const u8, args: anytype) void {
    logRaw(log_type, format, args) catch |e| panic(e);
}

fn logRaw(log_type: LogType, comptime format: []const u8, args: anytype) !void {
    const w = std.io.getStdErr().writer();
    try writePrefix(w, log_type);
    try w.writeByte(' ');
    try w.print(format, args);
    try w.writeByte('\n');
}

pub fn sourceError(source: Source, token: []const u8, comptime format: []const u8, args: anytype) void {
    sourceLog(.err, source, token, format, args);
}

pub fn sourceInfo(source: Source, token: []const u8, comptime format: []const u8, args: anytype) void {
    sourceLog(.info, source, token, format, args);
}

pub fn sourceLog(log_type: LogType, source: Source, token: []const u8, comptime format: []const u8, args: anytype) void {
    sourceLogRaw(log_type, source, token, format, args) catch |e| panic(e);
}

fn sourceLogRaw(log_type: LogType, source: Source, token: []const u8, comptime format: []const u8, args: anytype) !void {
    const w = std.io.getStdErr().writer();
    try writePrefix(w, log_type);
    const source_name: []const u8 = source.name orelse "???";
    const location = source.tokenLocation(token) orelse std.debug.panic(
        "token {s} is not in source [{s}]:\n\n{s}",
        .{token, source_name, source.text}
    );
    try w.print(" {s}:{d}:{d} ", .{ source_name, location.line + 1, location.column + 1});
    switch (log_type) {
        .err => try w.writeAll(ansi.yellow),
        .info => try w.writeAll(ansi.cyan),
    }
    try w.print(format, args);
    try w.writeAll(ansi.clear);
    try w.writeByte('\n');
    const line = source.lines[location.line];
    var tab_count: usize = 0;
    try w.print("{s}\n", .{line});
    if (token.len > 0) {
        try w.writeAll(ansi.green);
        for (line[0..location.column]) |c| {
            if (c == '\t') {
                tab_count += 1;
            }
        }
        try w.writeByteNTimes('\t', tab_count);
        try w.writeByteNTimes(' ', location.column - tab_count);
        try w.writeByteNTimes('^', token.len);
        try w.writeAll(ansi.clear);
    }
    try w.writeByte('\n');
}