const std = @import("std");

fn move_ptr(line_ptr: *[*]const u8) void {
    line_ptr.* += 1;
}

fn move_slice(line_slice: *[]const u8) void {
    line_slice.* = line_slice.*[1..];
}

pub fn main() void {
    {
        const line = [_]u8{ 'a', 'b', 'c' };
        var line_ptr: [*]const u8 = &line;
        line_ptr += 1;
        std.log.debug("{c}", .{line_ptr[0]}); // b
    }
    {
        var line = "abc";
        var line_ptr: [*]const u8 = line;
        line_ptr += 1;
        std.log.debug("{c}", .{line_ptr[0]}); // b
    }
    {
        var line = "abc";
        var line_ptr: [*]const u8 = line;
        move_ptr(&line_ptr);
        std.log.debug("{c}", .{line_ptr[0]}); // b
    }
    {
        const line = [_]u8{ 'a', 'b', 'c' };
        var line_slice: []const u8 = &line;
        line_slice = line_slice[1..];
        std.log.debug("{c}", .{line_slice[0]}); // b
    }
    {
        const line = [_]u8{ 'a', 'b', 'c' };
        var line_slice: []const u8 = &line;
        move_slice(&line_slice);
        std.log.debug("{c}", .{line_slice[0]}); // b
    }
}
