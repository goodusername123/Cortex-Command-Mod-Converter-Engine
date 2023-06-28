const std = @import("std");

// pub fn main()

fn move(line_ptr: *[*]const u8) void {
    line_ptr.* += 1;
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
        move(&line_ptr);
        std.log.debug("{c}", .{line_ptr[0]}); // b
    }
}
