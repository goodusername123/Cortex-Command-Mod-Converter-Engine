const std = @import("std");

// /* foo   */ /* foo2*//*foo3*/
// DataModule
// \t/* bar */Description      = Epic /* baz
// bee */\tSupportedGameVersion = Pre4
//
// // foo foo2 foo3
// DataModule
// \tDescription = Epic // bar baz
// \tSupportedGameVersion = Pre4 // bee
//
// [
//     { "comment": "foo foo2 foo3" },
//     { "property": "DataModule", "children": [
//         { "property": "Description", "value": "Epic", "comment": "bar baz" },
//         { "property": "SupportedGameVersion", "value": "Pre4", "comment": "bee" }
//     ]}
// [

const TokenType = enum {
    Comment,
    Tabs,
    Spaces,
    Equals,
    Newlines,
    Word,
};

const Token = struct {
    token_type: TokenType,
    value: []const u8,
};

// const AST = struct {
//     property: []const u8,
//     value: []const u8,
//     comment: ?[]const u8 = null,
//     children: ?*AST = null,
// };

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!
}

// test "simple test" {
//     var list = std.ArrayList(i32).init(std.testing.allocator);
//     defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
//     try list.append(42);
//     try std.testing.expectEqual(@as(i32, 42), list.pop());
// }

// fn add_line(ast: *AST, line: []const u8, allocator: ) void {
//     _ = line;
//     _ = ast;
// }

test "ast" {
    // TODO: Try making gpa const
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator();
    _ = allocator;
    defer _ = gpa.deinit();

    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try std.fs.realpath("tests/ini_test_files/token_and_cst/simple/in.ini", &path_buf);

    var file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    // TODO: Replace 4242 with something else
    var buf: [4242]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var iter = std.mem.split(u8, line, "=");
        var count: usize = 0;
        while (iter.next()) |token| : (count += 1) {
            std.log.warn("{d}: '{s}'", .{ count, token });
        }
        // try std.testing.expectEqualStrings("AddEffect = MOPixel", line);
        // std.log.warn("{s}", .{line});
    }

    // const ast = AST{
    //     .property = "a",
    //     .value = "b",
    // };
}
