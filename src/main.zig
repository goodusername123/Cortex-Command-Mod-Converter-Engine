const std = @import("std");

const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

/// This .ini input file:
/// /* foo   */ /* foo2*//*foo3*/
/// DataModule
/// \t/* bar */Description      = Epic /* baz
/// bee */\tSupportedGameVersion = Pre4
///
/// Turns into this .ini output file:
/// // foo foo2 foo3
/// DataModule
/// \tDescription = Epic // bar baz
/// \tSupportedGameVersion = Pre4 // bee
///
/// That output file is internally represented with these AST nodes:
/// [
///     { "comment": "foo foo2 foo3" },
///     { "property": "DataModule", "children": [
///         { "property": "Description", "value": "Epic", "comment": "bar baz" },
///         { "property": "SupportedGameVersion", "value": "Pre4", "comment": "bee" }
///     ]}
/// [
const TokenType = enum {
    // TODO: Maybe merge these as "Comment"
    SingleComment,
    MultiComment,

    Tabs,
    Spaces,
    Equals,
    Word,
};

const Token = struct {
    token_type: TokenType,
    slice: []const u8,
};

fn getToken(slice: *[]const u8, in_multiline_comment: *bool) Token {
    if (in_multiline_comment.*) {
        var i: usize = 0;
        while (i < slice.len) : (i += 1) {
            if (slice.*[i] == '*' and i + 1 < slice.len and slice.*[i + 1] == '/') {
                in_multiline_comment.* = false;
                break;
            }
        }

        const token = Token{ .token_type = .MultiComment, .slice = slice.*[0 .. i + 2] };
        slice.* = slice.*[i + 2 ..];
        return token;
    }

    return switch (slice.*[0]) {
        '/' => {
            return switch (slice.*[1]) {
                '/' => {
                    const token = Token{ .token_type = .SingleComment, .slice = slice.* };
                    slice.* = slice.*[slice.len..];
                    return token;
                },
                '*' => {
                    in_multiline_comment.* = true;

                    var i: usize = 2;
                    while (i < slice.len) : (i += 1) {
                        if (slice.*[i] == '*' and i + 1 < slice.len and slice.*[i + 1] == '/') {
                            in_multiline_comment.* = false;
                            break;
                        }
                    }

                    const token = Token{ .token_type = .MultiComment, .slice = slice.*[0 .. i + 2] };
                    slice.* = slice.*[i + 2 ..];
                    return token;
                },
                else => {
                    const token = Token{ .token_type = .Word, .slice = slice.*[0..] };
                    slice.* = slice.*[1..];
                    return token;
                },
            };
        },
        '\t' => {
            var i: usize = 1;
            for (slice.*[1..]) |character| {
                if (character != '\t') {
                    break;
                }
                i += 1;
            }

            const token = Token{ .token_type = .Tabs, .slice = slice.*[0..i] };
            slice.* = slice.*[i..];
            return token;
        },
        ' ' => {
            var i: usize = 1;
            for (slice.*[1..]) |character| {
                if (character != ' ') {
                    break;
                }
                i += 1;
            }

            const token = Token{ .token_type = .Spaces, .slice = slice.*[0..i] };
            slice.* = slice.*[i..];
            return token;
        },
        '=' => {
            const token = Token{ .token_type = .Equals, .slice = slice.*[0..1] };
            slice.* = slice.*[1..];
            return token;
        },
        else => {
            const token = Token{ .token_type = .Word, .slice = slice.*[0..] };
            slice.* = slice.*[1..];
            return token;
        },
    };
}

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

test "ast" {
    // TODO: Try making gpa const
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = &gpa.allocator();
    // _ = allocator;
    // defer _ = gpa.deinit();

    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try std.fs.realpath("src/test.ini", &path_buf);

    var file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var tokens = ArrayList(Token).init(test_allocator);
    defer tokens.deinit();

    var text = try in_stream.readAllAlloc(test_allocator, std.math.maxInt(usize));
    defer test_allocator.free(text);

    var text_slice: []const u8 = text;

    var token: Token = undefined;
    var in_multiline_comment = false;

    // token = getToken(&text_slice, &in_multiline_comment);
    // try expect(token.token_type == .MultiComment);
    // try expectEqualStrings("/* a */", token.slice);
    // try expectEqualStrings("// b", text_slice);

    // token = getToken(&text_slice, &in_multiline_comment);
    // try expect(token.token_type == .SingleComment);
    // try expectEqualStrings("// b", token.slice);
    // try expectEqualStrings("", text_slice);

    // token = getToken(&text_slice, &in_multiline_comment);
    // try expect(token.token_type == .Tabs);
    // try expectEqualStrings("\t\t", token.slice);
    // try expectEqualStrings("=  =\n", text_slice);

    // token = getToken(&text_slice, &in_multiline_comment);
    // try expect(token.token_type == .Equals);
    // try expectEqualStrings("=", token.slice);
    // try expectEqualStrings("  =\n", text_slice);

    // token = getToken(&text_slice, &in_multiline_comment);
    // try expect(token.token_type == .Spaces);
    // try expectEqualStrings("  ", token.slice);
    // try expectEqualStrings("=\n", text_slice);

    // var line_number: i32 = 1;
    // _ = line_number;

    var lines = std.mem.split(u8, text, "\n");
    while (lines.next()) |line| {
        _ = line;

        token = getToken(&text_slice, &in_multiline_comment);
        try expect(token.token_type == .MultiComment);
        try expectEqualStrings("/* a */", token.slice);
        try expectEqualStrings("// b", text_slice);

        token = getToken(&text_slice, &in_multiline_comment);
        try expect(token.token_type == .SingleComment);
        try expectEqualStrings("// b", token.slice);
        try expectEqualStrings("", text_slice);

        std.log.warn("xd", .{});

        // Move "line" along as ptr/slice and add tokens in loop
        // while (true) {
        //     try tokens.append(getToken(&line, line_number));
        // }

        // line_number += 1;
    }

    // std.log.warn("{d}", .{line_number});
    // std.log.warn("{s} '{s}'", .{ @tagName(tokens.items[0].token_type), tokens.items[0].slice });

    // std.log.warn("{s}", .{@tagName(tokens.items[1].token_type)});
    // std.log.warn("'{s}'", .{tokens.items[1].slice});

    // const x = tokens.items[0];
    // _ = x;
    // const x = tokens.?[0];
    // std.log.warn("{s}", .{x.slice});
    // try expect(1 == 2);
    // try expect(eql(
    //     []Token,
    //     tokens.items,
    //     []Token{{.token_type = .Word, .slice = "xd",}},
    // ));
    // try expect(std.meta.eql(
    //     tokens.items[0],
    //     Token{ .token_type = .Word, .slice = "AddEffect = MOPixel" },
    // ));

    // const ast = AST{
    //     .property = "a",
    //     .value = "b",
    // };
}
