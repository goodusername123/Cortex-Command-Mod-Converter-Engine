const std = @import("std");

const test_allocator = std.testing.allocator;

const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

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
/// That output file is stored roughly like so:
/// {
///     {
///         .property = "DataModule";
///         .comments = { "foo", "foo2", "foo3" };
///         .children = {
///             {
///                 .property = "Description";
///                 .value = "Epic";
///                 .comments = { "bar", "baz" };
///             },
///             {
///                 .property = "SupportedGameVersion";
///                 .value = "Pre4";
///                 .comments = { "bee" };
///             }
///         }
///     }
/// }
///
/// The actual way it is stored tries to use very few memory allocations:
///
/// // Not using ArrayList, since we don't want padding
/// nodes: MultiArrayList(Node) = {
///     {
///         .property = slice of Sentence "DataModule";
///         .comments = slice of Comments "foo", "foo2", "foo3";
///         .children = slice of Nodes 1 and 2;
///     },
///     {
///         .property = slice of Sentence "Description";
///         .value = slice of Sentence "Epic";
///         .comments = slice of Comments "bar", "baz";
///     },
///     {
///         .property = slice of Sentence "SupportedGameVersion";
///         .value = slice of Sentence "Pre4";
///         .comments = slice of Comment "bee";
///     }
/// };
///
/// // The Node at index 0 owns index 0, 1 and 2
/// comments: ArrayList([]const u8) = {
///     "foo", "foo2", "foo3", "bar", "baz", "bee"
/// };
const Token = struct {
    type: TokenType,
    slice: []const u8,

    const TokenType = enum {
        Comment,
        Tabs,
        Spaces,
        Equals,
        Sentence,
    };
};

const Node = struct {
    property: ?[]const u8 = null,
    value: ?[]const u8 = null,
    comments: ?[][]const u8 = null,
    children: ?[]Node = null,
};

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
    // const gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // defer _ = gpa.deinit();
    // const allocator = &gpa.allocator();
    // _ = allocator;

    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try std.fs.realpath("src/test.ini", &path_buf);

    var file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var tokens = std.ArrayList(Token).init(test_allocator);
    defer tokens.deinit();

    var text = try in_stream.readAllAlloc(test_allocator, std.math.maxInt(usize));
    defer test_allocator.free(text);

    var text_slice: []const u8 = text;

    var in_multiline_comment = false;

    const NodeList = std.MultiArrayList(Node);
    var nodes = NodeList{};
    defer nodes.deinit(test_allocator);

    var comments = std.ArrayList([]const u8).init(test_allocator);
    defer comments.deinit();

    var lines = std.mem.split(u8, text, "\n");
    while (lines.next()) |line| {
        _ = line;
        var node = Node{};

        const SeenStates = enum {
            start,
            property,
            value,
        };

        var seen: SeenStates = .start;
        _ = seen;

        while (text_slice.len > 0) {
            const token = getToken(&text_slice, &in_multiline_comment);

            if (token.type == .Sentence) {
                std.log.warn("{s}", .{token.slice});
            }
        }

        try nodes.append(test_allocator, node);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Tabs);
        // try expectEqualStrings("\t", token.slice);
        // try expectEqualStrings("w xy = /v z /* a b */// c d ", text_slice);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Sentence);
        // try expectEqualStrings("w xy", token.slice);
        // try expectEqualStrings(" = /v z /* a b */// c d ", text_slice);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Spaces);
        // try expectEqualStrings(" ", token.slice);
        // try expectEqualStrings("= /v z /* a b */// c d ", text_slice);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Equals);
        // try expectEqualStrings("=", token.slice);
        // try expectEqualStrings(" /v z /* a b */// c d ", text_slice);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Spaces);
        // try expectEqualStrings(" ", token.slice);
        // try expectEqualStrings("/v z /* a b */// c d ", text_slice);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Sentence);
        // try expectEqualStrings("/v z", token.slice);
        // try expectEqualStrings(" /* a b */// c d ", text_slice);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Spaces);
        // try expectEqualStrings(" ", token.slice);
        // try expectEqualStrings("/* a b */// c d ", text_slice);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Comment);
        // try expectEqualStrings("/* a b */", token.slice);
        // try expectEqualStrings("// c d ", text_slice);

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Comment);
        // try expectEqualStrings("// c d ", token.slice);
        // try expectEqualStrings("", text_slice);

        // std.log.warn("{d}", .{text_slice.len});

        // token = getToken(&text_slice, &in_multiline_comment);
        // try expect(token.type == .Comment);
        // try expectEqualStrings("// c d ", token.slice);
        // try expectEqualStrings("", text_slice);

        std.log.warn("xd", .{});

        // while (true) {
        //     try tokens.append(getToken(&line, line_number));
        // }
    }

    // std.log.warn("{d}", .{line_number});
    // std.log.warn("{s} '{s}'", .{ @tagName(tokens.items[0].type), tokens.items[0].slice });

    // std.log.warn("{s}", .{@tagName(tokens.items[1].type)});
    // std.log.warn("'{s}'", .{tokens.items[1].slice});

    // const x = tokens.items[0];
    // _ = x;
    // const x = tokens.?[0];
    // std.log.warn("{s}", .{x.slice});
    // try expect(1 == 2);
    // try expect(eql(
    //     []Token,
    //     tokens.items,
    //     []Token{{.type = .Sentence, .slice = "xd",}},
    // ));
    // try expect(std.meta.eql(
    //     tokens.items[0],
    //     Token{ .type = .Sentence, .slice = "AddEffect = MOPixel" },
    // ));

    // const node = Node{
    //     .property = "a",
    //     .value = "b",
    // };
}

fn getToken(slice: *[]const u8, in_multiline_comment: *bool) Token {
    if (in_multiline_comment.*) {
        var i: usize = 0;
        while (i < slice.len) : (i += 1) {
            if (slice.*[i] == '*' and i + 1 < slice.len and slice.*[i + 1] == '/') {
                in_multiline_comment.* = false;
                break;
            }
        }

        const token = Token{ .type = .Comment, .slice = slice.*[0 .. i + 2] };
        slice.* = slice.*[i + 2 ..];
        return token;
    }

    return switch (slice.*[0]) {
        '/' => {
            return switch (slice.*[1]) {
                '/' => {
                    const token = Token{ .type = .Comment, .slice = slice.* };
                    slice.* = slice.*[slice.len..];
                    return token;
                },
                '*' => {
                    in_multiline_comment.* = true;

                    var i: usize = 2;
                    // TODO: Either use while-loops or for-loops everywhere in this function
                    while (i < slice.len) : (i += 1) {
                        if (slice.*[i] == '*' and i + 1 < slice.len and slice.*[i + 1] == '/') {
                            in_multiline_comment.* = false;
                            i += 2;
                            break;
                        }
                    }

                    const token = Token{ .type = .Comment, .slice = slice.*[0..i] };
                    slice.* = slice.*[i..];
                    return token;
                },
                else => {
                    // std.log.warn("foo", .{});

                    // A Sentence ends with a word, or the start of a comment
                    var end_index: usize = 2;
                    var sentence_end_index: usize = end_index;
                    while (end_index < slice.len) : (end_index += 1) {
                        if (slice.*[end_index] == '=' or (slice.*[end_index] == '/' and end_index + 1 < slice.len and slice.*[end_index + 1] == '*')) {
                            break;
                        }
                        if (slice.*[end_index] != ' ') {
                            sentence_end_index = end_index + 1;
                        }
                    }

                    const token = Token{ .type = .Sentence, .slice = slice.*[0..sentence_end_index] };
                    slice.* = slice.*[sentence_end_index..];
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

            const token = Token{ .type = .Tabs, .slice = slice.*[0..i] };
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

            const token = Token{ .type = .Spaces, .slice = slice.*[0..i] };
            slice.* = slice.*[i..];
            return token;
        },
        '=' => {
            const token = Token{ .type = .Equals, .slice = slice.*[0..1] };
            slice.* = slice.*[1..];
            return token;
        },
        else => {
            // std.log.warn("bar", .{});

            // A Sentence ends with a word, or the start of a comment
            var end_index: usize = 1;
            var sentence_end_index: usize = end_index;
            while (end_index < slice.len) : (end_index += 1) {
                if (slice.*[end_index] == '=' or (slice.*[end_index] == '/' and end_index + 1 < slice.len and slice.*[end_index + 1] == '*')) {
                    break;
                }
                if (slice.*[end_index] != ' ') {
                    sentence_end_index = end_index + 1;
                }
            }

            const token = Token{ .type = .Sentence, .slice = slice.*[0..sentence_end_index] };
            slice.* = slice.*[sentence_end_index..];
            return token;
        },
    };
}
