const std = @import("std");

// const test_allocator = std.testing.allocator;

// const expect = std.testing.expect;
// const expectEqualStrings = std.testing.expectEqualStrings;

/// This .ini input file:
/// /* foo1   */ /* foo2*//*foo3*/
/// DataModule
/// \tSupportedGameVersion = Pre4
/// \t/* bar */IconFile      = ContentFile /* baz
/// bee */\t\tFilePath=foo.png
/// \tDescription = lol
///
/// Turns into this .ini output file:
/// // foo1 foo2 foo3
/// DataModule
/// \tSupportedGameVersion = Pre4
/// \tIconFile = ContentFile // bar baz
/// \t\tFilePath = foo.png // bee
/// \tDescription = lol
///
/// That output file is stored roughly like so:
/// {
///     {
///         .comments = { "foo1", "foo2", "foo3" };
///     },
///     {
///         .property = "DataModule";
///         .children = {
///             {
///                 .property = "SupportedGameVersion";
///                 .value = "Pre4";
///             },
///             {
///                 .property = "IconFile";
///                 .value = "ContentFile";
///                 .comments = { "bar", "baz" };
///                 .children = {
///                     .property = "FilePath";
///                     .value = "foo.png";
///                     .comments = { "bee" };
///                 }
///             },
///             {
///                 .property = "Description";
///                 .value = "lol";
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
///         .comments = slice of Comments "foo1", "foo2", "foo3" from comments;
///     },
///     {
///         .property = slice of Sentence "DataModule";
///         .children = slice of child Node indices 2, 3 and 5 from childIndices;
///     },
///     {
///         .property = slice of Sentence "SupportedGameVersion";
///         .value = slice of Sentence "Pre4";
///     },
///     {
///         .property = slice of Sentence "IconFile";
///         .value = slice of Sentence "ContentFile";
///         .comments = slice of Comments "bar" and "baz" from comments;
///         .children = slice of child Node index 4 from childIndices;
///     },
///     {
///         .property = slice of Sentence "FilePath";
///         .value = slice of Sentence "foo.png";
///         .comments = slice of Comment "bee" from comments;
///     },
///     {
///         .property = slice of Sentence "Description";
///         .value = slice of Sentence "lol";
///     }
/// };
///
/// // The Node at index 0 owns indices 0, 1 and 2
/// comments: ArrayList([]const u8) = {
///     "foo1", "foo2", "foo3", "bar", "baz", "bee"
/// };
///
/// // The Node at index 1 owns the values 2, 3 and 5
/// childIndices: ArrayList(u32) = {
///     2, 3, 5, 4
/// };
///
/// // The Node at index 1 owns the value 3
/// // This array is created by looping over all of the file's text,
/// // and having an ArrayList stack of the line numbers who own which indentation depth
/// childIndexCounts: ArrayList(u32) = {
///     0, 3, 0, 1, 0, 0
/// };
const Token = struct {
    type: Type,
    slice: []const u8,

    const Type = enum {
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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = gpa.allocator();

    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const path = try std.fs.realpath("src/test.ini", &path_buf);

    var file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var text = try in_stream.readAllAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(text);

    var in_multiline_comment = false;

    const NodeList = std.MultiArrayList(Node);
    var nodes = NodeList{};
    defer nodes.deinit(allocator);

    var comments = std.ArrayList([]const u8).init(allocator);
    defer comments.deinit();

    var lines = std.mem.split(u8, text, "\n");
    while (lines.next()) |line| {
        var line_slice: []const u8 = line;

        // std.debug.print("'{s}'\n", .{line});

        var node = Node{};

        const States = enum {
            Start,
            Property,
            Equals,
            Value,
        };

        var seen: States = .Start;

        while (line_slice.len > 0) {
            const token = getToken(&line_slice, &in_multiline_comment);

            std.debug.print("'{s}' {}\n", .{ token.slice, token.type });

            if (seen == .Start and token.type == .Sentence) {
                node.property = token.slice;
                seen = .Property;
            }
            if (seen == .Property and token.type == .Equals) {
                seen = .Equals;
            }
            if (seen == .Equals and token.type == .Sentence) {
                node.value = token.slice;
                seen = .Value;
            }
            if (token.type == .Comment) {
                try comments.append(token.slice);
            }
            if (token.type == .Tabs) {}
        }

        try nodes.append(allocator, node);
    }

    // Print nodes
    {
        std.debug.print("{}\n", .{nodes});
        var i: usize = 0;
        while (i < nodes.len) : (i += 1) {
            const node = nodes.get(i);
            std.debug.print("{}\n", .{node});
        }
    }

    // Print comments
    std.debug.print("{}\n", .{comments});
    var i: usize = 0;
    while (i < comments.items.len) : (i += 1) {
        const comment = comments.items[i];
        std.debug.print("'{s}'\n", .{comment});
    }
}

test "ast" {
    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Tabs);
    // try expectEqualStrings("\t", token.slice);
    // try expectEqualStrings("w xy = /v z /* a b */// c d ", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Sentence);
    // try expectEqualStrings("w xy", token.slice);
    // try expectEqualStrings(" = /v z /* a b */// c d ", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Spaces);
    // try expectEqualStrings(" ", token.slice);
    // try expectEqualStrings("= /v z /* a b */// c d ", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Equals);
    // try expectEqualStrings("=", token.slice);
    // try expectEqualStrings(" /v z /* a b */// c d ", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Spaces);
    // try expectEqualStrings(" ", token.slice);
    // try expectEqualStrings("/v z /* a b */// c d ", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Sentence);
    // try expectEqualStrings("/v z", token.slice);
    // try expectEqualStrings(" /* a b */// c d ", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Spaces);
    // try expectEqualStrings(" ", token.slice);
    // try expectEqualStrings("/* a b */// c d ", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Comment);
    // try expectEqualStrings("/* a b */", token.slice);
    // try expectEqualStrings("// c d ", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Comment);
    // try expectEqualStrings("// c d ", token.slice);
    // try expectEqualStrings("", line_slice);

    // token = getToken(&line_slice, &in_multiline_comment);
    // try expect(token.type == .Comment);
    // try expectEqualStrings("// c d ", token.slice);
    // try expectEqualStrings("", line_slice);
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
