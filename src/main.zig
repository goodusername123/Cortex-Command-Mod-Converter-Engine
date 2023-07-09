const std = @import("std");
const ArrayList = std.ArrayList;
const ArenaAllocator = std.heap.ArenaAllocator;
const page_allocator = std.heap.page_allocator;
const realpath = std.fs.realpath;
const bufferedReader = std.io.bufferedReader;
const maxInt = std.math.maxInt;
const MultiArrayList = std.MultiArrayList;
const Allocator = std.mem.Allocator;
const trim = std.mem.trim;
const replacementSize = std.mem.replacementSize;
const replace = std.mem.replace;
const fmtSliceEscapeUpper = std.fmt.fmtSliceEscapeUpper;
const tmpDir = std.testing.tmpDir;
const MAX_PATH_BYTES = std.fs.MAX_PATH_BYTES;
const join = std.fs.path.join;
// const test_allocator = std.testing.allocator;
// const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

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
/// This is how it is actually stored:
///
/// // Not using ArrayList, since we don't want padding
/// nodes: MultiArrayList(Node) = {
///     {
///         .comments = "foo1", "foo2", "foo3";
///     },
///     {
///         .property = "DataModule";
///         .children = 2, 3 and 5;
///     },
///     {
///         .property = "SupportedGameVersion";
///         .value = "Pre4";
///     },
///     {
///         .property = "IconFile";
///         .value = "ContentFile";
///         .comments = "bar" and "baz";
///         .children = 4;
///     },
///     {
///         .property = "FilePath";
///         .value = "foo.png";
///         .comments = "bee";
///     },
///     {
///         .property = "Description";
///         .value = "lol";
///     }
/// };
pub fn main() !void {
    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    return convert("tests/ini_test_files/general/comments/in.ini", "C:/Users/welfj/Desktop/out.ini", allocator);
}

const Token = struct {
    type: Type,
    slice: []const u8,

    const Type = enum {
        Comment,
        Tabs,
        Spaces,
        Equals,
        Newline,
        Sentence,
    };
};

const Node = struct {
    tabs: ?[]const u8 = null,
    property: ?[]const u8 = null,
    value: ?[]const u8 = null,
    comments: ArrayList([]const u8),
    children: ArrayList(Node),
};

fn convert(input_path: []const u8, output_path: []const u8, allocator: Allocator) !void {
    const cwd = std.fs.cwd();

    var input_file = try cwd.openFile(input_path, .{});
    defer input_file.close();

    var buf_reader = bufferedReader(input_file.reader());
    var in_stream = buf_reader.reader();

    const text = try in_stream.readAllAlloc(allocator, maxInt(usize));
    defer allocator.free(text);

    const NodeList = MultiArrayList(Node);
    var nodes = NodeList{};
    defer nodes.deinit(allocator);

    var lf_text = try crlfToLf(text, allocator);
    defer allocator.free(lf_text);

    // TODO: Should I stop passing the address of allocator, tokens, and ast everywhere?

    var tokens = try getTokens(lf_text, allocator);

    var ast = try getAst(&tokens, allocator);

    const output_file = try cwd.createFile(output_path, .{});
    defer output_file.close();

    for (ast.items) |*child, index| {
        try writeAst(child, &output_file);

        // Don't add a trailing newline, since writeAst() already adds it
        if (child.property != null and index < ast.items.len - 1) {
            try output_file.writeAll("\n");
        }
    }
}

fn crlfToLf(text: []const u8, allocator: Allocator) ![]const u8 {
    const replacement_size = replacementSize(u8, text, "\r\n", "\n");
    var lf_text = try allocator.alloc(u8, replacement_size);
    _ = replace(u8, text, "\r\n", "\n", lf_text);
    return lf_text;
}

fn getTokens(lf_text: []const u8, allocator: Allocator) !ArrayList(Token) {
    var slice: []const u8 = lf_text;

    var tokens = ArrayList(Token).init(allocator);

    var in_multiline_comment = false;

    while (slice.len > 0) {
        const token = getToken(&slice, &in_multiline_comment);
        try tokens.append(token);
    }

    return tokens;
}

fn getToken(slice: *[]const u8, in_multiline_comment: *bool) Token {
    // TODO: Consistently use either while-loops or for-loops everywhere in this function

    if (slice.*[0] == '\n') {
        const token = Token{ .type = .Newline, .slice = slice.*[0..1] };
        slice.* = slice.*[1..];
        return token;
    } else if (in_multiline_comment.*) {
        var i: usize = 0;
        while (i < slice.len) : (i += 1) {
            if (slice.*[i] == '\n') {
                break;
            } else if (slice.*[i] == '*' and slice.*[i + 1] == '/') {
                in_multiline_comment.* = false;
                i += 2;
                break;
            }
        }

        const comment_end_index = if (in_multiline_comment.*) i else i - 2;
        const comment = trim(u8, slice.*[0..comment_end_index], " ");
        const token = Token{ .type = .Comment, .slice = comment };
        slice.* = slice.*[i..];
        return token;
    } else if (slice.*[0] == '/' and slice.*[1] == '/') {
        const index = std.mem.indexOf(u8, slice.*[2..], "\n");
        const newline_index = if (index != null) index.? + 2 else slice.len;
        const token = Token{ .type = .Comment, .slice = trim(u8, slice.*[2..newline_index], " ") };
        slice.* = slice.*[newline_index..];
        return token;
    } else if (slice.*[0] == '/' and slice.*[1] == '*') {
        in_multiline_comment.* = true;

        var i: usize = 2;
        while (i < slice.len) : (i += 1) {
            if (slice.*[i] == '\n') {
                break;
            } else if (slice.*[i] == '*' and slice.*[i + 1] == '/') {
                in_multiline_comment.* = false;
                i += 2;
                break;
            }
        }

        const comment_end_index = if (in_multiline_comment.*) i else i - 2;
        const comment = trim(u8, slice.*[2..comment_end_index], " ");
        const token = Token{ .type = .Comment, .slice = comment };
        slice.* = slice.*[i..];
        return token;
    } else if (slice.*[0] == '\t') {
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
    } else if (slice.*[0] == ' ') {
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
    } else if (slice.*[0] == '=') {
        const token = Token{ .type = .Equals, .slice = slice.*[0..1] };
        // TODO: Check what happens if a line ends with an =, since I don't know if slice ends with '\0'
        // TODO: The same question goes for the comment parsing code that reads 2 characters
        slice.* = slice.*[1..];
        return token;
    }

    // A Sentence ends with a word, or the start of a comment
    var end_index: usize = 1;
    var sentence_end_index: usize = end_index;
    while (end_index < slice.len) : (end_index += 1) {
        // TODO: This doesn't handle an = being part of the value correctly
        if (slice.*[end_index] == '=' or slice.*[end_index] == '\n' or (slice.*[end_index] == '/' and (slice.*[end_index + 1] == '*' or slice.*[end_index + 1] == '/'))) {
            break;
        }
        if (slice.*[end_index] != ' ') {
            sentence_end_index = end_index + 1;
        }
    }

    const token = Token{ .type = .Sentence, .slice = slice.*[0..sentence_end_index] };
    slice.* = slice.*[sentence_end_index..];
    return token;
}

fn getAst(tokens: *ArrayList(Token), allocator: Allocator) !ArrayList(Node) {
    var ast = ArrayList(Node).init(allocator);

    var token_index: usize = 0;

    while (token_index < tokens.items.len) {
        const node = try getNode(tokens, &token_index, 0, allocator);
        try ast.append(node);
    }

    return ast;
}

const NodeError = error{
    TooManyTabs,
    Unexpected,
};

fn getNode(tokens: *ArrayList(Token), token_index: *usize, depth: i32, allocator: Allocator) error{ TooManyTabs, Unexpected, OutOfMemory }!Node {
    const States = enum {
        Start,
        Property,
        Equals,
        Value,
        Newline,
    };

    var seen: States = .Start;

    var node = Node{
        .comments = ArrayList([]const u8).init(allocator),
        .children = ArrayList(Node).init(allocator),
    };

    while (token_index.* < tokens.items.len) {
        const token = tokens.items[token_index.*];

        // TODO: Figure out why {s: <42} doesn't set the width to 42
        std.debug.print("'{s}'\t\t{}\n", .{ fmtSliceEscapeUpper(token.slice), token.type });

        if (seen == .Start and token.type == .Sentence) {
            node.property = token.slice;
            seen = .Property;
            token_index.* += 1;
        } else if (seen == .Start and token.type == .Tabs) {
            if (token.slice.len > depth) {
                return NodeError.TooManyTabs;
            } else if (token.slice.len == depth) {
                node.tabs = token.slice;
                token_index.* += 1;
            } else {
                return node;
            }
        } else if (seen == .Newline and token.type == .Tabs) {
            if (token.slice.len > depth + 1) {
                return NodeError.TooManyTabs;
            } else if (token.slice.len == depth + 1) {
                const child_node = try getNode(tokens, token_index, depth + 1, allocator);
                try node.children.append(child_node);
            } else {
                return node;
            }
        } else if (seen == .Property and token.type == .Equals) {
            seen = .Equals;
            token_index.* += 1;
        } else if (seen == .Equals and token.type == .Sentence) {
            node.value = token.slice;
            seen = .Value;
            token_index.* += 1;
        } else if (seen == .Newline and (depth == 0 or token.type == .Tabs or token.type == .Equals or token.type == .Sentence)) {
            return node;
        } else if (token.type == .Comment) {
            try node.comments.append(token.slice);
            token_index.* += 1;
        } else if (token.type == .Spaces) {
            token_index.* += 1;
        } else if (token.type == .Newline) {
            seen = .Newline;
            token_index.* += 1;
        } else {
            return NodeError.Unexpected;
        }
    }

    return node;
}

fn writeAst(node: *Node, file: *const std.fs.File) !void {
    // Don't add an empty line
    if (node.property == null and node.comments.items.len == 0) {
        return;
    }

    // Write tabs to file
    if (node.tabs != null) {
        // std.debug.print("'{s}'\n", .{fmtSliceEscapeUpper(node.tabs.?)});
        try file.writeAll(node.tabs.?);
    }

    // Write property to file
    if (node.property != null) {
        // std.debug.print("'{s}'\n", .{node.property.?});
        try file.writeAll(node.property.?);
    }

    // Write value and equals to file
    if (node.value != null) {
        // std.debug.print("' = '\n", .{});
        try file.writeAll(" = ");

        // std.debug.print("'{s}'\n", .{node.value.?});
        try file.writeAll(node.value.?);
    }

    // Write comments to file
    if (node.comments.items.len > 0) {
        // std.debug.print("' //'\n", .{});

        if (node.tabs != null or node.property != null) {
            try file.writeAll(" ");
        }

        try file.writeAll("//");

        for (node.comments.items) |comment| {
            // std.debug.print("' {s}'\n", .{comment});
            try file.writeAll(" ");
            try file.writeAll(comment);
        }
    }

    // Write newline to file
    // std.debug.print("'\\n'\n", .{});
    try file.writeAll("\n");

    // Recursively enter child nodes
    // std.debug.print("Recursing into child\n", .{});
    for (node.children.items) |*child| {
        try writeAst(child, file);
    }
}

test "everything" {
    var tmpdir = tmpDir(.{});
    defer tmpdir.cleanup();

    var tmpdir_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    var tmpdir_path = try tmpdir.dir.realpath(".", &tmpdir_path_buffer);

    var iterable_tests = try std.fs.cwd().openIterableDir("tests/ini_test_files/general", .{});
    defer iterable_tests.close();

    // TODO: Use test_allocator
    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);
    defer tests_walker.deinit();

    while (try tests_walker.next()) |entry| {
        var out_buffer: [MAX_PATH_BYTES]u8 = undefined;
        var dir_path = try entry.dir.realpath(".", &out_buffer);

        if (entry.kind == std.fs.File.Kind.File and std.mem.eql(u8, entry.basename, "in.ini")) {
            std.debug.print("\nSubtest '{s}'", .{entry.path});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and std.mem.eql(u8, entry.basename, "in.ini"), dir_path });

            var input_path = try join(allocator, &.{ dir_path, "in.ini" });
            defer allocator.free(input_path);
            var expected_path = try join(allocator, &.{ dir_path, "out.ini" });
            defer allocator.free(expected_path);

            var output_path = try join(allocator, &.{ tmpdir_path, "output.ini" });
            defer allocator.free(output_path);

            // std.debug.print("{s}\n{s}\n{s}\n\n", .{ input_path, expected_path, output_path });

            try convert(input_path, output_path, allocator);

            const cwd = std.fs.cwd();

            var expected_file = try cwd.openFile(expected_path, .{});
            defer expected_file.close();
            var expected_buf_reader = bufferedReader(expected_file.reader());
            var expected_stream = expected_buf_reader.reader();
            const expected_text_crlf = try expected_stream.readAllAlloc(allocator, maxInt(usize));
            defer allocator.free(expected_text_crlf);
            var expected_text = try crlfToLf(expected_text_crlf, allocator);
            defer allocator.free(expected_text);

            var output_file = try cwd.openFile(output_path, .{});
            defer output_file.close();
            var output_buf_reader = bufferedReader(output_file.reader());
            var output_stream = output_buf_reader.reader();
            const output_text_crlf = try output_stream.readAllAlloc(allocator, maxInt(usize));
            defer allocator.free(output_text_crlf);
            var output_text = try crlfToLf(output_text_crlf, allocator);
            defer allocator.free(output_text);

            try expectEqualStrings(expected_text, output_text);
            std.debug.print(" succeeded!", .{});
        }
    }
}
