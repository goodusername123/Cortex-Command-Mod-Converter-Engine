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
const bufferedWriter = std.io.bufferedWriter;
const fmtSliceEscapeUpper = std.fmt.fmtSliceEscapeUpper;
const tmpDir = std.testing.tmpDir;
const MAX_PATH_BYTES = std.fs.MAX_PATH_BYTES;
const join = std.fs.path.join;
const basename = std.fs.path.basename;
// const test_allocator = std.testing.allocator;
// const expect = std.testing.expect;
const expectEqualStrings = std.testing.expectEqualStrings;

/// The purpose of the converter engine is to take an .ini input file like this:
/// /* foo1   */ /* foo2*//*foo3*/
/// DataModule
/// \tSupportedGameVersion = Pre4
/// \t/* bar */IconFile      = ContentFile /* baz
/// bee */\t\tFilePath=foo.png
/// \tDescription = bop
///
/// and to turn it into this .ini output file:
/// // foo1 foo2 foo3
/// DataModule
/// \tSupportedGameVersion = Pre4
/// \tIconFile = ContentFile // bar baz
/// \t\tFilePath = foo.png // bee
/// \tDescription = bop
///
/// returned in the form of this Abstract Syntax Tree:
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
///                 .value = "bop";
///             }
///         }
///     }
/// }
const Token = struct {
    type: Type,
    slice: []const u8,

    // TODO: Can I turn this struct into a tagged union,
    // in order to get rid of this Type enum definition?
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
    property: ?[]const u8 = null,
    value: ?[]const u8 = null,
    comments: ArrayList([]const u8),
    children: ArrayList(Node),
};

const File = struct {
    name: []const u8,
    ast: ArrayList(Node),
};

const Folder = struct {
    name: []const u8,
    files: ArrayList(File),
    folders: ArrayList(Folder),
};

pub fn main() !void {
    try convert(
        "I:/Programming/Cortex-Command-Mod-Converter-Engine/tests/mod/in/mod.rte",
        "I:/Programming/Cortex-Command-Mod-Converter-Engine/tests/mod/out",
    );
}

fn convert(input_mod_path: []const u8, output_folder_path: []const u8) !void {
    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var file_tree = try getFileTree(input_mod_path, allocator);

    const output_mod_path = try join(allocator, &.{ output_folder_path, basename(input_mod_path) });

    try writeFileTree(&file_tree, output_mod_path, allocator);
}

fn writeFileTree(file_tree: *const Folder, output_mod_path: []const u8, allocator: Allocator) !void {
    std.fs.makeDirAbsolute(output_mod_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => |e| return e,
    };

    for (file_tree.files.items) |file| {
        std.debug.print("output_mod_path: '{s}', file.name: '{s}'\n", .{ output_mod_path, file.name });
        const file_path = try join(allocator, &.{ output_mod_path, file.name });
        std.debug.print("file_path: '{s}'\n", .{file_path});
        try writeAst(&file.ast, file_path);
    }

    for (file_tree.folders.items) |folder| {
        const child_output_mod_path = try join(allocator, &.{ output_mod_path, folder.name });
        std.debug.print("{s}\n", .{child_output_mod_path});
        try writeFileTree(&folder, child_output_mod_path, allocator);
    }
}

// fn getFileTree(input_mod_path: []const u8, allocator: Allocator) !Folder {
//     return try getFileTreeRecursively(input_mod_path, allocator);
// }

fn getFileTree(folder_path: []const u8, allocator: Allocator) !Folder {
    std.debug.print("folder_path: '{s}'\n", .{folder_path});

    var folder = Folder{
        .name = basename(folder_path),
        .files = ArrayList(File).init(allocator),
        .folders = ArrayList(Folder).init(allocator),
    };

    var dir = try std.fs.openIterableDirAbsolute(folder_path, .{});
    defer dir.close();

    var dir_iterator = dir.iterate();
    while (try dir_iterator.next()) |entry| {
        std.debug.print("entry name '{s}', entry kind: '{}'\n", .{ entry.name, entry.kind });
        if (entry.kind == std.fs.File.Kind.File) {
            var file = File{
                .name = try allocator.dupe(u8, entry.name),
                .ast = try getFileAst(try join(allocator, &.{ folder_path, entry.name }), allocator),
            };
            try folder.files.append(file);
        } else if (entry.kind == std.fs.File.Kind.Directory) {
            var child_folder = try getFileTree(try join(allocator, &.{ folder_path, entry.name }), allocator);
            try folder.folders.append(child_folder);
        }
    }

    return folder;
}

fn getFileAst(file_path: []const u8, allocator: Allocator) !ArrayList(Node) {
    const text = try readFile(file_path, allocator);

    var tokens = try getTokens(text, allocator);

    // TODO: Should I stop passing the address of tokens and ast everywhere?

    var ast = try getAstFromTokens(&tokens, allocator);
    return ast;
}

fn readFile(input_path: []const u8, allocator: Allocator) ![]const u8 {
    const cwd = std.fs.cwd();

    var input_file = try cwd.openFile(input_path, .{});
    defer input_file.close();

    var buf_reader = bufferedReader(input_file.reader());
    var in_stream = buf_reader.reader();

    const text = try in_stream.readAllAlloc(allocator, maxInt(usize));

    const lf_text = try crlfToLf(text, allocator);

    return lf_text;
}

fn crlfToLf(text: []const u8, allocator: Allocator) ![]const u8 {
    const replacement_size = replacementSize(u8, text, "\r\n", "\n");
    var lf_text = try allocator.alloc(u8, replacement_size);
    _ = replace(u8, text, "\r\n", "\n", lf_text);
    return lf_text;
}

const TokenError = error{
    UnclosedMultiComment,
};

fn getTokens(lf_text: []const u8, allocator: Allocator) !ArrayList(Token) {
    var slice: []const u8 = lf_text;

    var tokens = ArrayList(Token).init(allocator);

    var multiline_comment_depth: i32 = 0;

    while (slice.len > 0) {
        const token = getToken(&slice, &multiline_comment_depth);
        // std.debug.print("'{s}'\t\t{}\n", .{ fmtSliceEscapeUpper(token.slice), token.type });
        try tokens.append(token);
    }

    if (multiline_comment_depth > 0) {
        return TokenError.UnclosedMultiComment;
    }

    return tokens;
}

fn getToken(slice: *[]const u8, multiline_comment_depth: *i32) Token {
    // TODO: Consistently use either while-loops or for-loops everywhere in this function

    if (slice.*[0] == '\n') {
        const token = Token{ .type = .Newline, .slice = slice.*[0..1] };
        slice.* = slice.*[1..];
        return token;
    } else if (multiline_comment_depth.* > 0) {
        var found_comment_marker = false;
        var i: usize = 0;
        while (i < slice.len) {
            if (slice.*[i] == '\n') {
                break;
            } else if (slice.*[i] == '/' and slice.*[i + 1] == '*') {
                multiline_comment_depth.* += 1;
                i += 2;
                found_comment_marker = true;
                break;
            } else if (slice.*[i] == '*' and slice.*[i + 1] == '/') {
                multiline_comment_depth.* -= 1;
                i += 2;
                found_comment_marker = true;
                break;
            } else {
                i += 1;
            }
        }

        const comment_end_index = if (found_comment_marker) i - 2 else i;
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
        multiline_comment_depth.* += 1;

        var found_comment_marker = false;
        var i: usize = 2;
        while (i < slice.len) {
            if (slice.*[i] == '\n') {
                break;
            } else if (slice.*[i] == '/' and slice.*[i + 1] == '*') {
                multiline_comment_depth.* += 1;
                i += 2;
                found_comment_marker = true;
                break;
            } else if (slice.*[i] == '*' and slice.*[i + 1] == '/') {
                multiline_comment_depth.* -= 1;
                i += 2;
                found_comment_marker = true;
                break;
            } else {
                i += 1;
            }
        }

        const comment_end_index = if (found_comment_marker) i - 2 else i;
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

fn getAstFromTokens(tokens: *ArrayList(Token), allocator: Allocator) !ArrayList(Node) {
    var ast = ArrayList(Node).init(allocator);

    var token_index: usize = 0;

    if (lineHasSentence(tokens, token_index)) {
        token_index = leftTrimFirstLine(tokens);
    }

    while (token_index < tokens.items.len) {
        const node = try getNode(tokens, &token_index, 0, allocator);
        try ast.append(node);
    }

    return ast;
}

fn lineHasSentence(tokens: *ArrayList(Token), token_index_: usize) bool {
    var token_index = token_index_;

    while (token_index < tokens.items.len) {
        const token = tokens.items[token_index];

        if (token.type == .Newline) {
            return false;
        } else if (token.type == .Sentence) {
            return true;
        }

        token_index += 1;
    }

    return false;
}

fn leftTrimFirstLine(tokens: *ArrayList(Token)) usize {
    var token_index: usize = 0;

    while (tokens.items[token_index].type != .Sentence) {
        token_index += 1;
    }

    return token_index;
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

    var token = tokens.items[token_index.*];

    var line_depth = getLineDepth(tokens, token_index.*);
    // std.debug.print("a {}\n", .{line_depth});
    if (line_depth > depth) {
        return NodeError.TooManyTabs;
    } else if (line_depth < depth) {
        return node;
    }

    var checked_line_depth = true;

    while (token_index.* < tokens.items.len) {
        token = tokens.items[token_index.*];

        // TODO: Figure out why {s: <42} doesn't set the width to 42
        // std.debug.print("'{s}'\t\t{}\n", .{ fmtSliceEscapeUpper(token.slice), token.type });

        if (seen == .Start and token.type == .Sentence) {
            // This if-statement is deliberately in a loop,
            // since whitespace and multiline comments may come before it
            node.property = token.slice;
            seen = .Property;
            token_index.* += 1;
        } else if (seen == .Newline and !checked_line_depth) {
            checked_line_depth = true;

            line_depth = getLineDepth(tokens, token_index.*);
            // std.debug.print("b {}\n", .{line_depth});

            if (line_depth > depth + 1) {
                return NodeError.TooManyTabs;
            } else if (line_depth == depth + 1) {
                const child_node = try getNode(tokens, token_index, depth + 1, allocator);
                try node.children.append(child_node);
                checked_line_depth = false;
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
        } else if (token.type == .Comment) {
            if (token.slice.len > 0) {
                try node.comments.append(token.slice);
            }
            token_index.* += 1;
        } else if (token.type == .Tabs or token.type == .Spaces) {
            token_index.* += 1;
        } else if (token.type == .Newline) {
            seen = .Newline;
            token_index.* += 1;
            checked_line_depth = false;
        } else {
            // std.debug.print("{}\n", .{token});
            return NodeError.Unexpected;
        }
    }

    return node;
}

fn getLineDepth(tokens: *ArrayList(Token), token_index_: usize) i32 {
    var token_index = token_index_;

    var token = tokens.items[token_index];

    if (token.type == .Sentence) {
        return 0;
    } else if (!lineHasSentence(tokens, token_index)) {
        return getNextSentenceDepth(tokens, token_index);
    }

    var tabs_seen: i32 = 0;

    while (token_index < tokens.items.len) {
        token = tokens.items[token_index];

        if (token.type == .Sentence) {
            return tabs_seen;
        }

        if (token.type == .Tabs) {
            tabs_seen += @intCast(i32, token.slice.len);
        }

        token_index += 1;
    }

    // If the end of the file is reached
    // TODO: Find a way to return the same depth as the previous Sentence line
    // It isn't as easy as "return depth", since it can also be "return depth + 1"
    return 0;
}

fn getNextSentenceDepth(tokens: *ArrayList(Token), token_index_: usize) i32 {
    var token_index = token_index_;

    var tabs_seen: i32 = 0;

    while (token_index < tokens.items.len) {
        const token = tokens.items[token_index];

        if (token.type == .Newline) {
            tabs_seen = 0;
        } else if (token.type == .Sentence) {
            return tabs_seen;
        }

        if (token.type == .Tabs) {
            tabs_seen += @intCast(i32, token.slice.len);
        }

        token_index += 1;
    }

    // If the end of the file is reached
    // TODO: Find a way to return the same depth as the previous Sentence line
    // It isn't as easy as "return depth", since it can also be "return depth + 1"
    return 0;
}

fn writeAst(ast: *const ArrayList(Node), output_path: []const u8) !void {
    const cwd = std.fs.cwd();
    const output_file = try cwd.createFile(output_path, .{});
    defer output_file.close();
    var buffered = bufferedWriter(output_file.writer());
    const buffered_writer = buffered.writer();

    for (ast.items) |*child, index| {
        try writeAstRecursively(child, buffered_writer, 0);

        // Doesn't add a trailing newline, because writeAstRecursively() already adds it
        if (child.property != null and index < ast.items.len - 1) {
            try writeBuffered(buffered_writer, "\n");
        }
    }

    try buffered.flush();
}

fn writeAstRecursively(node: *Node, buffered_writer: anytype, depth: usize) !void {
    // Don't add an empty line
    if (node.property == null and node.comments.items.len == 0) {
        return;
    }

    // Write tabs to file
    var i: usize = 0;
    while (i < depth) : (i += 1) {
        // std.debug.print("'\t'\n", .{});
        try writeBuffered(buffered_writer, "\t");
    }

    // Write property to file
    if (node.property != null) {
        // std.debug.print("'{s}'\n", .{node.property.?});
        try writeBuffered(buffered_writer, node.property.?);
    }

    // Write value and equals to file
    if (node.value != null) {
        // std.debug.print("' = '\n", .{});
        try writeBuffered(buffered_writer, " = ");

        // std.debug.print("'{s}'\n", .{node.value.?});
        try writeBuffered(buffered_writer, node.value.?);
    }

    // Write comments to file
    if (node.comments.items.len > 0) {
        // std.debug.print("' //'\n", .{});

        if (node.property != null) {
            try writeBuffered(buffered_writer, " ");
        }

        try writeBuffered(buffered_writer, "//");

        for (node.comments.items) |comment| {
            // std.debug.print("' {s}'\n", .{comment});
            try writeBuffered(buffered_writer, " ");
            try writeBuffered(buffered_writer, comment);
        }
    }

    // Write newline to file
    // std.debug.print("'\\n'\n", .{});
    try writeBuffered(buffered_writer, "\n");

    // Recursively enter child nodes
    // std.debug.print("Recursing into child\n", .{});
    for (node.children.items) |*child| {
        try writeAstRecursively(child, buffered_writer, depth + 1);
    }
}

fn writeBuffered(buffered_writer: anytype, string: []const u8) !void {
    try buffered_writer.print("{s}", .{string});
}

test "general" {
    var tmpdir = tmpDir(.{});
    defer tmpdir.cleanup();

    var tmpdir_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    const tmpdir_path = try tmpdir.dir.realpath(".", &tmpdir_path_buffer);

    var iterable_tests = try std.fs.cwd().openIterableDir("tests/general", .{});
    defer iterable_tests.close();

    // TODO: Use test_allocator
    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);

    while (try tests_walker.next()) |entry| {
        var out_buffer: [MAX_PATH_BYTES]u8 = undefined;
        const dir_path = try entry.dir.realpath(".", &out_buffer);

        if (entry.kind == std.fs.File.Kind.File and std.mem.eql(u8, entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'general/{s}'", .{entry.path});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and std.mem.eql(u8, entry.basename, "in.ini"), dir_path });

            const input_path = try join(allocator, &.{ dir_path, "in.ini" });
            const expected_path = try join(allocator, &.{ dir_path, "out.ini" });

            const output_path = try join(allocator, &.{ tmpdir_path, "output.ini" });

            // std.debug.print("{s}\n{s}\n{s}\n\n", .{ input_path, expected_path, output_path });

            const text = try readFile(input_path, allocator);

            var tokens = try getTokens(text, allocator);

            var ast = try getAstFromTokens(&tokens, allocator);

            try writeAst(&ast, output_path);

            const cwd = std.fs.cwd();

            const expected_file = try cwd.openFile(expected_path, .{});
            defer expected_file.close();
            var expected_buf_reader = bufferedReader(expected_file.reader());
            const expected_stream = expected_buf_reader.reader();
            const expected_text_crlf = try expected_stream.readAllAlloc(allocator, maxInt(usize));
            const expected_text = try crlfToLf(expected_text_crlf, allocator);

            const output_file = try cwd.openFile(output_path, .{});
            defer output_file.close();
            var output_buf_reader = bufferedReader(output_file.reader());
            const output_stream = output_buf_reader.reader();
            const output_text_crlf = try output_stream.readAllAlloc(allocator, maxInt(usize));
            const output_text = try crlfToLf(output_text_crlf, allocator);

            try expectEqualStrings(expected_text, output_text);
            std.debug.print(" passed", .{});
        }
    }

    std.debug.print("\n\n", .{});
}

test "invalid" {
    var iterable_tests = try std.fs.cwd().openIterableDir("tests/invalid", .{});
    defer iterable_tests.close();

    // TODO: Use test_allocator
    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);

    while (try tests_walker.next()) |entry| {
        var out_buffer: [MAX_PATH_BYTES]u8 = undefined;
        const dir_path = try entry.dir.realpath(".", &out_buffer);

        if (entry.kind == std.fs.File.Kind.File and std.mem.eql(u8, entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'invalid/{s}'", .{entry.path});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and std.mem.eql(u8, entry.basename, "in.ini"), dir_path });

            const input_path = try join(allocator, &.{ dir_path, "in.ini" });
            const error_path = try join(allocator, &.{ dir_path, "error.txt" });

            // std.debug.print("{s}\n{s}\n\n", .{ input_path, error_path });

            const cwd = std.fs.cwd();
            const error_file = try cwd.openFile(error_path, .{});
            defer error_file.close();
            var error_buf_reader = bufferedReader(error_file.reader());
            const error_stream = error_buf_reader.reader();
            const error_text_crlf = try error_stream.readAllAlloc(allocator, maxInt(usize));
            const error_text = try crlfToLf(error_text_crlf, allocator);

            const text = try readFile(input_path, allocator);

            verifyInvalidTestThrowsError(&text, allocator) catch |err| {
                try expectEqualStrings(@errorName(err), error_text);
            };

            std.debug.print(" passed", .{});
        }
    }

    std.debug.print("\n\n", .{});
}

fn verifyInvalidTestThrowsError(text: *const []const u8, allocator: Allocator) !void {
    var tokens = try getTokens(text.*, allocator);

    var ast = try getAstFromTokens(&tokens, allocator);
    _ = ast;

    // Tests from the invalid/ folder should always return an error from this function before reaching this
    unreachable;
}
