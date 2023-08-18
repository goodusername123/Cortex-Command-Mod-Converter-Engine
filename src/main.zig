const std = @import("std");

const default_max_load_percentage = std.hash_map.default_max_load_percentage;
const default_max_value_len = std.json.default_max_value_len;
const MAX_PATH_BYTES = std.fs.MAX_PATH_BYTES;
const page_allocator = std.heap.page_allocator;

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const HashMap = std.hash_map.HashMap;
const MultiArrayList = std.MultiArrayList;
const Scanner = std.json.Scanner;
const StringHashMap = std.hash_map.StringHashMap;

const allocPrint = std.fmt.allocPrint;
const basename = std.fs.path.basename;
const bufferedReader = std.io.bufferedReader;
const bufferedWriter = std.io.bufferedWriter;
const copyFileAbsolute = std.fs.copyFileAbsolute;
const endsWith = std.mem.endsWith;
const eql = std.mem.eql;
const expectEqualStrings = std.testing.expectEqualStrings;
const extension = std.fs.path.extension;
const fabs = std.math.fabs;
const fmtSliceEscapeUpper = std.fmt.fmtSliceEscapeUpper;
const join = std.fs.path.join;
const makeDirAbsolute = std.fs.makeDirAbsolute;
const maxInt = std.math.maxInt;
const parseFloat = std.fmt.parseFloat;
const parseFromSliceLeaky = std.json.parseFromSliceLeaky;
const realpath = std.fs.realpath;
const replace = std.mem.replace;
const replacementSize = std.mem.replacementSize;
const tmpDir = std.testing.tmpDir;
const trim = std.mem.trim;
const updateFileAbsolute = std.fs.updateFileAbsolute;

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

const Rule = struct {
    old_property: []const u8,
    old_value: []const u8,
    new_property: ?[]const u8,
    new_value: ?[]const u8,
};

const IniFile = struct {
    name: []const u8,
    ast: ArrayList(Node),
};

const IniFolder = struct {
    name: []const u8,
    files: ArrayList(IniFile),
    folders: ArrayList(IniFolder),
};

const Diagnostics = struct {
    file_path: ?[]const u8 = null,
    token: ?[]const u8 = null,
    line: ?i32 = null,
    column: ?i32 = null,
};

const PropertyValuePair = struct {
    property: []const u8,
    value: []const u8,
};

const PropertyValuePairContext = struct {
    pub fn hash(self: PropertyValuePairContext, x: PropertyValuePair) u64 {
        _ = self;
        // TODO: XOR is shite; it returns 0 when the property and value are identical
        // I tried replacing it with this one, but it panics with integer overflow:
        // Source: https://stackoverflow.com/a/27952689/13279557
        // var property_hash = std.hash_map.hashString(x.property);
        // const value_hash = std.hash_map.hashString(x.value);
        // property_hash ^= value_hash + 0x517cc1b727220a95 + (property_hash << 6) + (property_hash >> 2);
        // return property_hash;

        return std.hash_map.hashString(x.property) ^ std.hash_map.hashString(x.value);
    }

    pub fn eql(self: PropertyValuePairContext, a: PropertyValuePair, b: PropertyValuePair) bool {
        _ = self;
        return std.mem.eql(u8, a.property, b.property) and std.mem.eql(u8, a.value, b.value);
    }
};

const UpdateIniFileTreeErrors = error{
    ExpectedValue,
};

pub fn main() !void {
    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    const cwd = std.fs.cwd();

    var input_mod_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    const input_mod_path = try cwd.realpath("tons_of_mods/in", &input_mod_path_buffer);

    var output_mod_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    const output_mod_path = try cwd.realpath("tons_of_mods/out", &output_mod_path_buffer);

    var diagnostics: Diagnostics = .{};
    convert(
        input_mod_path,
        output_mod_path,
        allocator,
        &diagnostics,
    ) catch |err| switch (err) {
        error.UnexpectedToken => {
            const token = diagnostics.token orelse "null";
            const file_path = diagnostics.file_path orelse "null";
            const line = diagnostics.line orelse -1;
            const column = diagnostics.column orelse -1;

            std.debug.print("Error: Unexpected token\nToken: '{s}'\nFile path: {s}\nLine: {}\nColumn: {} (roughly)\n", .{
                token,
                file_path,
                line,
                column,
            });
        },
        error.TooManyTabs => {
            const file_path = diagnostics.file_path orelse "null";
            const line = diagnostics.line orelse -1;
            const column = diagnostics.column orelse -1;

            std.debug.print("Error: Too many tabs\nFile path: {s}\nLine: {} (roughly)\nColumn: {} (roughly)\n", .{
                file_path,
                line,
                column,
            });
        },
        else => |e| return e,
    };
}

fn convert(input_mod_path: []const u8, output_folder_path: []const u8, allocator: Allocator, diagnostics: *Diagnostics) !void {
    try makeOutputDirs(input_mod_path, output_folder_path, allocator);

    try copyFiles(input_mod_path, output_folder_path, allocator);

    const lua_rules = try parseLuaRules(allocator);
    try applyLuaRules(lua_rules, output_folder_path, allocator);

    var file_tree = try getIniFileTree(input_mod_path, allocator, diagnostics);

    // Create a hashmap, where the key is a property,
    // and the value is a list of Nodes that have this key
    var properties = StringHashMap(ArrayList(*Node)).init(allocator);
    try addProperties(&file_tree, &properties, allocator);

	// This HAS to be called before addPropertyValuePairs(),
	// cause the PropertyValuePair keys it generates can't be modified later.
	//
	// It also HAS to be called before applyIniFilePathRules(),
	// because otherwise this could happen:
	// The game reports that Base.rte/foo.png doesn't exist,
	// so the user enters this rule:
	// "Base.rte/foo.png": "Base.rte/bar.png"
	// The game reports that Base.rte/foo.png *still* doesn't exist,
	// due to the parsed input mod containing "Base.rte/foo.bmp"
	// The rule isn't applied to this string, due to it saying .bmp!
    try bmpExtensionToPng(&properties, allocator);

    // Create a hashmap, where the key is a PropertyValuePair,
    // and the value is a list of Nodes that have this key
    var property_value_pairs = HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage).init(allocator);
    try addPropertyValuePairs(&file_tree, &property_value_pairs, allocator);

    const ini_copy_of_rules = try parseIniCopyOfRules(allocator);
    try applyIniCopyOfRules(ini_copy_of_rules, &property_value_pairs);

    const ini_file_path_rules = try parseIniFilePathRules(allocator);
    try applyIniFilePathRules(ini_file_path_rules, &property_value_pairs);

    const ini_script_path_rules = try parseIniScriptPathRules(allocator);
    try applyIniScriptPathRules(ini_script_path_rules, &property_value_pairs);

    const ini_property_rules = try parseIniPropertyRules(allocator);
    try applyIniPropertyRules(ini_property_rules, &properties);

    const ini_rules = try parseIniRules(allocator);
    applyIniRules(ini_rules, &property_value_pairs);

    const ini_sound_container_rules = try parseIniSoundContainerRules(allocator);
    applyIniSoundContainerRules(ini_sound_container_rules, &property_value_pairs);

    try updateIniFileTree(&properties, &property_value_pairs, allocator);

    try writeIniFileTree(&file_tree, output_folder_path, allocator);
}

fn makeOutputDirs(input_folder_path: []const u8, output_folder_path: []const u8, allocator: Allocator) !void {
    makeDirAbsolute(output_folder_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => |e| return e,
    };

    var iterable_dir = try std.fs.openIterableDirAbsolute(input_folder_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.directory) {
            const child_input_folder_path = try join(allocator, &.{ input_folder_path, entry.name });
            const child_output_folder_path = try join(allocator, &.{ output_folder_path, entry.name });
            try makeOutputDirs(child_input_folder_path, child_output_folder_path, allocator);
        }
    }
}

/// Doesn't copy .ini files
fn copyFiles(input_folder_path: []const u8, output_folder_path: []const u8, allocator: Allocator) !void {
    var iterable_dir = try std.fs.openIterableDirAbsolute(input_folder_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.file) {
            if (!eql(u8, extension(entry.name), ".ini")) {
                const input_file_path = try join(allocator, &.{ input_folder_path, entry.name });
                const output_file_path = try join(allocator, &.{ output_folder_path, entry.name });
                const access = std.fs.accessAbsolute(output_file_path, .{});

                if (access == error.FileNotFound) {
                    try copyFileAbsolute(input_file_path, output_file_path, .{});
                } else if (access catch null) |_| {
                    // TODO: Windows can be significantly faster if we use iterable_dir.dir.stat() manually here,
                    // if (and only if) directory mod times are updated when files change on Windows!

                    const input_stat = try iterable_dir.dir.statFile(input_file_path);
                    const output_stat = try iterable_dir.dir.statFile(output_file_path);
                    if (!identicalStats(input_stat, output_stat, null)) {
                        _ = try updateFileAbsolute(input_file_path, output_file_path, .{});
                    }
                } else {
                    return access;
                }
            }
        } else if (entry.kind == std.fs.File.Kind.directory) {
            const child_input_folder_path = try join(allocator, &.{ input_folder_path, entry.name });
            const child_output_folder_path = try join(allocator, &.{ output_folder_path, entry.name });
            try copyFiles(child_input_folder_path, child_output_folder_path, allocator);
        }
    }
}

fn identicalStats(stat1: std.fs.File.Stat, stat2: std.fs.File.Stat, override_mode: ?std.os.system.mode_t) bool {
    const actual_mode = override_mode orelse stat1.mode;

    return (stat1.size == stat2.size and
        stat1.mtime == stat2.mtime and
        actual_mode == stat2.mode);
}

fn parseLuaRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const lua_rules_path = "src/lua_rules.json";
    const lua_rules_text = try readFile(lua_rules_path, allocator);

    var scanner = Scanner.initCompleteInput(allocator, lua_rules_text);

    var lua_rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return lua_rules;
}

fn applyLuaRules(lua_rules: std.json.ArrayHashMap([]const u8), folder_path: []const u8, allocator: Allocator) !void {
    var iterable_dir = try std.fs.openIterableDirAbsolute(folder_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |dir_entry| {
        if (dir_entry.kind == std.fs.File.Kind.file) {
            if (eql(u8, extension(dir_entry.name), ".lua")) {
                const file_path = try join(allocator, &.{ folder_path, dir_entry.name });
                var text = try readFile(file_path, allocator);

                var text_contains_any_key = false;

                var map_iterator = lua_rules.map.iterator();
                while (map_iterator.next()) |map_entry| {
                    const key = map_entry.key_ptr.*;
                    const value = map_entry.value_ptr.*;

                    const text_contains_key = std.mem.indexOfPos(u8, text, 0, key) != null;
                    if (text_contains_key) {
                        text_contains_any_key = true;

                        const replacement_size = replacementSize(u8, text, key, value);
                        const new_text = try allocator.alloc(u8, replacement_size);
                        _ = replace(u8, text, key, value, new_text);
                        text = new_text;
                    }
                }

                if (text_contains_any_key) {
                    const cwd = std.fs.cwd();
                    var file = try cwd.createFile(file_path, .{});
                    defer file.close();
                    try file.writeAll(text);
                }
            }
        } else if (dir_entry.kind == std.fs.File.Kind.directory) {
            try applyLuaRules(lua_rules, try join(allocator, &.{ folder_path, dir_entry.name }), allocator);
        }
    }
}

fn getIniFileTree(folder_path: []const u8, allocator: Allocator, diagnostics: *Diagnostics) !IniFolder {
    // std.debug.print("folder_path: '{s}'\n", .{folder_path});

    var folder = IniFolder{
        .name = basename(folder_path),
        .files = ArrayList(IniFile).init(allocator),
        .folders = ArrayList(IniFolder).init(allocator),
    };

    var iterable_dir = try std.fs.openIterableDirAbsolute(folder_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |entry| {
        // std.debug.print("entry name '{s}', entry kind: '{}'\n", .{ entry.name, entry.kind });
        if (entry.kind == std.fs.File.Kind.file) {
            const file_path = try join(allocator, &.{ folder_path, entry.name });
            if (eql(u8, extension(entry.name), ".ini")) {
                diagnostics.file_path = file_path;
                // std.debug.print("file path '{s}'\n", .{file_path});
                const text = try readFile(file_path, allocator);

                var tokens = try getTokens(text, allocator);

                // TODO: Should I stop passing the address of tokens and ast everywhere?
                var ast = try getAstFromTokens(&tokens, allocator, diagnostics);

                var file = IniFile{
                    .name = try allocator.dupe(u8, entry.name),
                    .ast = ast,
                };
                try folder.files.append(file);
            }
        } else if (entry.kind == std.fs.File.Kind.directory) {
            var child_folder = try getIniFileTree(try join(allocator, &.{ folder_path, entry.name }), allocator, diagnostics);
            try folder.folders.append(child_folder);
        }
    }

    return folder;
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

fn getTokens(lf_text: []const u8, allocator: Allocator) !ArrayList(Token) {
    const TokenError = error{
        UnclosedMultiComment,
    };

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
        const newline_index = if (index) |i| i + 2 else slice.len;
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

fn getAstFromTokens(tokens: *ArrayList(Token), allocator: Allocator, diagnostics: *Diagnostics) !ArrayList(Node) {
    var ast = ArrayList(Node).init(allocator);

    var token_index: usize = 0;

    if (lineHasSentence(tokens, token_index)) {
        token_index = leftTrimFirstLine(tokens);
    }

    while (token_index < tokens.items.len) {
        const node = try getNode(tokens, &token_index, 0, allocator, diagnostics);
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

fn getNode(tokens: *ArrayList(Token), token_index: *usize, depth: i32, allocator: Allocator, diagnostics: *Diagnostics) error{ TooManyTabs, UnexpectedToken, OutOfMemory }!Node {
    const States = enum {
        Start,
        Property,
        Equals,
        Value,
        Newline,
    };

    const NodeError = error{
        TooManyTabs,
        UnexpectedToken,
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
        calculateLineAndColumnDiagnostics(tokens, token_index.*, diagnostics);
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
                calculateLineAndColumnDiagnostics(tokens, token_index.*, diagnostics);
                return NodeError.TooManyTabs;
            } else if (line_depth == depth + 1) {
                const child_node = try getNode(tokens, token_index, depth + 1, allocator, diagnostics);
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
            diagnostics.token = token.slice;
            calculateLineAndColumnDiagnostics(tokens, token_index.*, diagnostics);
            return NodeError.UnexpectedToken;
        }
    }

    return node;
}

fn calculateLineAndColumnDiagnostics(tokens: *ArrayList(Token), token_index: usize, diagnostics: *Diagnostics) void {
    diagnostics.line = 1;
    diagnostics.column = 1;

    var i: usize = 0;
    while (i < token_index) {
        var token = tokens.items[i];

        if (token.type == .Newline) {
            diagnostics.line.? += 1;
            diagnostics.column.? = 1;
        } else {
            diagnostics.column.? += @intCast(token.slice.len);
        }

        i += 1;
    }
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
            tabs_seen += @intCast(token.slice.len);
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
            tabs_seen += @intCast(token.slice.len);
        }

        token_index += 1;
    }

    // If the end of the file is reached
    // TODO: Find a way to return the same depth as the previous Sentence line
    // It isn't as easy as "return depth", since it can also be "return depth + 1"
    return 0;
}

fn addProperties(file_tree: *IniFolder, properties: *StringHashMap(ArrayList(*Node)), allocator: Allocator) !void {
    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            try addFileProperties(node, properties, allocator);
        }
    }

    for (file_tree.folders.items) |*folder| {
        try addProperties(folder, properties, allocator);
    }
}

fn addFileProperties(node: *Node, properties: *StringHashMap(ArrayList(*Node)), allocator: Allocator) !void {
    if (node.property) |property| {
        var result = try properties.getOrPut(property);

        if (!result.found_existing) {
            result.value_ptr.* = ArrayList(*Node).init(allocator);
        }

        try result.value_ptr.*.append(node);
    }

    for (node.children.items) |*child| {
        try addFileProperties(child, properties, allocator);
    }
}

fn bmpExtensionToPng(properties: *StringHashMap(ArrayList(*Node)), allocator: Allocator) !void {
    var file_path = properties.get("FilePath");
    if (file_path) |nodes| {
        for (nodes.items) |node| {
            if (node.value) |*path| {
                if (endsWith(u8, path.*, ".bmp") and !eql(u8, path.*, "palette.bmp") and !eql(u8, path.*, "palettemat.bmp")) {
                    // We have to dupe, since the u8s are const, but the slice itself isn't
                    var new_path = try allocator.dupe(u8, path.*);
                    new_path[new_path.len - 1] = 'g';
                    new_path[new_path.len - 2] = 'n';
                    new_path[new_path.len - 3] = 'p';
                    node.value = new_path;
                }
            }
        }
    }
}

fn addPropertyValuePairs(file_tree: *IniFolder, property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage), allocator: Allocator) !void {
    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            try addFilePropertyValuePairs(node, property_value_pairs, allocator);
        }
    }

    for (file_tree.folders.items) |*folder| {
        try addPropertyValuePairs(folder, property_value_pairs, allocator);
    }
}

fn addFilePropertyValuePairs(node: *Node, property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage), allocator: Allocator) !void {
    if (node.property != null and node.value != null) {
        const property_value_pair = PropertyValuePair{
            .property = node.property.?,
            .value = node.value.?,
        };
        var result = try property_value_pairs.getOrPut(property_value_pair);

        if (!result.found_existing) {
            result.value_ptr.* = ArrayList(*Node).init(allocator);
        }

        try result.value_ptr.*.append(node);
    }

    for (node.children.items) |*child| {
        try addFilePropertyValuePairs(child, property_value_pairs, allocator);
    }
}

fn parseIniCopyOfRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const ini_copy_of_rules_path = "src/ini_copy_of_rules.json";
    const ini_copy_of_rules_text = try readFile(ini_copy_of_rules_path, allocator);

    var scanner = Scanner.initCompleteInput(allocator, ini_copy_of_rules_text);

    var ini_copy_of_rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return ini_copy_of_rules;
}

fn applyIniCopyOfRules(ini_copy_of_rules: std.json.ArrayHashMap([]const u8), property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage)) !void {
    var map_iterator = ini_copy_of_rules.map.iterator();
    while (map_iterator.next()) |map_entry| {
        const old_value = map_entry.key_ptr.*;
        const new_value = map_entry.value_ptr.*;

        var pair = PropertyValuePair{
            .property = "CopyOf",
            .value = old_value,
        };
        var result = property_value_pairs.get(pair);
        if (result) |r| {
            for (r.items) |line| {
                line.value = new_value;
            }
        }
    }
}

fn parseIniFilePathRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const ini_copy_of_rules_path = "src/ini_file_path_rules.json";
    const ini_copy_of_rules_text = try readFile(ini_copy_of_rules_path, allocator);

    var scanner = Scanner.initCompleteInput(allocator, ini_copy_of_rules_text);

    var ini_copy_of_rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return ini_copy_of_rules;
}

fn applyIniFilePathRules(ini_copy_of_rules: std.json.ArrayHashMap([]const u8), property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage)) !void {
    var map_iterator = ini_copy_of_rules.map.iterator();
    while (map_iterator.next()) |map_entry| {
        const old_value = map_entry.key_ptr.*;
        const new_value = map_entry.value_ptr.*;

        var pair = PropertyValuePair{
            .property = "FilePath",
            .value = old_value,
        };
        var result = property_value_pairs.get(pair);
        if (result) |r| {
            for (r.items) |line| {
                line.value = new_value;
            }
        }
    }
}

fn parseIniScriptPathRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const ini_copy_of_rules_path = "src/ini_script_path_rules.json";
    const ini_copy_of_rules_text = try readFile(ini_copy_of_rules_path, allocator);

    var scanner = Scanner.initCompleteInput(allocator, ini_copy_of_rules_text);

    var ini_copy_of_rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return ini_copy_of_rules;
}

fn applyIniScriptPathRules(ini_copy_of_rules: std.json.ArrayHashMap([]const u8), property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage)) !void {
    var map_iterator = ini_copy_of_rules.map.iterator();
    while (map_iterator.next()) |map_entry| {
        const old_value = map_entry.key_ptr.*;
        const new_value = map_entry.value_ptr.*;

        var pair = PropertyValuePair{
            .property = "ScriptPath",
            .value = old_value,
        };
        var result = property_value_pairs.get(pair);
        if (result) |r| {
            for (r.items) |line| {
                line.value = new_value;
            }
        }
    }
}

fn parseIniPropertyRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const ini_property_rules_path = "src/ini_property_rules.json";
    const ini_property_rules_text = try readFile(ini_property_rules_path, allocator);

    var scanner = Scanner.initCompleteInput(allocator, ini_property_rules_text);

    var ini_property_rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return ini_property_rules;
}

fn applyIniPropertyRules(ini_property_rules: std.json.ArrayHashMap([]const u8), properties: *StringHashMap(ArrayList(*Node))) !void {
    var map_iterator = ini_property_rules.map.iterator();
    while (map_iterator.next()) |map_entry| {
        const old_property = map_entry.key_ptr.*;
        const new_property = map_entry.value_ptr.*;

        var result = properties.get(old_property);
        if (result) |r| {
            for (r.items) |line| {
                line.property = new_property;
            }
        }
    }
}

fn parseIniRules(allocator: Allocator) ![]Rule {
    const path = "src/ini_rules.json";
    const text = try readFile(path, allocator);
    return try parseFromSliceLeaky([]Rule, allocator, text, .{});
}

fn applyIniRules(ini_rules: []Rule, property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage)) void {
    for (ini_rules) |rule| {
        var key = PropertyValuePair{
            .property = rule.old_property,
            .value = rule.old_value,
        };
        var result = property_value_pairs.get(key);
        if (result) |r| {
            for (r.items) |line| {
                line.property = rule.new_property;
                line.value = rule.new_value;
            }
        }
    }
}

fn parseIniSoundContainerRules(allocator: Allocator) ![][]const u8 {
    const path = "src/ini_sound_container_rules.json";
    const text = try readFile(path, allocator);
    return try parseFromSliceLeaky([][]const u8, allocator, text, .{});
}

fn applyIniSoundContainerRules(ini_sound_container_rules: [][]const u8, property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage)) void {
    for (ini_sound_container_rules) |property| {
        var key = PropertyValuePair{
            .property = property,
            .value = "Sound",
        };
        var result = property_value_pairs.get(key);
        if (result) |r| {
            for (r.items) |line| {
                line.value = "SoundContainer";
            }
        }
    }
}

fn updateIniFileTree(properties: *StringHashMap(ArrayList(*Node)), property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage), allocator: Allocator) !void {
    try addGripStrength(property_value_pairs, allocator);
    try addOrUpdateSupportedGameVersion(properties, allocator);
    try maxLengthToOffsets(property_value_pairs, allocator);
    try maxMassToMaxInventoryMass(properties, allocator);
    try maxThrottleRangeToPositiveThrottleMultiplier(properties, allocator);
    try minThrottleRangeToNegativeThrottleMultiplier(properties, allocator);
}

fn addGripStrength(property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage), allocator: Allocator) !void {
    var pair = PropertyValuePair{
        .property = "AddActor",
        .value = "Arm",
    };

    var arm = property_value_pairs.get(pair);

    if (arm) |nodes| {
        // Add GripStrength to any Arm that is missing it
        outer: for (nodes.items) |node| {
            var children = &node.children;

            for (children.items) |child| {
                if (child.property) |property| {
                    if (eql(u8, property, "GripStrength")) {
                        continue :outer;
                    }
                }
            }

            try children.append(Node{
                .property = "GripStrength",
                .value = "424242",
                .comments = ArrayList([]const u8).init(allocator),
                .children = ArrayList(Node).init(allocator),
            });

            var result = try property_value_pairs.getOrPut(pair);

            if (!result.found_existing) {
                result.value_ptr.* = ArrayList(*Node).init(allocator);
            }

            try result.value_ptr.*.append(node);
        }
    }
}

fn addOrUpdateSupportedGameVersion(properties: *StringHashMap(ArrayList(*Node)), allocator: Allocator) !void {
    const err = error{
        MoreThanOneSupportedGameVersion,
        // MissingDataModule,
        MoreThanOneDataModule,
    };

    var supported_game_version = properties.get("SupportedGameVersion");
    if (supported_game_version) |nodes| {
        // Update the SupportedGameVersion, if necessary
        if (nodes.items.len == 1) {
            var node = nodes.items[0];

            if (node.*.value) |value| {
                // TODO: Maybe add a check that the input version isn't newer
                // than the version this engine thinks is the latest?
                if (!eql(u8, value, "Pre-Release 5.0")) {
                    node.*.value = "Pre-Release 5.0";
                }
            } else {
                return UpdateIniFileTreeErrors.ExpectedValue;
            }
        } else {
            return err.MoreThanOneSupportedGameVersion;
        }
    } else {
        // Add the SupportedGameVersion
        var data_module = properties.get("DataModule");
        if (data_module) |nodes| {
            if (nodes.items.len == 1) {
                var children = &nodes.items[0].children;

                try children.append(Node{
                    .property = "SupportedGameVersion",
                    .value = "Pre-Release 5.0",
                    .comments = ArrayList([]const u8).init(allocator),
                    .children = ArrayList(Node).init(allocator),
                });

                var supported_list = ArrayList(*Node).init(allocator);
                try supported_list.append(nodes.items[0]);
                try properties.put("SupportedGameVersion", supported_list);
            } else {
                return err.MoreThanOneDataModule;
            }
        } else {
            // TODO: Maybe bring this back?
            // return err.MissingDataModule;
        }
    }
}

fn maxLengthToOffsets(property_value_pairs: *HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage), allocator: Allocator) !void {
    var pair = PropertyValuePair{
        .property = "AddActor",
        .value = "Leg",
    };

    var leg = property_value_pairs.get(pair);

    if (leg) |nodes| {
        for (nodes.items) |node| {
            var children = &node.children;

            for (children.items) |*child| {
                if (child.property) |property| {
                    if (eql(u8, property, "MaxLength")) {
                        if (child.value) |value| {
                            child.property = "ContractedOffset";
                            child.value = "Vector";
                            try child.children.append(Node{
                                .property = "X",
                                .value = try allocPrint(allocator, "{d}", .{try parseFloat(f32, value) / 2}),
                                .comments = ArrayList([]const u8).init(allocator),
                                .children = ArrayList(Node).init(allocator),
                            });
                            try child.children.append(Node{
                                .property = "Y",
                                .value = "0",
                                .comments = ArrayList([]const u8).init(allocator),
                                .children = ArrayList(Node).init(allocator),
                            });

                            var extended_offset = Node{
                                .property = "ExtendedOffset",
                                .value = "Vector",
                                .comments = ArrayList([]const u8).init(allocator),
                                .children = ArrayList(Node).init(allocator),
                            };
                            try extended_offset.children.append(Node{
                                .property = "X",
                                .value = value,
                                .comments = ArrayList([]const u8).init(allocator),
                                .children = ArrayList(Node).init(allocator),
                            });
                            try extended_offset.children.append(Node{
                                .property = "Y",
                                .value = "0",
                                .comments = ArrayList([]const u8).init(allocator),
                                .children = ArrayList(Node).init(allocator),
                            });
                            try node.children.append(extended_offset);
                        } else {
                            return UpdateIniFileTreeErrors.ExpectedValue;
                        }
                    }
                }
            }
        }
    }
}

fn maxMassToMaxInventoryMass(properties: *StringHashMap(ArrayList(*Node)), allocator: Allocator) !void {
    var actor = properties.get("AddActor");
    if (actor) |actor_nodes| {
        for (actor_nodes.items) |actor_node| {
            for (actor_node.children.items) |*child_node| {
                if (child_node.property) |child_property| {
                    if (eql(u8, child_property, "MaxMass")) {
                        if (child_node.value) |v| {
                            const max_mass = try parseFloat(f32, v);
                            for (actor_node.children.items) |child_node2| {
                                if (child_node2.property) |child_property2| {
                                    if (eql(u8, child_property2, "Mass")) {
                                        if (child_node2.value) |v2| {
                                            const mass = try parseFloat(f32, v2);
                                            child_node.property = "MaxInventoryMass";
                                            const max_inventory_mass = max_mass - mass;
                                            child_node.value = try allocPrint(allocator, "{d}", .{max_inventory_mass});
                                        } else {
                                            return UpdateIniFileTreeErrors.ExpectedValue;
                                        }
                                    }
                                }
                            }
                        } else {
                            return UpdateIniFileTreeErrors.ExpectedValue;
                        }
                    }
                }
            }
        }
    }
}

fn maxThrottleRangeToPositiveThrottleMultiplier(properties: *StringHashMap(ArrayList(*Node)), allocator: Allocator) !void {
    var min_throttle_range = properties.get("MaxThrottleRange");
    if (min_throttle_range) |nodes| {
        for (nodes.items) |node| {
            // TODO: This node should be removed from properties["MinThrottleRange"] its list
            node.property = "PositiveThrottleMultiplier";
            if (node.value) |v| {
                const old_value = try parseFloat(f32, v);
                const new_value = fabs(1 + fabs(old_value));
                node.value = try allocPrint(allocator, "{d}", .{new_value});
            } else {
                return UpdateIniFileTreeErrors.ExpectedValue;
            }
        }
    }
}

fn minThrottleRangeToNegativeThrottleMultiplier(properties: *StringHashMap(ArrayList(*Node)), allocator: Allocator) !void {
    var min_throttle_range = properties.get("MinThrottleRange");
    if (min_throttle_range) |nodes| {
        for (nodes.items) |node| {
            // TODO: This node should be removed from properties["MinThrottleRange"] its list
            node.property = "NegativeThrottleMultiplier";
            if (node.value) |v| {
                const old_value = try parseFloat(f32, v);
                const new_value = fabs(1 - fabs(old_value));
                node.value = try allocPrint(allocator, "{d}", .{new_value});
            } else {
                return UpdateIniFileTreeErrors.ExpectedValue;
            }
        }
    }
}

fn writeIniFileTree(file_tree: *const IniFolder, output_folder_path: []const u8, allocator: Allocator) !void {
    for (file_tree.files.items) |file| {
        // std.debug.print("output_folder_path: '{s}', file.name: '{s}'\n", .{ output_folder_path, file.name });
        const file_path = try join(allocator, &.{ output_folder_path, file.name });
        // std.debug.print("file_path: '{s}'\n", .{file_path});
        try writeAst(&file.ast, file_path);
    }

    for (file_tree.folders.items) |folder| {
        const child_output_mod_path = try join(allocator, &.{ output_folder_path, folder.name });
        // std.debug.print("{s}\n", .{child_output_mod_path});
        try writeIniFileTree(&folder, child_output_mod_path, allocator);
    }
}

fn writeAst(ast: *const ArrayList(Node), output_path: []const u8) !void {
    const cwd = std.fs.cwd();
    const output_file = try cwd.createFile(output_path, .{});
    defer output_file.close();

    var buffered = bufferedWriter(output_file.writer());
    const buffered_writer = buffered.writer();

    for (ast.items, 0..) |*node, index| {
        try writeAstRecursively(node, buffered_writer, 0);

        // Doesn't add a trailing newline, because writeAstRecursively() already adds it
        if (node.property != null and index < ast.items.len - 1) {
            try writeBuffered(buffered_writer, "\n");
        }
    }

    try buffered.flush();
}

fn writeAstRecursively(node: *Node, buffered_writer: anytype, depth: usize) !void {
    // std.debug.print("{}\n", .{node});

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
    if (node.property) |property| {
        // std.debug.print("'{s}'\n", .{property});
        try writeBuffered(buffered_writer, property);
    }

    // Write value and equals to file
    if (node.value) |value| {
        // std.debug.print("' = '\n", .{});
        try writeBuffered(buffered_writer, " = ");

        // std.debug.print("'{s}'\n", .{value});
        try writeBuffered(buffered_writer, value);
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

    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);

    while (try tests_walker.next()) |entry| {
        var out_buffer: [MAX_PATH_BYTES]u8 = undefined;
        const dir_path = try entry.dir.realpath(".", &out_buffer);

        if (entry.kind == std.fs.File.Kind.file and eql(u8, entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'general/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and eql(u8, entry.basename, "in.ini"), dir_path });

            const input_path = try join(allocator, &.{ dir_path, "in.ini" });
            const expected_path = try join(allocator, &.{ dir_path, "out.ini" });

            const output_path = try join(allocator, &.{ tmpdir_path, "output.ini" });

            // std.debug.print("{s}\n{s}\n{s}\n\n", .{ input_path, expected_path, output_path });

            const text = try readFile(input_path, allocator);

            var tokens = try getTokens(text, allocator);

            var diagnostics: Diagnostics = .{};
            var ast = try getAstFromTokens(&tokens, allocator, &diagnostics);

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

test "lua_rules" {
    var tmpdir = tmpDir(.{});
    defer tmpdir.cleanup();

    var tmpdir_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    const tmpdir_path = try tmpdir.dir.realpath(".", &tmpdir_path_buffer);

    var iterable_tests = try std.fs.cwd().openIterableDir("tests/lua_rules", .{});
    defer iterable_tests.close();

    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);

    while (try tests_walker.next()) |entry| {
        var out_buffer: [MAX_PATH_BYTES]u8 = undefined;
        const dir_path = try entry.dir.realpath(".", &out_buffer);

        if (entry.kind == std.fs.File.Kind.file and eql(u8, entry.basename, "in.lua")) {
            std.debug.print("\nSubtest 'lua_rules/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});

            const input_path = try join(allocator, &.{ dir_path, "in.lua" });
            const expected_path = try join(allocator, &.{ dir_path, "out.lua" });
            const output_path = try join(allocator, &.{ tmpdir_path, "output.lua" });

            // std.debug.print("{s}\n{s}\n{s}\n\n", .{ input_path, expected_path, output_path });

            // const text = try readFile(input_path, allocator);

            _ = try updateFileAbsolute(input_path, output_path, .{});

            const lua_rules = try parseLuaRules(allocator);
            try applyLuaRules(lua_rules, tmpdir_path, allocator);

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

test "ini_rules" {
    var tmpdir = tmpDir(.{});
    defer tmpdir.cleanup();

    var tmpdir_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    const tmpdir_path = try tmpdir.dir.realpath(".", &tmpdir_path_buffer);

    var iterable_tests = try std.fs.cwd().openIterableDir("tests/ini_rules", .{});
    defer iterable_tests.close();

    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);

    while (try tests_walker.next()) |entry| {
        var out_buffer: [MAX_PATH_BYTES]u8 = undefined;
        const dir_path = try entry.dir.realpath(".", &out_buffer);

        if (entry.kind == std.fs.File.Kind.file and eql(u8, entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'ini_rules/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and eql(u8, entry.basename, "in.ini"), dir_path });

            const input_path = try join(allocator, &.{ dir_path, "in.ini" });
            const expected_path = try join(allocator, &.{ dir_path, "out.ini" });

            const output_path = try join(allocator, &.{ tmpdir_path, "output.ini" });

            // std.debug.print("{s}\n{s}\n{s}\n\n", .{ input_path, expected_path, output_path });

            const text = try readFile(input_path, allocator);

            var tokens = try getTokens(text, allocator);

            var diagnostics: Diagnostics = .{};
            var ast = try getAstFromTokens(&tokens, allocator, &diagnostics);

            var file_tree = IniFolder{
                .name = "",
                .files = ArrayList(IniFile).init(allocator),
                .folders = ArrayList(IniFolder).init(allocator),
            };

            try file_tree.files.append(IniFile{
                .name = "",
                .ast = ast,
            });

            var properties = StringHashMap(ArrayList(*Node)).init(allocator);
            try addProperties(&file_tree, &properties, allocator);

    		try bmpExtensionToPng(&properties, allocator);

            var property_value_pairs = HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage).init(allocator);
            try addPropertyValuePairs(&file_tree, &property_value_pairs, allocator);

            const ini_copy_of_rules = try parseIniCopyOfRules(allocator);
            try applyIniCopyOfRules(ini_copy_of_rules, &property_value_pairs);

            const ini_file_path_rules = try parseIniFilePathRules(allocator);
            try applyIniFilePathRules(ini_file_path_rules, &property_value_pairs);

            const ini_script_path_rules = try parseIniScriptPathRules(allocator);
            try applyIniScriptPathRules(ini_script_path_rules, &property_value_pairs);

            // TODO: Figure out a way to remove all these function calls that are already done by convert()
            // TODO: At the moment I am literally keeping both in sync manually

            const ini_property_rules = try parseIniPropertyRules(allocator);
            try applyIniPropertyRules(ini_property_rules, &properties);

            const ini_rules = try parseIniRules(allocator);
            applyIniRules(ini_rules, &property_value_pairs);

            const ini_sound_container_rules = try parseIniSoundContainerRules(allocator);
            applyIniSoundContainerRules(ini_sound_container_rules, &property_value_pairs);

            try updateIniFileTree(&properties, &property_value_pairs, allocator);

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

test "updated" {
    var tmpdir = tmpDir(.{});
    defer tmpdir.cleanup();

    var tmpdir_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    const tmpdir_path = try tmpdir.dir.realpath(".", &tmpdir_path_buffer);

    var iterable_tests = try std.fs.cwd().openIterableDir("tests/updated", .{});
    defer iterable_tests.close();

    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);

    while (try tests_walker.next()) |entry| {
        var out_buffer: [MAX_PATH_BYTES]u8 = undefined;
        const dir_path = try entry.dir.realpath(".", &out_buffer);

        if (entry.kind == std.fs.File.Kind.file and eql(u8, entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'updated/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and eql(u8, entry.basename, "in.ini"), dir_path });

            const input_path = try join(allocator, &.{ dir_path, "in.ini" });
            const expected_path = try join(allocator, &.{ dir_path, "out.ini" });

            const output_path = try join(allocator, &.{ tmpdir_path, "output.ini" });

            // std.debug.print("{s}\n{s}\n{s}\n\n", .{ input_path, expected_path, output_path });

            const text = try readFile(input_path, allocator);

            var tokens = try getTokens(text, allocator);

            var diagnostics: Diagnostics = .{};
            var ast = try getAstFromTokens(&tokens, allocator, &diagnostics);

            var file_tree = IniFolder{
                .name = "",
                .files = ArrayList(IniFile).init(allocator),
                .folders = ArrayList(IniFolder).init(allocator),
            };

            try file_tree.files.append(IniFile{
                .name = "",
                .ast = ast,
            });

            var properties = StringHashMap(ArrayList(*Node)).init(allocator);
            try addProperties(&file_tree, &properties, allocator);

    		try bmpExtensionToPng(&properties, allocator);

            var property_value_pairs = HashMap(PropertyValuePair, ArrayList(*Node), PropertyValuePairContext, default_max_load_percentage).init(allocator);
            try addPropertyValuePairs(&file_tree, &property_value_pairs, allocator);

            try updateIniFileTree(&properties, &property_value_pairs, allocator);

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

    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);

    while (try tests_walker.next()) |entry| {
        var out_buffer: [MAX_PATH_BYTES]u8 = undefined;
        const dir_path = try entry.dir.realpath(".", &out_buffer);

        if (entry.kind == std.fs.File.Kind.file and eql(u8, entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'invalid/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and eql(u8, entry.basename, "in.ini"), dir_path });

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

    var diagnostics: Diagnostics = .{};
    var ast = try getAstFromTokens(&tokens, allocator, &diagnostics);
    _ = ast;

    // Tests from the invalid/ folder should always return an error from this function before reaching this
    unreachable;
}
