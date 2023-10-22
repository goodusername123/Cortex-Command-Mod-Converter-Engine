const std = @import("std");
const ziplib = @cImport(@cInclude("zip.h"));

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

pub const Diagnostics = struct {
    file_path: ?[]const u8 = null,
    token: ?[]const u8 = null,
    line: ?i32 = null,
    column: ?i32 = null,
};

const PropertyValuePair = struct {
    property: []const u8,
    value: []const u8,
};

const UpdateIniFileTreeErrors = error{
    ExpectedValue,
};

pub fn main() !void {
    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    const cwd = std.fs.cwd();

    var input_folder_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    // const input_folder_path = try cwd.realpath("tests/mod/in", &input_folder_path_buffer);
    const input_folder_path = try cwd.realpath("I:/Programming/Cortex-Command-Community-Project-Data/LegacyModConverter-v1.0-pre5.2/Input", &input_folder_path_buffer);
    // const input_folder_path = try cwd.realpath("I:/Programming/Cortex-Command-Mod-Converter-Engine/tons_of_mods/extra", &input_folder_path_buffer);
    // const input_folder_path = try cwd.realpath("I:/Games/CCCP Pre5.2 - Copy/foo", &input_folder_path_buffer);

    var output_folder_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
    // const output_folder_path = try cwd.realpath("tests/mod/out", &output_folder_path_buffer);
    const output_folder_path = try cwd.realpath("I:/Programming/Cortex-Command-Community-Project-Data/Mods", &output_folder_path_buffer);
    // const output_folder_path = try cwd.realpath("I:/Programming/Cortex-Command-Community-Project-Data/Mods/tmp", &output_folder_path_buffer);
    // const output_folder_path = try cwd.realpath("I:/Games/CCCP Pre5.2 - Copy/Mods", &output_folder_path_buffer);

    var diagnostics: Diagnostics = .{};
    convert(
        input_folder_path,
        output_folder_path,
        allocator,
        &diagnostics,
    ) catch |err| switch (err) {
        error.UnexpectedToken => {
            const token = diagnostics.token orelse "null";
            const file_path = diagnostics.file_path orelse "null";
            const line = diagnostics.line orelse -1;
            const column = diagnostics.column orelse -1;

            std.debug.print("Error: Unexpected '{s}' at {s}:{}:{}\n", .{
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

            std.debug.print("Error: Too many tabs at {s}:{}:{}\n", .{
                file_path,
                line,
                column,
            });
        },
        else => |e| return e,
    };

    try zip_mods(input_folder_path, output_folder_path, allocator);
}

pub fn convert(input_folder_path: []const u8, output_folder_path: []const u8, allocator: Allocator, diagnostics: *Diagnostics) !void {
    std.debug.print("Making all output dirs...\n", .{});
    try makeOutputDirs(input_folder_path, output_folder_path, allocator);

    std.debug.print("Copying files...\n", .{});
    try copyFiles(input_folder_path, output_folder_path, allocator);

    const lua_rules = try parseLuaRules(allocator);
    std.debug.print("Applying Lua rules...\n", .{});
    try applyLuaRules(lua_rules, output_folder_path, allocator);

    std.debug.print("Getting INI file tree...\n", .{});
    var file_tree = try getIniFileTree(input_folder_path, allocator, diagnostics);

    try applyOnNodes(pathToFilePath, &file_tree);

    // It also HAS to be called before applyIniFilePathRules(),
    // because otherwise this could happen:
    // The game reports that Base.rte/foo.png doesn't exist,
    // so the user enters this ini_file_path_rules.json rule:
    // "Base.rte/foo.png": "Base.rte/bar.png"
    // The game reports that Base.rte/foo.png *still* doesn't exist,
    // due to the parsed input mod containing "Base.rte/foo.bmp"!
    std.debug.print("Bmp extension to png...\n", .{});
    try applyOnNodesAlloc(bmpExtensionToPng, &file_tree, allocator);

    std.debug.print("Wav extension to flac...\n", .{});
    try applyOnNodesAlloc(wavExtensionToFlac, &file_tree, allocator);

    const ini_copy_of_rules = try parseIniCopyOfRules(allocator);
    std.debug.print("Applying INI CopyOf rules...\n", .{});
    applyIniCopyOfRules(ini_copy_of_rules, &file_tree);

    const ini_file_path_rules = try parseIniFilePathRules(allocator);
    std.debug.print("Applying INI FilePath rules...\n", .{});
    applyIniFilePathRules(ini_file_path_rules, &file_tree);

    const ini_script_path_rules = try parseIniScriptPathRules(allocator);
    std.debug.print("Applying INI ScriptPath rules...\n", .{});
    applyIniScriptPathRules(ini_script_path_rules, &file_tree);

    const ini_property_rules = try parseIniPropertyRules(allocator);
    std.debug.print("Applying INI property rules...\n", .{});
    applyIniPropertyRules(ini_property_rules, &file_tree);

    const ini_rules = try parseIniRules(allocator);
    std.debug.print("Applying INI rules...\n", .{});
    applyIniRules(ini_rules, &file_tree);

    const ini_sound_container_rules = try parseIniSoundContainerRules(allocator);
    std.debug.print("Applying INI SoundContainer rules...\n", .{});
    applyIniSoundContainerRules(ini_sound_container_rules, &file_tree);

    std.debug.print("Updating INI file tree...\n", .{});
    try updateIniFileTree(&file_tree, allocator);

    std.debug.print("Writing INI file tree...\n", .{});
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
            if (strEql(extension(entry.name), ".bmp")) {
                const input_file_path = try join(allocator, &.{ input_folder_path, entry.name });

                var output_name = try allocator.dupe(u8, entry.name);

                output_name[output_name.len - 1] = 'g';
                output_name[output_name.len - 2] = 'n';
                output_name[output_name.len - 3] = 'p';

                const output_file_path = try join(allocator, &.{ output_folder_path, output_name });

                const output_file_access = std.fs.accessAbsolute(output_file_path, .{});

                if (output_file_access == error.FileNotFound) {
                    try convertBmpToPng(input_file_path, output_file_path, allocator);
                } else if (output_file_access catch null) |_| { // Else if there was no access error
                    // TODO: Windows can be significantly faster if we use iterable_dir.dir.stat() manually here,
                    // if (and only if) directory mod times are updated when files change on Windows!

                    const input_stat = try iterable_dir.dir.statFile(input_file_path);
                    const output_stat = try iterable_dir.dir.statFile(output_file_path);

                    // std.debug.print("{}\n{}\n\n", .{ input_stat.mtime, output_stat.mtime });

                    if (input_stat.mtime > output_stat.mtime) {
                        // std.debug.print("Converted bmp to png\n", .{});
                        // TODO: Figure out whether a different function should be called in this case,
                        // similar to below where updateFileAbsolute() can be called instead of copyFileAbsolute()
                        try convertBmpToPng(input_file_path, output_file_path, allocator);
                    }
                } else { // Else return the access error
                    return output_file_access;
                }
            } else if (strEql(extension(entry.name), ".wav")) {
                const input_file_path = try join(allocator, &.{ input_folder_path, entry.name });

                // Create a copy of the entry name that is one character longer, so the "c" in .flac fits
                var output_name = try allocator.alloc(u8, entry.name.len + 1);
                @memcpy(output_name[0..entry.name.len], entry.name);

                output_name[output_name.len - 1] = 'c';
                output_name[output_name.len - 2] = 'a';
                output_name[output_name.len - 3] = 'l';
                output_name[output_name.len - 4] = 'f';

                const output_file_path = try join(allocator, &.{ output_folder_path, output_name });

                const output_file_access = std.fs.accessAbsolute(output_file_path, .{});

                if (output_file_access == error.FileNotFound) {
                    try convertWavToFlac(input_file_path, output_file_path, allocator);
                } else if (output_file_access catch null) |_| { // Else if there was no access error
                    // TODO: Windows can be significantly faster if we use iterable_dir.dir.stat() manually here,
                    // if (and only if) directory mod times are updated when files change on Windows!

                    const input_stat = try iterable_dir.dir.statFile(input_file_path);
                    const output_stat = try iterable_dir.dir.statFile(output_file_path);

                    if (input_stat.mtime > output_stat.mtime) {
                        // std.debug.print("Converted wav to flac\n", .{});
                        // TODO: Figure out whether a different function should be called in this case,
                        // similar to below where updateFileAbsolute() can be called instead of copyFileAbsolute()
                        try convertWavToFlac(input_file_path, output_file_path, allocator);
                    }
                } else { // Else return the access error
                    return output_file_access;
                }
            } else if (!strEql(extension(entry.name), ".ini")) {
                const input_file_path = try join(allocator, &.{ input_folder_path, entry.name });
                const output_file_path = try join(allocator, &.{ output_folder_path, entry.name });

                const output_file_access = std.fs.accessAbsolute(output_file_path, .{});

                if (output_file_access == error.FileNotFound) {
                    try copyFileAbsolute(input_file_path, output_file_path, .{});
                } else if (output_file_access catch null) |_| { // Else if there was no access error
                    // TODO: Windows can be significantly faster if we use iterable_dir.dir.stat() manually here,
                    // if (and only if) directory mod times are updated when files change on Windows!

                    const input_stat = try iterable_dir.dir.statFile(input_file_path);
                    const output_stat = try iterable_dir.dir.statFile(output_file_path);

                    if (input_stat.mtime > output_stat.mtime) {
                        // std.debug.print("Copied something else\n", .{});
                        // TODO: Reverify that this is faster than the plain copyFileAbsolute()
                        _ = try updateFileAbsolute(input_file_path, output_file_path, .{});
                    }
                } else { // Else return the access error
                    return output_file_access;
                }
            }
        } else if (entry.kind == std.fs.File.Kind.directory) {
            const child_input_folder_path = try join(allocator, &.{ input_folder_path, entry.name });
            const child_output_folder_path = try join(allocator, &.{ output_folder_path, entry.name });
            try copyFiles(child_input_folder_path, child_output_folder_path, allocator);
        }
    }
}

fn strEql(str1: []const u8, str2: []const u8) bool {
    return eql(u8, str1, str2);
}

fn convertBmpToPng(input_file_path: []const u8, output_file_path: []const u8, allocator: Allocator) !void {
    // TODO: ffmpeg won't always be available, so include its source code and call that instead
    var argv = [_][]const u8{ "ffmpeg", "-i", input_file_path, output_file_path, "-y" };
    const result = try std.ChildProcess.exec(.{ .argv = &argv, .allocator = allocator });
    _ = result;
}

fn convertWavToFlac(input_file_path: []const u8, output_file_path: []const u8, allocator: Allocator) !void {
    // TODO: ffmpeg won't always be available, so include its source code and call that instead
    var argv = [_][]const u8{ "ffmpeg", "-i", input_file_path, output_file_path, "-y" };
    const result = try std.ChildProcess.exec(.{ .argv = &argv, .allocator = allocator });
    _ = result;

    // var line_iter = std.mem.split(u8, result.stderr, "\n");
    // while (line_iter.next()) |line| {
    //     if (line.len == 0) continue;
    //     std.debug.print("{s}\n", .{line});
    // }
    // std.debug.print("\n", .{});
}

fn parseLuaRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const text = try readFile(parentDir() ++ "/lua_rules.json", allocator);

    var scanner = Scanner.initCompleteInput(allocator, text);

    var lua_rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return lua_rules;
}

inline fn parentDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}

fn applyLuaRules(lua_rules: std.json.ArrayHashMap([]const u8), folder_path: []const u8, allocator: Allocator) !void {
    var iterable_dir = try std.fs.openIterableDirAbsolute(folder_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |dir_entry| {
        if (dir_entry.kind == std.fs.File.Kind.file) {
            if (strEql(extension(dir_entry.name), ".lua")) {
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
            if (strEql(extension(entry.name), ".ini")) {
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

    var seen_property = false;

    while (slice.len > 0) {
        const token = getToken(&slice, &multiline_comment_depth, &seen_property);
        // std.debug.print("'{s}'\t\t{}\n", .{ fmtSliceEscapeUpper(token.slice), token.type });
        try tokens.append(token);
    }

    if (multiline_comment_depth > 0) {
        return TokenError.UnclosedMultiComment;
    }

    return tokens;
}

fn getToken(slice: *[]const u8, multiline_comment_depth: *i32, seen_property: *bool) Token {
    // TODO: Consistently use either while-loops or for-loops everywhere in this function

    if (slice.*[0] == '\n') {
        seen_property.* = false;
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
        if (slice.*[end_index] == '=' and !seen_property.*) {
            break;
        }
        if (slice.*[end_index] == '\n' or slice.*[end_index] == '\t' or (slice.*[end_index] == '/' and (slice.*[end_index + 1] == '*' or slice.*[end_index + 1] == '/'))) {
            break;
        }
        if (slice.*[end_index] != ' ') {
            sentence_end_index = end_index + 1;
        }
    }

    seen_property.* = true;
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

const nodeCallbackDef = fn (node: *Node) error{ ExpectedValue, InvalidCharacter, OutOfMemory }!void;

fn applyOnNodes(comptime nodeCallbackFn: nodeCallbackDef, file_tree: *IniFolder) !void {
    for (file_tree.folders.items) |*folder| {
        try applyOnNodes(nodeCallbackFn, folder);
    }

    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            try applyOnNode(nodeCallbackFn, node);
        }
    }
}

fn applyOnNode(comptime nodeCallbackFn: nodeCallbackDef, node: *Node) !void {
    try nodeCallbackFn(node);

    for (node.children.items) |*child| {
        try applyOnNode(nodeCallbackFn, child);
    }
}

const nodeAllocCallbackDef = fn (node: *Node, allocator: Allocator) error{ ExpectedValue, InvalidCharacter, OutOfMemory }!void;

fn applyOnNodesAlloc(comptime nodeAllocCallbackFn: nodeAllocCallbackDef, file_tree: *IniFolder, allocator: Allocator) !void {
    for (file_tree.folders.items) |*folder| {
        try applyOnNodesAlloc(nodeAllocCallbackFn, folder, allocator);
    }

    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            try applyOnNodeAlloc(nodeAllocCallbackFn, node, allocator);
        }
    }
}

fn applyOnNodeAlloc(comptime nodeAllocCallbackFn: nodeAllocCallbackDef, node: *Node, allocator: Allocator) !void {
    try nodeAllocCallbackFn(node, allocator);

    for (node.children.items) |*child| {
        try applyOnNodeAlloc(nodeAllocCallbackFn, child, allocator);
    }
}

fn pathToFilePath(node: *Node) !void {
    if (node.property) |property| {
        if (strEql(property, "Path")) {
            node.property = "FilePath";
        }
    }
}

fn bmpExtensionToPng(node: *Node, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "FilePath")) {
            if (node.value) |path| {
                if (endsWith(u8, path, ".bmp") and !strEql(path, "palette.bmp") and !strEql(path, "palettemat.bmp")) {
                    // We have to dupe, since the u8s in path are const
                    var new_path = try allocator.dupe(u8, path);

                    new_path[new_path.len - 1] = 'g';
                    new_path[new_path.len - 2] = 'n';
                    new_path[new_path.len - 3] = 'p';

                    node.value = new_path;
                }
            }
        }
    }
}

fn wavExtensionToFlac(node: *Node, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "FilePath")) {
            if (node.value) |path| {
                if (endsWith(u8, path, ".wav")) {
                    // Create a copy of the entry name that is one character longer, so the "c" in .flac fits
                    var new_path = try allocator.alloc(u8, path.len + 1);
                    @memcpy(new_path[0..path.len], path);

                    new_path[new_path.len - 1] = 'c';
                    new_path[new_path.len - 2] = 'a';
                    new_path[new_path.len - 3] = 'l';
                    new_path[new_path.len - 4] = 'f';

                    node.value = new_path;
                }
            }
        }
    }
}

fn parseIniCopyOfRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const text = try readFile(parentDir() ++ "/ini_copy_of_rules.json", allocator);

    var scanner = Scanner.initCompleteInput(allocator, text);

    var rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return rules;
}

fn applyIniCopyOfRules(rules: std.json.ArrayHashMap([]const u8), file_tree: *IniFolder) void {
    var map_iterator = rules.map.iterator();
    while (map_iterator.next()) |map_entry| {
        const old_value = map_entry.key_ptr.*;
        const new_value = map_entry.value_ptr.*;

        applyIniValueReplacementRulesRecursivelyFolder(file_tree, "CopyOf", old_value, new_value);
    }
}

fn applyIniValueReplacementRulesRecursivelyFolder(file_tree: *IniFolder, comptime property: []const u8, old_value: []const u8, new_value: []const u8) void {
    for (file_tree.folders.items) |*folder| {
        applyIniValueReplacementRulesRecursivelyFolder(folder, property, old_value, new_value);
    }

    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            applyIniValueReplacementRulesRecursivelyNode(node, property, old_value, new_value);
        }
    }
}

fn applyIniValueReplacementRulesRecursivelyNode(node: *Node, comptime property: []const u8, old_value: []const u8, new_value: []const u8) void {
    if (node.property) |node_property| {
        if (strEql(node_property, property)) {
            if (node.value) |value| {
                if (strEql(value, old_value)) {
                    node.value = new_value;
                }
            }
        }
    }

    for (node.children.items) |*child| {
        applyIniValueReplacementRulesRecursivelyNode(child, property, old_value, new_value);
    }
}

fn parseIniFilePathRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const text = try readFile(parentDir() ++ "/ini_file_path_rules.json", allocator);

    var scanner = Scanner.initCompleteInput(allocator, text);

    var rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return rules;
}

fn applyIniFilePathRules(rules: std.json.ArrayHashMap([]const u8), file_tree: *IniFolder) void {
    var map_iterator = rules.map.iterator();
    while (map_iterator.next()) |map_entry| {
        const old_value = map_entry.key_ptr.*;
        const new_value = map_entry.value_ptr.*;

        applyIniValueReplacementRulesRecursivelyFolder(file_tree, "FilePath", old_value, new_value);
    }
}

fn parseIniScriptPathRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const text = try readFile(parentDir() ++ "/ini_script_path_rules.json", allocator);

    var scanner = Scanner.initCompleteInput(allocator, text);

    var rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return rules;
}

fn applyIniScriptPathRules(rules: std.json.ArrayHashMap([]const u8), file_tree: *IniFolder) void {
    var map_iterator = rules.map.iterator();
    while (map_iterator.next()) |map_entry| {
        const old_value = map_entry.key_ptr.*;
        const new_value = map_entry.value_ptr.*;

        applyIniValueReplacementRulesRecursivelyFolder(file_tree, "ScriptPath", old_value, new_value);
    }
}

fn parseIniPropertyRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const text = try readFile(parentDir() ++ "/ini_property_rules.json", allocator);

    var scanner = Scanner.initCompleteInput(allocator, text);

    var ini_property_rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return ini_property_rules;
}

fn applyIniPropertyRules(ini_property_rules: std.json.ArrayHashMap([]const u8), file_tree: *IniFolder) void {
    var map_iterator = ini_property_rules.map.iterator();
    while (map_iterator.next()) |map_entry| {
        const old_property = map_entry.key_ptr.*;
        const new_property = map_entry.value_ptr.*;

        applyIniPropertyRulesRecursivelyFolder(file_tree, old_property, new_property);
    }
}

fn applyIniPropertyRulesRecursivelyFolder(file_tree: *IniFolder, old_property: []const u8, new_property: []const u8) void {
    for (file_tree.folders.items) |*folder| {
        applyIniPropertyRulesRecursivelyFolder(folder, old_property, new_property);
    }

    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            applyIniPropertyRulesRecursivelyNode(node, old_property, new_property);
        }
    }
}

fn applyIniPropertyRulesRecursivelyNode(node: *Node, old_property: []const u8, new_property: []const u8) void {
    if (node.property) |node_property| {
        if (strEql(node_property, old_property)) {
            node.property = new_property;
        }
    }

    for (node.children.items) |*child| {
        applyIniPropertyRulesRecursivelyNode(child, old_property, new_property);
    }
}

fn parseIniRules(allocator: Allocator) ![]Rule {
    const text = try readFile(parentDir() ++ "/ini_rules.json", allocator);
    return try parseFromSliceLeaky([]Rule, allocator, text, .{});
}

fn applyIniRules(ini_rules: []Rule, file_tree: *IniFolder) void {
    for (ini_rules) |*rule| {
        applyIniRulesRecursivelyFolder(file_tree, rule);
    }
}

fn applyIniRulesRecursivelyFolder(file_tree: *IniFolder, rule: *Rule) void {
    for (file_tree.folders.items) |*folder| {
        applyIniRulesRecursivelyFolder(folder, rule);
    }

    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            applyIniRulesRecursivelyNode(node, rule);
        }
    }
}

fn applyIniRulesRecursivelyNode(node: *Node, rule: *Rule) void {
    if (node.property) |node_property| {
        if (strEql(node_property, rule.old_property)) {
            if (node.value) |node_value| {
                if (strEql(node_value, rule.old_value)) {
                    node.property = rule.new_property;
                    node.value = rule.new_value;
                }
            }
        }
    }

    for (node.children.items) |*child| {
        applyIniRulesRecursivelyNode(child, rule);
    }
}

fn parseIniSoundContainerRules(allocator: Allocator) ![][]const u8 {
    const text = try readFile(parentDir() ++ "/ini_sound_container_rules.json", allocator);
    return try parseFromSliceLeaky([][]const u8, allocator, text, .{});
}

fn applyIniSoundContainerRules(ini_sound_container_rules: [][]const u8, file_tree: *IniFolder) void {
    for (ini_sound_container_rules) |property| {
        applyIniSoundContainerRulesRecursivelyFolder(file_tree, property);
    }
}

fn applyIniSoundContainerRulesRecursivelyFolder(file_tree: *IniFolder, property: []const u8) void {
    for (file_tree.folders.items) |*folder| {
        applyIniSoundContainerRulesRecursivelyFolder(folder, property);
    }

    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            applyIniSoundContainerRulesRecursivelyNode(node, property);
        }
    }
}

fn applyIniSoundContainerRulesRecursivelyNode(node: *Node, property: []const u8) void {
    if (node.property) |node_property| {
        if (strEql(node_property, property)) {
            if (node.value) |node_value| {
                if (strEql(node_value, "Sound")) {
                    node.value = "SoundContainer";
                }
            }
        }
    }

    for (node.children.items) |*child| {
        applyIniSoundContainerRulesRecursivelyNode(child, property);
    }
}

fn updateIniFileTree(file_tree: *IniFolder, allocator: Allocator) !void {
    try applyOnNodesAlloc(addGripStrength, file_tree, allocator);
    try applyOnNodesAlloc(addOrUpdateSupportedGameVersion, file_tree, allocator);
    try applyOnNodes(aemitterFuelToPemitter, file_tree);

    // AEJetpacks being made first-class citizens by Causeless
    {
        try aemitterToAejetpack(file_tree, file_tree, allocator);
        try moveJetpackModifiers(file_tree, file_tree, allocator);
        try copyJetpack(file_tree, file_tree, allocator);
        try moveJetpackModifiers(file_tree, file_tree, allocator);
        try removeJetpackModifiersFromActors(file_tree, file_tree, allocator);
    }

    try applyOnNodesAlloc(maxLengthToOffsets, file_tree, allocator);
    try applyOnNodesAlloc(maxMassToMaxInventoryMass, file_tree, allocator);
    try applyOnNodesAlloc(maxThrottleRangeToPositiveThrottleMultiplier, file_tree, allocator);
    try applyOnNodesAlloc(minThrottleRangeToNegativeThrottleMultiplier, file_tree, allocator);

    try pieMenu("ACDropShip", "Default Craft Pie Menu", 2, 1, 1, 1, file_tree, allocator);
    try pieMenu("ACrab", "Default Crab Pie Menu", 2, 2, 2, 2, file_tree, allocator);
    try pieMenu("ACRocket", "Default Craft Pie Menu", 2, 1, 1, 1, file_tree, allocator);
    try pieMenu("Actor", "Default Actor Pie Menu", 1, 0, 0, 0, file_tree, allocator);
    try pieMenu("AHuman", "Default Human Pie Menu", 2, 2, 2, 2, file_tree, allocator);
    try pieMenu("Turret", "Default Turret Pie Menu", 2, 0, 0, 1, file_tree, allocator);

    try applyOnNodesAlloc(removeSlTerrainProperties, file_tree, allocator);
    try applyOnNodes(shovelFlashFix, file_tree);
}

fn addGripStrength(node: *Node, allocator: Allocator) !void {
    if (node.property) |node_property| {
        if (strEql(node_property, "AddActor")) {
            if (node.value) |node_value| {
                if (strEql(node_value, "Arm")) {
                    var children = &node.children;

                    for (children.items) |child| {
                        if (child.property) |property| {
                            if (strEql(property, "GripStrength")) {
                                return;
                            }
                        }
                    }

                    try children.append(Node{
                        .property = "GripStrength",
                        .value = "424242",
                        .comments = ArrayList([]const u8).init(allocator),
                        .children = ArrayList(Node).init(allocator),
                    });
                }
            }
        }
    }
}

fn addOrUpdateSupportedGameVersion(node: *Node, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "DataModule")) {
            var has_supported_game_version = false;

            for (node.children.items) |*child| {
                if (child.property) |child_property| {
                    if (strEql(child_property, "SupportedGameVersion")) {
                        has_supported_game_version = true;

                        if (child.value) |child_value| {
                            if (!strEql(child_value, "Pre-Release 5.0")) {
                                child.value = "Pre-Release 5.0";
                            }
                        }
                    }
                }
            }

            if (!has_supported_game_version) {
                try node.children.append(Node{
                    .property = "SupportedGameVersion",
                    .value = "Pre-Release 5.0",
                    .comments = ArrayList([]const u8).init(allocator),
                    .children = ArrayList(Node).init(allocator),
                });
            }
        }
    }
}

fn aemitterFuelToPemitter(node: *Node) !void {
    if (node.property) |property| {
        if (strEql(property, "GibParticle")) {
            if (node.value) |value| {
                if (strEql(value, "AEmitter")) {
                    for (node.children.items) |*child| {
                        if (child.property) |child_property| {
                            if (strEql(child_property, "CopyOf")) {
                                if (child.value) |child_value| {
                                    if (strEql(child_value, "Fuel Fire Trace Black")) {
                                        node.value = "PEmitter";
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
    }
}

fn aemitterToAejetpack(folder: *IniFolder, file_tree: *IniFolder, allocator: Allocator) !void {
    for (folder.folders.items) |*subfolder| {
        try aemitterToAejetpack(subfolder, file_tree, allocator);
    }

    for (folder.files.items) |file| {
        for (file.ast.items) |*node| {
            try aemitterToAejetpackRecursivelyNode(node, file_tree, allocator);
        }
    }
}

// Translate "Jetpack = AEmitter" to "Jetpack = AEJetpack",
// and its "AddEffect = AEmitter" children to "AddEffect = AEJetpack"
fn aemitterToAejetpackRecursivelyNode(node: *Node, file_tree: *IniFolder, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "Jetpack")) {
            if (node.value) |value| {
                if (strEql(value, "AEmitter")) {
                    node.value = "AEJetpack";

                    try addEffectAemitterToAddEffectAejetpackCopyOfFinder(node, file_tree);
                }
            }
        }
    }

    for (node.children.items) |*child| {
        try aemitterToAejetpackRecursivelyNode(child, file_tree, allocator);
    }
}

fn addEffectAemitterToAddEffectAejetpackCopyOfFinder(node: *Node, file_tree: *IniFolder) error{ExpectedValue}!void {
    for (node.children.items) |*node_child| {
        if (node_child.property) |node_child_property| {
            if (strEql(node_child_property, "CopyOf")) {
                if (node_child.value) |preset_name| {
                    try addEffectAemitterToAddEffectAejetpackRecursivelyFolder(file_tree, file_tree, preset_name);
                } else {
                    return UpdateIniFileTreeErrors.ExpectedValue;
                }
            }
        }
    }
}

fn addEffectAemitterToAddEffectAejetpackRecursivelyFolder(folder: *IniFolder, file_tree: *IniFolder, preset_name: []const u8) !void {
    for (folder.folders.items) |*subfolder| {
        try addEffectAemitterToAddEffectAejetpackRecursivelyFolder(subfolder, file_tree, preset_name);
    }

    for (folder.files.items) |file| {
        for (file.ast.items) |*node| {
            try addEffectAemitterToAddEffectAejetpackRecursivelyNode(node, file_tree, preset_name);
        }
    }
}

fn addEffectAemitterToAddEffectAejetpackRecursivelyNode(node: *Node, file_tree: *IniFolder, preset_name: []const u8) !void {
    if (node.property) |property| {
        if (strEql(property, "AddEffect")) {
            if (node.value) |value| {
                if (strEql(value, "AEmitter")) {
                    for (node.children.items) |*child| {
                        if (child.property) |child_property| {
                            if (strEql(child_property, "PresetName")) {
                                if (child.value) |child_value| {
                                    if (strEql(child_value, preset_name)) {
                                        node.value = "AEJetpack";

                                        try addEffectAemitterToAddEffectAejetpackCopyOfFinder(node, file_tree);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    for (node.children.items) |*child| {
        try addEffectAemitterToAddEffectAejetpackRecursivelyNode(child, file_tree, preset_name);
    }
}

fn moveJetpackModifiers(folder: *IniFolder, file_tree: *IniFolder, allocator: Allocator) !void {
    for (folder.folders.items) |*subfolder| {
        try moveJetpackModifiers(subfolder, file_tree, allocator);
    }

    for (folder.files.items) |file| {
        for (file.ast.items) |*node| {
            try moveJetpackModifiersRecursivelyNode(node, file_tree, allocator);
        }
    }
}

// Move the Actor's jetpack modifiers to its last "Jetpack = AEJetpack",
// where the modifiers get pushed to the back of the AEJetpack if they came after it,
// and inserted at the front of it if they came before it.
// Make sure to do this by looping through the Actor's keys from back-to-front,
// stopping the loop if a CopyOf of an Actor is encountered.
fn moveJetpackModifiersRecursivelyNode(node: *Node, file_tree: *IniFolder, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "AddActor")) {
            if (node.value) |value| {
                if (strEql(value, "ACrab") or strEql(value, "AHuman")) {
                    var contains_aejetpack = false;
                    for (node.children.items) |*child| {
                        if (child.property) |child_property| {
                            if (strEql(child_property, "Jetpack")) {
                                if (child.value) |child_value| {
                                    if (strEql(child_value, "AEJetpack")) {
                                        contains_aejetpack = true;
                                    }
                                    break;
                                }
                            }
                        }
                    }

                    if (contains_aejetpack) {
                        var modifier_index = node.children.items.len;
                        while (modifier_index > 0) {
                            modifier_index -= 1;
                            const modifier = node.children.items[modifier_index];
                            if (modifier.property) |modifier_property| {
                                if (isJetpackModifier(modifier_property)) {
                                    for (node.children.items, 0..) |*aejetpack, aejetpack_index| {
                                        if (aejetpack.property) |aejetpack_property| {
                                            if (strEql(aejetpack_property, "Jetpack")) {
                                                if (aejetpack.value) |aejetpack_value| {
                                                    if (strEql(aejetpack_value, "AEJetpack")) {
                                                        if (aejetpack_index > modifier_index) {
                                                            try aejetpack.children.insert(0, modifier);
                                                        } else {
                                                            try aejetpack.children.append(modifier);
                                                        }
                                                    }
                                                    break;
                                                }
                                            }
                                        }
                                    }

                                    _ = node.children.orderedRemove(modifier_index);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    for (node.children.items) |*child| {
        try moveJetpackModifiersRecursivelyNode(child, file_tree, allocator);
    }
}

fn copyJetpack(folder: *IniFolder, file_tree: *IniFolder, allocator: Allocator) !void {
    for (folder.folders.items) |*subfolder| {
        try copyJetpack(subfolder, file_tree, allocator);
    }

    for (folder.files.items) |file| {
        for (file.ast.items) |*node| {
            try copyJetpackRecursivelyNode(node, file_tree, allocator);
        }
    }
}

// Looping back-to-front through every ACrab and AHuman Actor child node, if
// 1. we first encounter the Actor containing any of the six Jetpack modifiers, and
// 2. after that we encounter it CopyOfs an Actor that has a Jetpack with the value AEJetpack,
// before our Actor has any "Jetpack =", .insert() that Jetpack in our own Actor
// directly after the CopyOf.
// The CopyOf has to be searched recursively, since a copied Jetpack may be in a CopyOf of a CopyOf.
fn copyJetpackRecursivelyNode(node: *Node, file_tree: *IniFolder, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "AddActor")) {
            if (node.value) |value| {
                if (strEql(value, "ACrab") or strEql(value, "AHuman")) {
                    var seen_jetpack_modifier = false;
                    var i = node.children.items.len;
                    while (i > 0) {
                        i -= 1;
                        const child = node.children.items[i];
                        if (child.property) |child_property| {
                            if (strEql(child_property, "Jetpack")) {
                                break;
                            } else if (isJetpackModifier(child_property)) {
                                seen_jetpack_modifier = true;
                            } else if (strEql(child_property, "CopyOf")) {
                                if (child.value) |preset_name| {
                                    if (findJetpackRecursivelyFolder(file_tree, file_tree, preset_name)) |jetpack| {
                                        const jetpack_deepcopy = try deepCopyNode(jetpack, allocator);
                                        try node.children.insert(i + 1, jetpack_deepcopy);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    for (node.children.items) |*child| {
        try copyJetpackRecursivelyNode(child, file_tree, allocator);
    }
}

fn deepCopyNode(input_node: *Node, allocator: Allocator) !Node {
    var returned_node = Node{
        .property = if (input_node.property) |property| try allocator.dupe(u8, property) else null,
        .value = if (input_node.value) |value| try allocator.dupe(u8, value) else null,
        .comments = ArrayList([]const u8).init(allocator),
        .children = ArrayList(Node).init(allocator),
    };

    for (input_node.comments.items) |comment| {
        try returned_node.comments.append(try allocator.dupe(u8, comment));
    }

    for (input_node.children.items) |*child| {
        try returned_node.children.append(try deepCopyNode(child, allocator));
    }

    return returned_node;
}

fn removeJetpackModifiersFromActors(folder: *IniFolder, file_tree: *IniFolder, allocator: Allocator) !void {
    for (folder.folders.items) |*subfolder| {
        try removeJetpackModifiersFromActors(subfolder, file_tree, allocator);
    }

    for (folder.files.items) |file| {
        for (file.ast.items) |*node| {
            try removeJetpackModifiersFromActorsRecursivelyNode(node, file_tree, allocator);
        }
    }
}

fn removeJetpackModifiersFromActorsRecursivelyNode(node: *Node, file_tree: *IniFolder, allocator: Allocator) !void {
    // TODO:

    for (node.children.items) |*child| {
        try removeJetpackModifiersFromActorsRecursivelyNode(child, file_tree, allocator);
    }
}

fn isJetpackModifier(property: []const u8) bool {
    return strEql(property, "JumpTime") or strEql(property, "JetTime") or strEql(property, "JumpReplenishRate") or strEql(property, "JetReplenishRate") or strEql(property, "JumpAngleRange") or strEql(property, "JetAngleRange");
}

fn findJetpackRecursivelyFolder(folder: *IniFolder, file_tree: *IniFolder, preset_name: []const u8) ?*Node {
    for (folder.folders.items) |*subfolder| {
        if (findJetpackRecursivelyFolder(subfolder, file_tree, preset_name)) |returned| {
            return returned;
        }
    }

    for (folder.files.items) |file| {
        for (file.ast.items) |*node| {
            if (findJetpackRecursivelyNode(node, file_tree, preset_name)) |returned| {
                return returned;
            }
        }
    }

    return null;
}

fn findJetpackRecursivelyNode(node: *Node, file_tree: *IniFolder, preset_name: []const u8) ?*Node {
    if (node.property) |property| {
        if (strEql(property, "AddActor")) {
            if (node.value) |value| {
                if (strEql(value, "ACrab") or strEql(value, "AHuman")) {
                    var found_searched_for_preset_name = false;
                    for (node.children.items) |*child| {
                        if (child.property) |child_property| {
                            if (strEql(child_property, "PresetName")) {
                                if (child.value) |child_value| {
                                    if (strEql(child_value, preset_name)) {
                                        found_searched_for_preset_name = true;
                                    }
                                    break;
                                }
                            }
                        }
                    }

                    if (found_searched_for_preset_name) {
                        var i = node.children.items.len;
                        while (i > 0) {
                            i -= 1;
                            const child = &node.children.items[i];
                            if (child.property) |child_property| {
                                if (strEql(child_property, "Jetpack")) {
                                    if (child.value) |child_value| {
                                        if (strEql(child_value, "AEJetpack")) {
                                            return child;
                                        }
                                    }
                                } else if (strEql(child_property, "CopyOf")) {
                                    if (child.value) |child_preset_name| {
                                        return findJetpackRecursivelyFolder(file_tree, file_tree, child_preset_name);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    for (node.children.items) |*child| {
        if (findJetpackRecursivelyNode(child, file_tree, preset_name)) |returned| {
            return returned;
        }
    }

    return null;
}

fn maxLengthToOffsets(node: *Node, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "AddActor")) {
            if (node.value) |value| {
                if (strEql(value, "Leg")) {
                    var children = &node.children;

                    for (children.items) |*child| {
                        if (child.property) |child_property| {
                            if (strEql(child_property, "MaxLength")) {
                                if (child.value) |child_value| {
                                    child.property = "ContractedOffset";
                                    child.value = "Vector";
                                    try child.children.append(Node{
                                        .property = "X",
                                        .value = try allocPrint(allocator, "{d}", .{try parseFloat(f32, child_value) / 2}),
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
                                        .value = child_value,
                                        .comments = ArrayList([]const u8).init(allocator),
                                        .children = ArrayList(Node).init(allocator),
                                    });
                                    try extended_offset.children.append(Node{
                                        .property = "Y",
                                        .value = "0",
                                        .comments = ArrayList([]const u8).init(allocator),
                                        .children = ArrayList(Node).init(allocator),
                                    });
                                    try children.append(extended_offset);
                                } else {
                                    return UpdateIniFileTreeErrors.ExpectedValue;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn maxMassToMaxInventoryMass(node: *Node, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "AddActor")) {
            for (node.children.items) |*child| {
                if (child.property) |child_property| {
                    if (strEql(child_property, "MaxMass")) {
                        if (child.value) |v| {
                            const max_mass = try parseFloat(f32, v);

                            for (node.children.items) |child2| {
                                if (child2.property) |child_property2| {
                                    if (strEql(child_property2, "Mass")) {
                                        if (child2.value) |v2| {
                                            child.property = "MaxInventoryMass";

                                            const mass = try parseFloat(f32, v2);
                                            const max_inventory_mass = max_mass - mass;
                                            child.value = try allocPrint(allocator, "{d}", .{max_inventory_mass});
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

fn maxThrottleRangeToPositiveThrottleMultiplier(node: *Node, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "MaxThrottleRange")) {
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

fn minThrottleRangeToNegativeThrottleMultiplier(node: *Node, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "MinThrottleRange")) {
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

fn pieMenu(actor_name: []const u8, default_copy_of_name: []const u8, starting_direction_count_up: u32, starting_direction_count_down: u32, starting_direction_count_left: u32, starting_direction_count_right: u32, file_tree: *IniFolder, allocator: Allocator) !void {
    for (file_tree.folders.items) |*folder| {
        try pieMenu(actor_name, default_copy_of_name, starting_direction_count_up, starting_direction_count_down, starting_direction_count_left, starting_direction_count_right, folder, allocator);
    }

    for (file_tree.files.items) |file| {
        for (file.ast.items) |*node| {
            try pieMenuRecursivelyNode(node, actor_name, default_copy_of_name, starting_direction_count_up, starting_direction_count_down, starting_direction_count_left, starting_direction_count_right, allocator);
        }
    }
}

fn pieMenuRecursivelyNode(node: *Node, actor_name: []const u8, default_copy_of_name: []const u8, starting_direction_count_up: u32, starting_direction_count_down: u32, starting_direction_count_left: u32, starting_direction_count_right: u32, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "AddActor")) {
            if (node.value) |value| {
                if (strEql(value, actor_name)) {
                    var children = &node.children;

                    var contains_pie_slice = false;
                    for (children.items) |*child| {
                        if (child.property) |child_property| {
                            if (strEql(child_property, "AddPieSlice")) {
                                if (child.value) |child_value| {
                                    if (strEql(child_value, "PieSlice")) {
                                        contains_pie_slice = true;
                                    }
                                } else {
                                    return UpdateIniFileTreeErrors.ExpectedValue;
                                }
                            }
                        }
                    }

                    if (contains_pie_slice) {
                        var pie_menu = Node{
                            .property = "PieMenu",
                            .value = "PieMenu",
                            .comments = ArrayList([]const u8).init(allocator),
                            .children = ArrayList(Node).init(allocator),
                        };

                        try pie_menu.children.append(Node{
                            .property = "CopyOf",
                            .value = default_copy_of_name,
                            .comments = ArrayList([]const u8).init(allocator),
                            .children = ArrayList(Node).init(allocator),
                        });

                        for (children.items) |*child| {
                            if (child.property) |child_property| {
                                if (strEql(child_property, "AddPieSlice")) {
                                    if (child.value) |child_value| {
                                        if (strEql(child_value, "PieSlice")) {
                                            // Make a copy of the PieSlice in the PieMenu
                                            try pie_menu.children.append(Node{
                                                .property = "AddPieSlice",
                                                .value = "PieSlice",
                                                .comments = child.comments,
                                                .children = child.children,
                                            });

                                            // Remove the PieSlice from the root of the Actor
                                            child.property = null;
                                            child.value = null;
                                            child.comments = ArrayList([]const u8).init(allocator);
                                            child.children = ArrayList(Node).init(allocator);
                                        }
                                    } else {
                                        return UpdateIniFileTreeErrors.ExpectedValue;
                                    }
                                }
                            }
                        }

                        try applyPieQuadrantSlotLimit(&pie_menu, "Up", starting_direction_count_up);
                        try applyPieQuadrantSlotLimit(&pie_menu, "Down", starting_direction_count_down);
                        try applyPieQuadrantSlotLimit(&pie_menu, "Left", starting_direction_count_left);
                        try applyPieQuadrantSlotLimit(&pie_menu, "Right", starting_direction_count_right);

                        try children.append(pie_menu);
                    }
                }
            }
        }
    }

    for (node.children.items) |*child| {
        try pieMenuRecursivelyNode(child, actor_name, default_copy_of_name, starting_direction_count_up, starting_direction_count_down, starting_direction_count_left, starting_direction_count_right, allocator);
    }
}

fn applyPieQuadrantSlotLimit(pie_menu: *Node, direction: []const u8, starting_direction_count: u32) !void {
    var direction_count = starting_direction_count;

    // From the Source repo in System/PieQuadrant.h, under the name c_PieQuadrantSlotCount
    const PieQuadrantSlotCount = 5;

    for (pie_menu.children.items) |menu_child| {
        if (menu_child.property) |meny_child_property| {
            if (strEql(meny_child_property, "AddPieSlice")) {
                if (menu_child.value) |value| {
                    if (strEql(value, "PieSlice")) {
                        for (menu_child.children.items) |*slice_child| {
                            if (slice_child.property) |slice_child_property| {
                                if (strEql(slice_child_property, "Direction")) {
                                    if (slice_child.value) |slice_child_value| {
                                        if (strEql(slice_child_value, direction)) {
                                            if (direction_count == PieQuadrantSlotCount) {
                                                slice_child.value = "Any";
                                            } else {
                                                direction_count += 1;
                                            }
                                        }
                                    } else {
                                        return UpdateIniFileTreeErrors.ExpectedValue;
                                    }
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

fn removeSlTerrainProperties(node: *Node, allocator: Allocator) !void {
    if (node.property) |property| {
        if (strEql(property, "Terrain")) {
            if (node.value) |value| {
                if (strEql(value, "SLTerrain")) {
                    for (node.children.items) |*child| {
                        if (child.property) |child_property| {
                            // Remove Offset from the Terrain
                            if (strEql(child_property, "Offset")) {
                                if (child.value) |child_value| {
                                    if (strEql(child_value, "Vector")) {
                                        child.property = null;
                                        child.value = null;
                                        child.comments = ArrayList([]const u8).init(allocator);
                                        child.children = ArrayList(Node).init(allocator);
                                    }
                                } else {
                                    return UpdateIniFileTreeErrors.ExpectedValue;
                                }
                            }

                            // Remove DrawTransparent from the Terrain
                            if (strEql(child_property, "DrawTransparent")) {
                                child.property = null;
                                child.value = null;
                                child.comments = ArrayList([]const u8).init(allocator);
                                child.children = ArrayList(Node).init(allocator);
                            }

                            // Remove ScrollRatio from the Terrain
                            if (strEql(child_property, "ScrollRatio")) {
                                if (child.value) |child_value| {
                                    if (strEql(child_value, "Vector")) {
                                        child.property = null;
                                        child.value = null;
                                        child.comments = ArrayList([]const u8).init(allocator);
                                        child.children = ArrayList(Node).init(allocator);
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
    }
}

fn shovelFlashFix(node: *Node) !void {
    if (node.property) |property| {
        if (strEql(property, "AddDevice")) {
            if (node.value) |value| {
                if (strEql(value, "HDFirearm")) {
                    var changed_shovel_sprite = false;

                    for (node.children.items) |firearm_child| {
                        if (firearm_child.property) |firearm_child_property| {
                            if (strEql(firearm_child_property, "SpriteFile")) {
                                if (firearm_child.value) |firearm_child_value| {
                                    if (strEql(firearm_child_value, "ContentFile")) {
                                        for (firearm_child.children.items) |*content_file_child| {
                                            if (content_file_child.property) |content_file_child_property| {
                                                if (strEql(content_file_child_property, "FilePath")) {
                                                    if (content_file_child.value) |content_file_child_value| {
                                                        if (strEql(content_file_child_value, "Ronin.rte/Effects/Pyro/Flashes/ShovelFlash.png")) {
                                                            content_file_child.value = "Ronin.rte/Devices/Tools/Shovel/Effects/ShovelFlash.png";

                                                            changed_shovel_sprite = true;
                                                        }
                                                    } else {
                                                        return UpdateIniFileTreeErrors.ExpectedValue;
                                                    }
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

                    if (changed_shovel_sprite) {
                        for (node.children.items) |*firearm_child| {
                            if (firearm_child.property) |firearm_child_property| {
                                if (strEql(firearm_child_property, "FrameCount")) {
                                    if (firearm_child.value) |firearm_child_value| {
                                        if (strEql(firearm_child_value, "2")) {
                                            firearm_child.value = "1";
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
        const child_output_folder_path = try join(allocator, &.{ output_folder_path, folder.name });
        // std.debug.print("{s}\n", .{child_output_folder_path});
        try writeIniFileTree(&folder, child_output_folder_path, allocator);
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

        if (entry.kind == std.fs.File.Kind.file and strEql(entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'general/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and strEql(entry.basename, "in.ini"), dir_path });

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

        if (entry.kind == std.fs.File.Kind.file and strEql(entry.basename, "in.lua")) {
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

        if (entry.kind == std.fs.File.Kind.file and strEql(entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'ini_rules/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and strEql(entry.basename, "in.ini"), dir_path });

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

            try applyOnNodes(pathToFilePath, &file_tree);

            try applyOnNodesAlloc(bmpExtensionToPng, &file_tree, allocator);

            try applyOnNodesAlloc(wavExtensionToFlac, &file_tree, allocator);

            const ini_copy_of_rules = try parseIniCopyOfRules(allocator);
            applyIniCopyOfRules(ini_copy_of_rules, &file_tree);

            const ini_file_path_rules = try parseIniFilePathRules(allocator);
            applyIniFilePathRules(ini_file_path_rules, &file_tree);

            const ini_script_path_rules = try parseIniScriptPathRules(allocator);
            applyIniScriptPathRules(ini_script_path_rules, &file_tree);

            // // TODO: Figure out a way to remove all these function calls that are already done by convert()
            // // TODO: At the moment I am literally keeping these in sync manually

            const ini_property_rules = try parseIniPropertyRules(allocator);
            applyIniPropertyRules(ini_property_rules, &file_tree);

            const ini_rules = try parseIniRules(allocator);
            applyIniRules(ini_rules, &file_tree);

            const ini_sound_container_rules = try parseIniSoundContainerRules(allocator);
            applyIniSoundContainerRules(ini_sound_container_rules, &file_tree);

            try updateIniFileTree(&file_tree, allocator);

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

        if (entry.kind == std.fs.File.Kind.file and strEql(entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'updated/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and strEql(entry.basename, "in.ini"), dir_path });

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

            try applyOnNodes(pathToFilePath, &file_tree);

            try applyOnNodesAlloc(bmpExtensionToPng, &file_tree, allocator);

            try applyOnNodesAlloc(wavExtensionToFlac, &file_tree, allocator);

            const ini_copy_of_rules = try parseIniCopyOfRules(allocator);
            applyIniCopyOfRules(ini_copy_of_rules, &file_tree);

            const ini_file_path_rules = try parseIniFilePathRules(allocator);
            applyIniFilePathRules(ini_file_path_rules, &file_tree);

            const ini_script_path_rules = try parseIniScriptPathRules(allocator);
            applyIniScriptPathRules(ini_script_path_rules, &file_tree);

            // // TODO: Figure out a way to remove all these function calls that are already done by convert()
            // // TODO: At the moment I am literally keeping these in sync manually

            const ini_property_rules = try parseIniPropertyRules(allocator);
            applyIniPropertyRules(ini_property_rules, &file_tree);

            const ini_rules = try parseIniRules(allocator);
            applyIniRules(ini_rules, &file_tree);

            const ini_sound_container_rules = try parseIniSoundContainerRules(allocator);
            applyIniSoundContainerRules(ini_sound_container_rules, &file_tree);

            try updateIniFileTree(&file_tree, allocator);

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

        if (entry.kind == std.fs.File.Kind.file and strEql(entry.basename, "in.ini")) {
            std.debug.print("\nSubtest 'invalid/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});
            // std.debug.print("{s}\n{}\n{}\n{s}\n{}\n{s}\n", .{ entry.basename, entry.dir, entry.kind, entry.path, entry.kind == std.fs.File.Kind.File and strEql(entry.basename, "in.ini"), dir_path });

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
                try expectEqualStrings(error_text, @errorName(err));
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

pub fn zip_mods(input_folder_path: []const u8, output_folder_path: []const u8, allocator: Allocator) !void {
    var iterable_dir = try std.fs.openIterableDirAbsolute(input_folder_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.directory) {
            const mod_folder_path = try join(allocator, &.{ output_folder_path, entry.name });

            const needle = ".rte";
            const replacement = "-pre5.2-v1.0.zip";
            const name = try allocator.alloc(u8, replacementSize(u8, entry.name, needle, replacement));
            _ = replace(u8, entry.name, needle, replacement, name);

            const mod_zip_path = try allocator.dupeZ(u8, try join(allocator, &.{ output_folder_path, name }));

            var zip = ziplib.zip_open(mod_zip_path.ptr, ziplib.ZIP_DEFAULT_COMPRESSION_LEVEL, 'w') orelse return error.ZipOpen;

            try zip_mod_recursively(zip, mod_folder_path, entry.name);

            ziplib.zip_close(zip);
        }
    }
}

fn zip_mod_recursively(zip: *ziplib.zip_t, full_path: []const u8, sub_path: []const u8) !void {
    var child_full_path_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var child_sub_path_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;

    var iterable_dir = try std.fs.openIterableDirAbsolute(full_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |entry| {
        const child_full_path = try std.fmt.bufPrintZ(&child_full_path_buffer, "{s}/{s}", .{ full_path, entry.name });
        const child_sub_path = try std.fmt.bufPrintZ(&child_sub_path_buffer, "{s}/{s}", .{ sub_path, entry.name });

        if (entry.kind == std.fs.File.Kind.file) {
            // TODO: Not sure whether these files are ever actually returned by Zig's dir iterator
            if (strEql(entry.name, ".") or strEql(entry.name, ".."))
                continue;

            if (ziplib.zip_entry_open(zip, child_sub_path) < 0) return error.ZipEntryOpen;
            if (ziplib.zip_entry_fwrite(zip, child_full_path) < 0) return error.ZipEntryFwrite;
            if (ziplib.zip_entry_close(zip) < 0) return error.ZipEntryClose;
        } else if (entry.kind == std.fs.File.Kind.directory) {
            try zip_mod_recursively(zip, child_full_path, child_sub_path);
        }
    }
}
