const std = @import("std");
const builtin = @import("builtin");

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
const indexOfDiff = std.mem.indexOfDiff;
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

/// Updated by `convert()` to record what it is doing.
/// If `convert()` crashed, look inside this struct to see why and where it did.
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

// TODO: Refactor into a CLI
pub fn main() !void {
    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    const program_name = args.next() orelse return error.ExpectedProgramName;
    _ = program_name;
    const input_folder_path = args.next() orelse return error.ExpectedInputFolderPath;
    const output_folder_path = args.next() orelse return error.ExpectedOutputFolderPath;

    var diagnostics: Diagnostics = .{};
    convert(
        input_folder_path,
        output_folder_path,
        allocator,
        &diagnostics,
    ) catch |err| switch (err) {
        error.InvalidInputPath => {
            std.log.info("Error: Invalid input path", .{});
            return err;
        },
        error.InvalidOutputPath => {
            std.log.info("Error: Invalid output path", .{});
            return err;
        },
        error.UnexpectedToken => {
            std.log.info("Error: Unexpected '{s}' at {s}:{}:{}\n", .{
                diagnostics.token orelse "null",
                diagnostics.file_path orelse "null",
                diagnostics.line orelse -1,
                diagnostics.column orelse -1,
            });
            return err;
        },
        error.UnclosedMultiComment => {
            std.log.info("Error: Unclosed multi-line comment in file {s}\n", .{
                diagnostics.file_path orelse "null",
            });
            return err;
        },
        error.TooManyTabs => {
            std.log.info("Error: Too many tabs at {s}:{}:{}\n", .{
                diagnostics.file_path orelse "null",
                diagnostics.line orelse -1,
                diagnostics.column orelse -1,
            });
            return err;
        },
        error.ExpectedADataModule => {
            std.log.info("Error: Expected a DataModule\n", .{});
            return err;
        },
        error.ContainsMoreThanOneDataModule => {
            std.log.info("Error: The mod contains more than one DataModule\n", .{});
            return err;
        },
        else => |e| return e,
    };

    try beautifyLua(output_folder_path, allocator);
}

/// For every mod directory in `input_folder_path`, it creates a copy of the mod directory in `output_folder_path` with the required changes to make it compatible with the latest version of the game.
/// If `convert()` crashed, the `diagnostics` argument allows you to know why and where it did.
pub fn convert(input_folder_path_: []const u8, output_folder_path_: []const u8, allocator: Allocator, diagnostics: *Diagnostics) !void {
    // Necessary solely because of .OBJECT_NAME_INVALID => unreachable in the std lib:
    // https://github.com/ziglang/zig/issues/15607#issue-1698930560
    if (!try isValidDirPath(input_folder_path_)) return error.InvalidInputPath;
    if (!try isValidDirPath(output_folder_path_)) return error.InvalidOutputPath;

    const input_folder_path = try std.fs.realpathAlloc(allocator, input_folder_path_);
    const output_folder_path = try std.fs.realpathAlloc(allocator, output_folder_path_);

    std.log.info("Making all output dirs...\n", .{});
    try makeOutputDirs(input_folder_path, output_folder_path, allocator);

    std.log.info("Copying files...\n", .{});
    try copyFiles(input_folder_path, output_folder_path, allocator);

    const lua_rules = try parseLuaRules(allocator);
    std.log.info("Applying Lua rules...\n", .{});
    try applyLuaRules(lua_rules, output_folder_path, allocator);

    std.log.info("Getting INI file tree...\n", .{});
    var file_tree = try getIniFileTree(input_folder_path, allocator, diagnostics);

    std.log.info("Getting the mod's game version...\n", .{});
    const mod_version = try getModVersion(&file_tree);

    // Check the version, since people since Pre6 are able to start using magenta
    // as a regular color in their RGB pngs
    if (mod_version == ModVersion.BeforePre6) {
        std.log.info("Replacing magenta in RGB .pngs with alpha...\n", .{});
        try replaceMagentaInRgbPngsWithAlpha(output_folder_path, allocator);
    }

    std.log.info("Replacing Path with FilePath...\n", .{});
    try applyOnNodes(pathToFilePath, &file_tree);

    // It also HAS to be called before applyIniFilePathRules(),
    // because otherwise this could happen:
    // The game reports that Base.rte/foo.png doesn't exist,
    // so the user enters this ini_file_path_rules.json rule:
    // "Base.rte/foo.png": "Base.rte/bar.png"
    // The game reports that Base.rte/foo.png *still* doesn't exist,
    // due to the parsed input mod containing "Base.rte/foo.bmp"!
    std.log.info("Bmp extension to png...\n", .{});
    try applyOnNodesAlloc(bmpExtensionToPng, &file_tree, allocator);

    std.log.info("Wav extension to flac...\n", .{});
    try applyOnNodesAlloc(wavExtensionToFlac, &file_tree, allocator);

    const ini_copy_of_rules = try parseIniCopyOfRules(allocator);
    std.log.info("Applying INI CopyOf rules...\n", .{});
    applyIniCopyOfRules(ini_copy_of_rules, &file_tree);

    const ini_file_path_rules = try parseIniFilePathRules(allocator);
    std.log.info("Applying INI FilePath rules...\n", .{});
    applyIniFilePathRules(ini_file_path_rules, &file_tree);

    const ini_script_path_rules = try parseIniScriptPathRules(allocator);
    std.log.info("Applying INI ScriptPath rules...\n", .{});
    applyIniScriptPathRules(ini_script_path_rules, &file_tree);

    const ini_property_rules = try parseIniPropertyRules(allocator);
    std.log.info("Applying INI property rules...\n", .{});
    applyIniPropertyRules(ini_property_rules, &file_tree);

    const ini_rules = try parseIniRules(allocator);
    std.log.info("Applying INI rules...\n", .{});
    applyIniRules(ini_rules, &file_tree);

    const ini_sound_container_rules = try parseIniSoundContainerRules(allocator);
    std.log.info("Applying INI SoundContainer rules...\n", .{});
    applyIniSoundContainerRules(ini_sound_container_rules, &file_tree);

    std.log.info("Updating INI file tree...\n", .{});
    try updateIniFileTree(&file_tree, allocator);

    std.log.info("Writing INI file tree...\n", .{});
    try writeIniFileTree(&file_tree, output_folder_path, allocator);
}

fn isValidDirPath(path: []const u8) !bool {
    if (builtin.os.tag != .windows) return error.OnlySupportingWindowsSowwy;

    const d = std.fs.cwd();
    const path_w_slice = try std.os.windows.sliceToPrefixedFileW(d.fd, path);
    const path_w = path_w_slice.span().ptr;

    const w = std.os.windows;
    const access_mask = w.STANDARD_RIGHTS_READ | w.FILE_READ_ATTRIBUTES | w.FILE_READ_EA |
        w.SYNCHRONIZE | w.FILE_TRAVERSE;

    var result = std.fs.Dir{
        .fd = undefined,
    };
    const path_len_bytes = @as(u16, @intCast(std.mem.sliceTo(path_w, 0).len * 2));
    var nt_name = w.UNICODE_STRING{
        .Length = path_len_bytes,
        .MaximumLength = path_len_bytes,
        .Buffer = @constCast(path_w),
    };
    var attr = w.OBJECT_ATTRIBUTES{
        .Length = @sizeOf(w.OBJECT_ATTRIBUTES),
        .RootDirectory = if (std.fs.path.isAbsoluteWindowsW(path_w)) null else d.fd,
        .Attributes = 0, // Note we do not use OBJ_CASE_INSENSITIVE here.
        .ObjectName = &nt_name,
        .SecurityDescriptor = null,
        .SecurityQualityOfService = null,
    };

    const no_follow = false;
    const open_reparse_point: w.DWORD = if (no_follow) w.FILE_OPEN_REPARSE_POINT else 0x0;
    var io: w.IO_STATUS_BLOCK = undefined;

    const rc = w.ntdll.NtCreateFile(
        &result.fd,
        access_mask,
        &attr,
        &io,
        null,
        0,
        w.FILE_SHARE_READ | w.FILE_SHARE_WRITE,
        w.FILE_OPEN,
        w.FILE_DIRECTORY_FILE | w.FILE_SYNCHRONOUS_IO_NONALERT | w.FILE_OPEN_FOR_BACKUP_INTENT | open_reparse_point,
        null,
        0,
    );
    return rc == .SUCCESS;
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
            const ext = extension(entry.name);

            if (strEql(ext, ".bmp")) {
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

                    if (input_stat.mtime > output_stat.mtime) {
                        // TODO: Figure out whether a different function should be called in this case,
                        // similar to below where updateFileAbsolute() can be called instead of copyFileAbsolute()
                        try convertBmpToPng(input_file_path, output_file_path, allocator);
                    }
                } else { // Else return the access error
                    return output_file_access;
                }
            } else if (strEql(ext, ".wav")) {
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
                        // TODO: Figure out whether a different function should be called in this case,
                        // similar to below where updateFileAbsolute() can be called instead of copyFileAbsolute()
                        try convertWavToFlac(input_file_path, output_file_path, allocator);
                    }
                } else { // Else return the access error
                    return output_file_access;
                }
            } else if (!strEql(ext, ".ini")) { // This function doesn't copy .ini files
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
    const argv = [_][]const u8{ getFfmpegPath(), "-i", input_file_path, output_file_path, "-y" };
    const result = try std.ChildProcess.exec(.{ .argv = &argv, .allocator = allocator });
    _ = result;
}

fn convertWavToFlac(input_file_path: []const u8, output_file_path: []const u8, allocator: Allocator) !void {
    const argv = [_][]const u8{ getFfmpegPath(), "-i", input_file_path, output_file_path, "-y" };
    const result = try std.ChildProcess.exec(.{ .argv = &argv, .allocator = allocator });
    _ = result;

    // var line_iter = std.mem.split(u8, result.stderr, "\n");
    // while (line_iter.next()) |line| {
    //     if (line.len == 0) continue;
    // }
}

fn parseLuaRules(allocator: Allocator) !std.json.ArrayHashMap([]const u8) {
    const text = try readFile("rules/lua_rules.json", allocator);

    var scanner = Scanner.initCompleteInput(allocator, text);

    var lua_rules = try std.json.ArrayHashMap([]const u8).jsonParse(allocator, &scanner, .{ .allocate = .alloc_if_needed, .max_value_len = default_max_value_len });
    return lua_rules;
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
                    var file = try std.fs.cwd().createFile(file_path, .{});
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
    var folder = IniFolder{
        .name = basename(folder_path),
        .files = ArrayList(IniFile).init(allocator),
        .folders = ArrayList(IniFolder).init(allocator),
    };

    var iterable_dir = try std.fs.openIterableDirAbsolute(folder_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.file) {
            const file_path = try join(allocator, &.{ folder_path, entry.name });
            if (strEql(extension(entry.name), ".ini")) {
                diagnostics.file_path = file_path;
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
    var input_file = try std.fs.cwd().openFile(input_path, .{});
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
        try tokens.append(token);
    }

    if (multiline_comment_depth > 0) {
        return TokenError.UnclosedMultiComment;
    }

    return tokens;
}

fn getToken(slice: *[]const u8, multiline_comment_depth: *i32, seen_property: *bool) Token {
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
    while (end_index < slice.len) {
        if (slice.*[end_index] == '=' and !seen_property.*) {
            break;
        }
        if (slice.*[end_index] == '\n' or slice.*[end_index] == '\t' or (slice.*[end_index] == '/' and (slice.*[end_index + 1] == '*' or slice.*[end_index + 1] == '/'))) {
            break;
        }
        if (slice.*[end_index] != ' ') {
            sentence_end_index = end_index + 1;
        }
        end_index += 1;
    }

    seen_property.* = true;
    const token = Token{ .type = .Sentence, .slice = slice.*[0..sentence_end_index] };
    slice.* = slice.*[sentence_end_index..];
    return token;
}

fn getAstFromTokens(tokens: *ArrayList(Token), allocator: Allocator, diagnostics: *Diagnostics) !ArrayList(Node) {
    var ast = ArrayList(Node).init(allocator);

    // The game ignores the indentation of the first line of a file,
    // but we choose to trim that indentation
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
    if (line_depth > depth) {
        calculateLineAndColumnDiagnostics(tokens, token_index.*, diagnostics);
        return NodeError.TooManyTabs;
    } else if (line_depth < depth) {
        return node;
    }

    var checked_line_depth = true;

    while (token_index.* < tokens.items.len) {
        token = tokens.items[token_index.*];

        if (seen == .Start and token.type == .Sentence) {
            // This if-statement is deliberately in a loop,
            // since whitespace and multiline comments may come before it
            node.property = token.slice;
            seen = .Property;
            token_index.* += 1;
        } else if (seen == .Newline and !checked_line_depth) {
            checked_line_depth = true;

            line_depth = getLineDepth(tokens, token_index.*);

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

// Uninitialized is used to detect SupportedGameVersion being seen a second time
const ModVersion = enum {
    Uninitialized,
    BeforePre6,
    Pre6,
};

fn getModVersion(file_tree: *IniFolder) !ModVersion {
    var mod_version = ModVersion.Uninitialized;

    for (file_tree.files.items) |file| {
        if (strEql(file.name, "Index.ini")) {
            var data_module_count: i32 = 0;

            for (file.ast.items) |*node| {
                try getModVersionRecursivelyNode(node, &mod_version, &data_module_count);
            }

            if (data_module_count == 0) {
                return error.ExpectedADataModule;
            } else if (data_module_count > 1) {
                return error.ContainsMoreThanOneDataModule;
            }

            break;
        }
    }

    // Apply all updates to the mod if it has no Index.ini,
    // as we want tests without an Index.ini to pass,
    // as well as CC Steam Workshop mods that have no SupportedGameVersion
    if (mod_version == ModVersion.Uninitialized) {
        mod_version = ModVersion.BeforePre6;
    }

    return mod_version;
}

fn getModVersionRecursivelyNode(node: *Node, mod_version: *ModVersion, data_module_count: *i32) !void {
    const ModVersionErrors = error{
        AlreadySeenASupportedGameVersion,
        UnrecognizedSupportedGameVersion,
    };

    if (node.property) |node_property| {
        if (strEql(node_property, "SupportedGameVersion")) {
            if (node.value) |value| {
                if (mod_version.* != ModVersion.Uninitialized) {
                    return ModVersionErrors.AlreadySeenASupportedGameVersion;
                }
                if (strEql(value, "6.0.0")) {
                    mod_version.* = ModVersion.Pre6;
                } else if (strEql(value, "Pre-Release 3.0") or strEql(value, "Pre-Release 4.0") or strEql(value, "Pre-Release 5.0") or strEql(value, "Pre-Release 5.1") or strEql(value, "Pre-Release 5.2") or strEql(value, "5.1.0")) {
                    mod_version.* = ModVersion.BeforePre6;
                } else {
                    return ModVersionErrors.UnrecognizedSupportedGameVersion;
                }
            }
        } else if (strEql(node_property, "DataModule")) {
            data_module_count.* += 1;
        }
    }

    for (node.children.items) |*child| {
        try getModVersionRecursivelyNode(child, mod_version, data_module_count);
    }
}

fn replaceMagentaInRgbPngsWithAlpha(output_folder_path: []const u8, allocator: Allocator) !void {
    var iterable_dir = try std.fs.openIterableDirAbsolute(output_folder_path, .{});
    defer iterable_dir.close();
    var dir_iterator = iterable_dir.iterate();

    while (try dir_iterator.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.file and strEql(extension(entry.name), ".png")) {
            const output_file_path = try join(allocator, &.{ output_folder_path, entry.name });

            if (try pngIsRgb(output_file_path)) {
                var tmpdir_output_folder = tmpDir(.{});
                defer tmpdir_output_folder.cleanup();

                var tmpdir_output_folder_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
                const tmpdir_output_folder_path = try tmpdir_output_folder.dir.realpath(".", &tmpdir_output_folder_path_buffer);

                // This file will have (255, 0, 255) replaced with (255, 0, 255, 0)
                const tmp_alpha_file_path = try join(allocator, &.{ tmpdir_output_folder_path, "alpha.png" });

                // Note that "colorkey" its "similarity" value has a default, and minimum, of 0.01,
                // which means that colors that are very close to (255, 0, 255) also turn transparent.
                // This is fine however, since I checked that the CC palette doesn't contain other colors
                // that are close enough to magenta to be turned transparent.
                const argv_alpha = [_][]const u8{ getFfmpegPath(), "-i", output_file_path, "-vf", "colorkey=magenta", "-y", tmp_alpha_file_path };
                const result_alpha = try std.ChildProcess.exec(.{ .argv = &argv_alpha, .allocator = allocator });
                _ = result_alpha;

                // This file will have (r=255, g=0, b=255, a=0) replaced with (r=0, g=0, b=0, a=0), as CC requires all RGB to be 0 when alpha is 0
                const tmp_zero_file_path = try join(allocator, &.{ tmpdir_output_folder_path, "zero.png" });

                // This insanity is just asking ffmpeg to replace any (r=255, g=0, b=255, a=0) with (r=0, g=0, b=0, a=0)
                const argv_zero = [_][]const u8{ getFfmpegPath(), "-i", tmp_alpha_file_path, "-vf", "geq=r='if(eq(r(X,Y),255)*eq(g(X,Y),0)*eq(b(X,Y),255),0,r(X,Y))':g='if(eq(r(X,Y),255)*eq(g(X,Y),0)*eq(b(X,Y),255),0,g(X,Y))':b='if(eq(r(X,Y),255)*eq(g(X,Y),0)*eq(b(X,Y),255),0,b(X,Y))':a='alpha(X,Y)'", "-y", tmp_zero_file_path };
                const result_zero = try std.ChildProcess.exec(.{ .argv = &argv_zero, .allocator = allocator });
                _ = result_zero;

                // Overwrite the old png with the new one that has transparency
                try copyFileAbsolute(tmp_zero_file_path, output_file_path, .{});
            }
        } else if (entry.kind == std.fs.File.Kind.directory) {
            const child_output_folder_path = try join(allocator, &.{ output_folder_path, entry.name });
            try replaceMagentaInRgbPngsWithAlpha(child_output_folder_path, allocator);
        }
    }
}

// RGB, as opposed to indexed with a palette
fn pngIsRgb(output_file_path: []const u8) !bool {
    const file = try std.fs.cwd().openFile(output_file_path, .{});
    defer file.close();

    const color_type_offset = 0x19;
    try file.seekTo(color_type_offset);

    const color_type = try file.reader().readByte();

    // Note that 2 means rgb, so we deliberately don't care about rgba,
    // since that'd be 6:
    // https://en.wikipedia.org/wiki/PNG#Pixel_format
    return color_type == 2;
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
        if (strEql(property, "FilePath") or strEql(property, "AddSound")) {
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
    const text = try readFile("rules/ini_copy_of_rules.json", allocator);

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
    const text = try readFile("rules/ini_file_path_rules.json", allocator);

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
    const text = try readFile("rules/ini_script_path_rules.json", allocator);

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
    const text = try readFile("rules/ini_property_rules.json", allocator);

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
    const text = try readFile("rules/ini_rules.json", allocator);
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
    const text = try readFile("rules/ini_sound_container_rules.json", allocator);
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
    try applyOnNodesAlloc(addGetsHitByMosWhenHeldToShields, file_tree, allocator);
    try applyOnNodesAlloc(addGripStrength, file_tree, allocator);
    try applyOnNodesAlloc(addOrUpdateSupportedGameVersion, file_tree, allocator);
    try applyOnNodes(aemitterFuelToPemitter, file_tree);

    // Handles AEJetpacks being made first-class citizens by Causeless
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

fn addGetsHitByMosWhenHeldToShields(node: *Node, allocator: Allocator) !void {
    if (node.property) |node_property| {
        if (isInsaneProperty(node_property)) {
            if (node.value) |node_value| {
                if (strEql(node_value, "HeldDevice")) {
                    var children = &node.children;

                    var is_in_shield_group = false;

                    for (children.items) |child| {
                        if (child.property) |property| {
                            if (strEql(property, "GetsHitByMOsWhenHeld")) {
                                return;
                            }
                            if (strEql(property, "AddToGroup")) {
                                if (child.value) |value| {
                                    if (strEql(value, "Shields")) {
                                        is_in_shield_group = true;
                                    }
                                }
                            }
                        }
                    }

                    if (!is_in_shield_group) {
                        return;
                    }

                    try children.append(Node{
                        .property = "GetsHitByMOsWhenHeld",
                        .value = "1",
                        .comments = ArrayList([]const u8).init(allocator),
                        .children = ArrayList(Node).init(allocator),
                    });
                }
            }
        }
    }
}

/// "Insane", because the game accepts things like "AddAmmo = AHuman" and "AddActor = HeldDevice"
fn isInsaneProperty(property: []const u8) bool {
    return strEql(property, "AddEffect") or strEql(property, "AddAmmo") or strEql(property, "AddDevice") or strEql(property, "AddActor");
}

fn addGripStrength(node: *Node, allocator: Allocator) !void {
    if (node.property) |node_property| {
        if (isInsaneProperty(node_property)) {
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
    const converter_game_version = "6.0.0";

    if (node.property) |property| {
        if (strEql(property, "DataModule")) {
            var has_supported_game_version = false;

            for (node.children.items) |*child| {
                if (child.property) |child_property| {
                    if (strEql(child_property, "SupportedGameVersion")) {
                        has_supported_game_version = true;

                        if (child.value) |child_value| {
                            if (!strEql(child_value, converter_game_version)) {
                                child.value = converter_game_version;
                            }
                        }
                    }
                }
            }

            if (!has_supported_game_version) {
                try node.children.append(Node{
                    .property = "SupportedGameVersion",
                    .value = converter_game_version,
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
        if (isInsaneProperty(property)) {
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

/// Move the Actor's jetpack modifiers to its last "Jetpack = AEJetpack",
/// where the modifiers get pushed to the back of the AEJetpack if they came after it,
/// and inserted to the front of it if they came before it.
/// Modifiers that come after the AEJetpack are .appended() in a forwards loop.
/// Modifiers that come before the AEJetpack are .inserted() in a backwards loop,
/// where the loop is stopped if a CopyOf is encountered.
fn moveJetpackModifiersRecursivelyNode(node: *Node, file_tree: *IniFolder, allocator: Allocator) !void {
    if (node.property) |property| {
        if (isInsaneProperty(property)) {
            if (node.value) |value| {
                if (strEql(value, "ACrab") or strEql(value, "AHuman")) {
                    try appendJetpackModifiers(node);
                    try prependJetpackModifiers(node);
                }
            }
        }
    }

    for (node.children.items) |*child| {
        try moveJetpackModifiersRecursivelyNode(child, file_tree, allocator);
    }
}

fn appendJetpackModifiers(node: *Node) !void {
    while (true) {
        var modifier_index: usize = 0;
        var removed = false;
        modifier_blk: while (modifier_index < node.children.items.len) {
            const modifier = node.children.items[modifier_index];
            if (modifier.property) |modifier_property| {
                if (isJetpackModifier(modifier_property)) {
                    // The AEJetpack *HAS* to be refound every time,
                    // in order for aejetpack_index to stay accurate
                    var aejetpack_index = node.children.items.len;
                    aejetpack_blk: while (aejetpack_index > 0) {
                        aejetpack_index -= 1;
                        const aejetpack = &node.children.items[aejetpack_index];
                        if (aejetpack.property) |aejetpack_property| {
                            // If there is no AEJetpack for it to be copied to
                            if (strEql(aejetpack_property, "CopyOf")) {
                                return;
                            } else if (strEql(aejetpack_property, "Jetpack")) {
                                if (aejetpack.value) |aejetpack_value| {
                                    if (strEql(aejetpack_value, "AEJetpack")) {
                                        if (modifier_index > aejetpack_index) {
                                            try aejetpack.children.append(modifier);
                                            _ = node.children.orderedRemove(modifier_index);
                                            removed = true;
                                            break :modifier_blk;
                                        } else {
                                            break :aejetpack_blk;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            modifier_index += 1;
        }
        if (!removed) {
            return;
        }
    }
}

fn prependJetpackModifiers(node: *Node) !void {
    while (true) {
        var modifier_index = node.children.items.len;
        var removed = false;
        modifier_blk: while (modifier_index > 0) {
            modifier_index -= 1;
            const modifier = node.children.items[modifier_index];
            if (modifier.property) |modifier_property| {
                if (strEql(modifier_property, "CopyOf")) {
                    return;
                } else if (isJetpackModifier(modifier_property)) {
                    // The AEJetpack *HAS* to be refound every time,
                    // in order for aejetpack_index to stay accurate
                    var aejetpack_index = node.children.items.len;
                    aejetpack_blk: while (aejetpack_index > 0) {
                        aejetpack_index -= 1;
                        const aejetpack = &node.children.items[aejetpack_index];
                        if (aejetpack.property) |aejetpack_property| {
                            // If there is no AEJetpack for it to be copied to
                            if (strEql(aejetpack_property, "CopyOf")) {
                                return;
                            } else if (strEql(aejetpack_property, "Jetpack")) {
                                if (aejetpack.value) |aejetpack_value| {
                                    if (strEql(aejetpack_value, "AEJetpack")) {
                                        if (modifier_index < aejetpack_index) {
                                            try aejetpack.children.insert(0, modifier);
                                            _ = node.children.orderedRemove(modifier_index);
                                            removed = true;
                                            break :modifier_blk;
                                        } else {
                                            break :aejetpack_blk;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if (!removed) {
            return;
        }
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
        if (isInsaneProperty(property)) {
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
                            } else if (seen_jetpack_modifier and strEql(child_property, "CopyOf")) {
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
    if (node.property) |property| {
        if (isInsaneProperty(property)) {
            if (node.value) |value| {
                if (strEql(value, "ACrab") or strEql(value, "AHuman")) {
                    var i = node.children.items.len;
                    while (i > 0) {
                        i -= 1;
                        const child = node.children.items[i];
                        if (child.property) |child_property| {
                            if (isJetpackModifier(child_property)) {
                                _ = node.children.orderedRemove(i);
                            }
                        }
                    }
                }
            }
        }
    }

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
        if (isInsaneProperty(property)) {
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
        if (isInsaneProperty(property)) {
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
        if (isInsaneProperty(property)) {
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
        if (isInsaneProperty(property)) {
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
        if (strEql(property, "Terrain") or strEql(property, "AddTerrain")) {
            if (node.value) |value| {
                if (strEql(value, "SLTerrain")) {
                    for (node.children.items) |*child| {
                        if (child.property) |child_property| {
                            // Remove DrawTransparent from the Terrain
                            if (strEql(child_property, "DrawTransparent")) {
                                child.property = null;
                                child.value = null;
                                child.comments = ArrayList([]const u8).init(allocator);
                                child.children = ArrayList(Node).init(allocator);
                            }

                            // Remove Offset and ScrollRatio and ScaleFactor from the Terrain
                            if (strEql(child_property, "Offset") or strEql(child_property, "ScrollRatio") or strEql(child_property, "ScaleFactor")) {
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
        if (isInsaneProperty(property)) {
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
        const file_path = try join(allocator, &.{ output_folder_path, file.name });
        try writeAst(&file.ast, file_path);
    }

    for (file_tree.folders.items) |folder| {
        const child_output_folder_path = try join(allocator, &.{ output_folder_path, folder.name });
        try writeIniFileTree(&folder, child_output_folder_path, allocator);
    }
}

fn writeAst(ast: *const ArrayList(Node), output_path: []const u8) !void {
    const output_file = try std.fs.cwd().createFile(output_path, .{});
    defer output_file.close();

    var buffered = bufferedWriter(output_file.writer());
    const buffered_writer = buffered.writer();

    for (ast.items, 0..) |*node, index| {
        try writeAstRecursively(node, buffered_writer, 0);

        // Don't add a trailing newline,
        // since writeAstRecursively() already adds it
        if (node.property != null and index < ast.items.len - 1) {
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

    var i: usize = 0;
    while (i < depth) : (i += 1) {
        try writeBuffered(buffered_writer, "\t");
    }

    if (node.property) |property| {
        try writeBuffered(buffered_writer, property);
    }

    if (node.value) |value| {
        try writeBuffered(buffered_writer, " = ");
        try writeBuffered(buffered_writer, value);
    }

    if (node.comments.items.len > 0) {
        if (node.property != null) {
            try writeBuffered(buffered_writer, " ");
        }

        try writeBuffered(buffered_writer, "//");

        for (node.comments.items) |comment| {
            try writeBuffered(buffered_writer, " ");
            try writeBuffered(buffered_writer, comment);
        }
    }

    try writeBuffered(buffered_writer, "\n");

    for (node.children.items) |*child| {
        try writeAstRecursively(child, buffered_writer, depth + 1);
    }
}

fn writeBuffered(buffered_writer: anytype, string: []const u8) !void {
    try buffered_writer.print("{s}", .{string});
}

test "general" {
    try testDirectory("general", false);
}

test "ini_rules" {
    try testDirectory("ini_rules", false);
}

test "invalid" {
    try testDirectory("invalid", true);
}

test "lua_rules" {
    try testDirectory("lua_rules", false);
}

test "mod" {
    try testDirectory("mod", false);
}

test "updated" {
    try testDirectory("updated", false);
}

fn testDirectory(comptime directory_name: []const u8, is_invalid_test: bool) !void {
    var iterable_tests = try std.fs.cwd().openIterableDir("tests/" ++ directory_name, .{});
    defer iterable_tests.close();

    var arena = ArenaAllocator.init(page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tests_walker = try iterable_tests.walk(allocator);
    defer tests_walker.deinit();

    while (try tests_walker.next()) |entry| {
        if (entry.kind == std.fs.File.Kind.directory and strEql(entry.basename, "input")) {
            std.debug.print("\nSubtest '" ++ directory_name ++ "/{s}'", .{std.fs.path.dirname(entry.path) orelse "null"});

            var test_folder_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
            const test_folder_path = try entry.dir.realpath(".", &test_folder_path_buffer);

            const input_folder_path = try join(allocator, &.{ test_folder_path, "input" });

            var tmpdir_output_folder = tmpDir(.{});
            defer tmpdir_output_folder.cleanup();
            var tmpdir_output_folder_path_buffer: [MAX_PATH_BYTES]u8 = undefined;
            const tmpdir_output_folder_path = try tmpdir_output_folder.dir.realpath(".", &tmpdir_output_folder_path_buffer);

            var diagnostics: Diagnostics = .{};
            convert(input_folder_path, tmpdir_output_folder_path, allocator, &diagnostics) catch |err| {
                if (is_invalid_test) {
                    const error_path = try join(allocator, &.{ test_folder_path, "expected_error.txt" });

                    const error_file = try std.fs.cwd().openFile(error_path, .{});
                    defer error_file.close();

                    var error_buf_reader = bufferedReader(error_file.reader());
                    const error_stream = error_buf_reader.reader();
                    const error_text_crlf = try error_stream.readAllAlloc(allocator, maxInt(usize));
                    const error_text = try crlfToLf(error_text_crlf, allocator);

                    try expectEqualStrings(error_text, @errorName(err));
                    std.debug.print(" passed", .{});

                    continue;
                } else {
                    return err;
                }
            };

            if (is_invalid_test) {
                return error.InvalidTestDidntReturnError;
            }

            const expected_result_path = try join(allocator, &.{ test_folder_path, "expected_result" });
            try testDirectoryFiles(test_folder_path, expected_result_path, tmpdir_output_folder_path, allocator);

            std.debug.print(" passed", .{});
        }
    }

    std.debug.print("\n\n", .{});
}

fn testDirectoryFiles(test_folder_path: []const u8, expected_result_path: []const u8, tmpdir_output_folder_path: []const u8, allocator: Allocator) !void {
    var iterable_expected_result = try std.fs.cwd().openIterableDir(expected_result_path, .{});
    defer iterable_expected_result.close();

    var expected_result_walker = try iterable_expected_result.walk(allocator);
    defer expected_result_walker.deinit();

    while (try expected_result_walker.next()) |expected_result_entry| {
        if (expected_result_entry.kind == std.fs.File.Kind.file) {
            const expected_file_path = try join(allocator, &.{ expected_result_path, expected_result_entry.path });

            const expected_file = try std.fs.cwd().openFile(expected_file_path, .{});
            defer expected_file.close();

            var expected_buf_reader = bufferedReader(expected_file.reader());
            const expected_stream = expected_buf_reader.reader();
            const expected_text_crlf = try expected_stream.readAllAlloc(allocator, maxInt(usize));
            const expected_text = try crlfToLf(expected_text_crlf, allocator);

            const output_path = try join(allocator, &.{ tmpdir_output_folder_path, expected_result_entry.path });
            const output_file = try std.fs.cwd().openFile(output_path, .{});
            defer output_file.close();

            var output_buf_reader = bufferedReader(output_file.reader());
            const output_stream = output_buf_reader.reader();
            const output_text_crlf = try output_stream.readAllAlloc(allocator, maxInt(usize));
            const output_text = try crlfToLf(output_text_crlf, allocator);

            const ext = extension(expected_result_entry.basename);
            if (strEql(ext, ".png") or strEql(ext, ".flac")) {
                if (indexOfDiff(u8, expected_text, output_text)) |diff_index| {
                    const unequal_copy_path = try join(allocator, &.{ test_folder_path, expected_result_entry.basename });

                    std.debug.print("\nUnequal file at index {} is '{s}'; copying it to `{s}`\n", .{ diff_index, output_path, unequal_copy_path });
                    try copyFileAbsolute(output_path, unequal_copy_path, .{});

                    return error.unequalFiles;
                }
            } else {
                try expectEqualStrings(expected_text, output_text);
            }
        }
    }
}

pub fn beautifyLua(output_folder_path: []const u8, allocator: Allocator) !void {
    std.log.info("Beautifying Lua...\n", .{});
    // TODO: Do we want to compile this from source?
    const argv = [_][]const u8{ getStyluaPath(), output_folder_path };
    const result = try std.ChildProcess.exec(.{ .argv = &argv, .allocator = allocator });
    _ = result;
}

inline fn getFfmpegPath() []const u8 {
    return "dependency_executables/ffmpeg/windows/ffmpeg.exe";
}

inline fn getStyluaPath() []const u8 {
    return "dependency_executables/stylua/windows/stylua.exe";
}
