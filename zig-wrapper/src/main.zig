const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const process = std.process;
const Io = std.Io;
const Dir = Io.Dir;
const File = Io.File;
const Allocator = std.mem.Allocator;

/// Compressed release tarball, embedded at compile time.
const release_tar_zst = @embedFile("release.tar.zst");

/// Content hash for cache invalidation (first 16 chars of sha256).
const release_hash = @embedFile("release.hash");

pub fn main(init: process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    const cache_dir = try getCacheDir(allocator, init.environ_map);
    defer allocator.free(cache_dir);

    const hash_str = mem.trimEnd(u8, release_hash, &[_]u8{ '\n', '\r', ' ' });

    const release_dir = try std.fmt.allocPrint(allocator, "{s}/a4c/{s}", .{ cache_dir, hash_str });
    defer allocator.free(release_dir);

    const marker_path = try std.fmt.allocPrint(allocator, "{s}/.extracted", .{release_dir});
    defer allocator.free(marker_path);

    // Extract if not already cached
    const needs_extract = blk: {
        Dir.accessAbsolute(io, marker_path, .{}) catch {
            break :blk true;
        };
        break :blk false;
    };

    if (needs_extract) {
        try extractRelease(allocator, io, release_dir, marker_path);
    }

    // Find ERTS version directory
    const erts_bin = try findErtsBin(allocator, io, release_dir);
    defer allocator.free(erts_bin);

    // Build the erlexec path
    const erlexec = try std.fmt.allocPrint(allocator, "{s}/erlexec", .{erts_bin});
    defer allocator.free(erlexec);

    // Point to the release
    const releases_dir = try std.fmt.allocPrint(allocator, "{s}/releases", .{release_dir});
    defer allocator.free(releases_dir);
    const lib_dir = try std.fmt.allocPrint(allocator, "{s}/lib", .{release_dir});
    defer allocator.free(lib_dir);

    const boot_file = try std.fmt.allocPrint(allocator, "{s}/0.1.0/start", .{releases_dir});
    defer allocator.free(boot_file);

    const sys_config = try std.fmt.allocPrint(allocator, "{s}/0.1.0/sys", .{releases_dir});
    defer allocator.free(sys_config);

    const vm_args = try std.fmt.allocPrint(allocator, "{s}/0.1.0/vm.args", .{releases_dir});
    defer allocator.free(vm_args);

    // Set env vars that erlexec needs
    try init.environ_map.put("ROOTDIR", release_dir);
    try init.environ_map.put("BINDIR", erts_bin);

    // Collect user arguments (skip argv[0])
    var user_args: std.ArrayList([]const u8) = .empty;
    defer user_args.deinit(allocator);

    var arg_iter = process.Args.Iterator.init(init.minimal.args);
    _ = arg_iter.skip();
    while (arg_iter.next()) |arg| {
        try user_args.append(allocator, arg);
    }

    // Build full argv for erlexec
    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(allocator);

    try argv.append(allocator, erlexec);
    try argv.append(allocator, "-boot_var");
    try argv.append(allocator, "RELEASE_LIB");
    try argv.append(allocator, lib_dir);
    try argv.append(allocator, "-boot");
    try argv.append(allocator, boot_file);
    try argv.append(allocator, "-config");
    try argv.append(allocator, sys_config);
    try argv.append(allocator, "-args_file");
    try argv.append(allocator, vm_args);
    try argv.append(allocator, "-extra");
    for (user_args.items) |arg| {
        try argv.append(allocator, arg);
    }

    // exec erlexec (replaces current process, never returns on success)
    const err = process.replace(io, .{
        .argv = argv.items,
        .environ_map = init.environ_map,
    });
    std.log.err("Failed to exec erlexec: {}", .{err});
    return err;
}

fn getCacheDir(allocator: Allocator, env: *const process.Environ.Map) ![]const u8 {
    if (comptime builtin.os.tag == .windows) {
        if (env.get("LOCALAPPDATA")) |local| return allocator.dupe(u8, local);
        if (env.get("TEMP")) |tmp| return allocator.dupe(u8, tmp);
        return error.NoCacheDir;
    }

    if (comptime builtin.os.tag == .macos) {
        if (env.get("HOME")) |home| {
            return std.fmt.allocPrint(allocator, "{s}/Library/Caches", .{home});
        }
        return error.NoCacheDir;
    }

    // Linux / other Unix
    if (env.get("XDG_CACHE_HOME")) |xdg| return allocator.dupe(u8, xdg);
    if (env.get("HOME")) |home| {
        return std.fmt.allocPrint(allocator, "{s}/.cache", .{home});
    }
    return allocator.dupe(u8, "/tmp");
}

fn extractRelease(allocator: Allocator, io: Io, release_dir: []const u8, marker_path: []const u8) !void {
    // Create the release directory
    makeDirRecursive(allocator, release_dir) catch |err| {
        std.log.err("Failed to create {s}: {}", .{ release_dir, err });
        return err;
    };

    // Write embedded tarball to a temp file
    const tmp_tar = try std.fmt.allocPrint(allocator, "{s}/release.tar.zst", .{release_dir});
    defer allocator.free(tmp_tar);

    const tar_file = try Dir.createFileAbsolute(io, tmp_tar, .{});
    try tar_file.writeStreamingAll(io, release_tar_zst);
    tar_file.close(io);

    // Extract using system tar
    var child = try process.spawn(io, .{
        .argv = &.{ "tar", "--zstd", "-xf", tmp_tar, "-C", release_dir },
    });
    const term = try child.wait(io);
    if (term != .exited or term.exited != 0) {
        std.log.err("tar extraction failed", .{});
        return error.TarExtractionFailed;
    }

    // Clean up temp file
    Dir.deleteFileAbsolute(io, tmp_tar) catch {};

    // Make erlexec executable
    makeExecutable(allocator, io, release_dir) catch {};

    // Write marker
    const marker_file = try Dir.createFileAbsolute(io, marker_path, .{});
    marker_file.close(io);
}

fn makeDirRecursive(allocator: Allocator, path: []const u8) !void {
    const path_z = try allocator.dupeZ(u8, path);
    defer allocator.free(path_z);

    const rc = std.c.mkdir(path_z, 0o755);
    if (rc == 0) return;

    const errno = std.posix.errno(rc);
    if (errno == .EXIST) return;
    if (errno == .NOENT) {
        if (mem.lastIndexOfScalar(u8, path, '/')) |idx| {
            if (idx > 0) {
                try makeDirRecursive(allocator, path[0..idx]);
                const rc2 = std.c.mkdir(path_z, 0o755);
                if (rc2 == 0) return;
                const errno2 = std.posix.errno(rc2);
                if (errno2 == .EXIST) return;
                return error.MkdirFailed;
            }
        }
    }
    return error.MkdirFailed;
}

fn makeExecutable(allocator: Allocator, io: Io, release_dir: []const u8) !void {
    const erts_bin = try findErtsBin(allocator, io, release_dir);
    defer allocator.free(erts_bin);

    chmodDirContents(allocator, io, erts_bin) catch {};

    const rel_bin = try std.fmt.allocPrint(allocator, "{s}/bin", .{release_dir});
    defer allocator.free(rel_bin);
    chmodDirContents(allocator, io, rel_bin) catch {};
}

fn chmodDirContents(allocator: Allocator, io: Io, dir_path: []const u8) !void {
    const dir = Dir.openDirAbsolute(io, dir_path, .{ .iterate = true }) catch return;
    var iter = dir.iterate();
    while (try iter.next(io)) |entry| {
        if (entry.kind == .file) {
            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
            defer allocator.free(full_path);
            const file = Dir.openFileAbsolute(io, full_path, .{ .mode = .read_write }) catch continue;
            defer file.close(io);
            const stat = try file.stat(io);
            file.setPermissions(io, stat.permissions) catch {};
            // Use chmod via C since Zig's permission API may not expose raw mode bits
            const path_z = allocator.dupeZ(u8, full_path) catch continue;
            defer allocator.free(path_z);
            _ = std.c.chmod(path_z, @intCast(stat.permissions.toMode() | 0o111));
        }
    }
}

fn findErtsBin(allocator: Allocator, io: Io, release_dir: []const u8) ![]const u8 {
    const dir = Dir.openDirAbsolute(io, release_dir, .{ .iterate = true }) catch return error.ErtsNotFound;
    var iter = dir.iterate();
    while (try iter.next(io)) |entry| {
        if (entry.kind == .directory and mem.startsWith(u8, entry.name, "erts-")) {
            return std.fmt.allocPrint(allocator, "{s}/{s}/bin", .{ release_dir, entry.name });
        }
    }
    return error.ErtsNotFound;
}
