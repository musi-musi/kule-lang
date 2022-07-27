const std = @import("std");
const kule = @import("../kule.zig");

const json = @import("json.zig");

const String = json.String;

const Allocator = std.mem.Allocator;


const hashString = std.hash_map.hashString;

const ProjectMap = std.StringHashMapUnmanaged(*Project);
const FileMap = std.StringHashMapUnmanaged(*File);

const Arena = std.heap.ArenaAllocator;

pub const Workspace = struct {
    allocator: Allocator,
    projects: ProjectMap,
    files: FileMap,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self {
            .allocator = allocator,
            .projects = .{},
            .files =  .{},
        };
    }

    pub fn deinit(self: *Self) void {
        var projects = self.projects.valueIterator();
        while (projects.next()) |project| {
            project.*.deinit();
        }
        var files = self.files.valueIterator();
        while (files.next()) |file| {
            file.*.deinit();
        }
        self.projects.deinit(self.allocator);
        self.files.deinit(self.allocator);
    }

    fn dupes(self: *Self, str: []const u8) ![]const u8 {
        return self.allocator.dupe(u8, str);
    }

    pub fn addProject(s: *Self, uri: []const u8) !*Project {
        var project = try s.allocator.create(Project);
        project.* = try Project.init(s, uri);
        try s.projects.put(s.allocator, project.uri, project);
        return project;
    }
    

    pub fn removeProject(s: *Self, uri: []const u8) !void {
        if (s.projects.fetchRemove(uri)) |kv| {
            var project = kv.value;
            var files = project.files.valueIterator();
            for (files.next()) |file| {
                _ = s.files.remove(file.uri);
            }
            project.deinit(s.allocator);
            s.allocator.destroy(project);
            return true;
        }
        else {
            return false;
        }
    }

    pub fn addOrUpdateFile(s: *Self, uri: []const u8, text: String) !*File {
        if (s.files.get(uri)) |file|{
            try file.updateText(text);
            return file;
        }
        else {
            const project = s.fileProject(uri);
            const file = try s.allocator.create(File);
            file.* = try File.init(s, project, uri, text);
            if (project != null) {
                try project.?.files.put(s.allocator, file.name, file);
            }
            try s.files.put(s.allocator, file.uri, file);
            return file;
        }
    }

    pub fn removeFile(s: *Self, uri: []const u8) bool {
        if (s.files.fetchRemove(uri)) |kv| {
            var file = kv.value;
            if (file.project) |project| {
                _ = project.files.remove(file.name);
            }
            file.deinit();
            s.allocator.destroy(file);
            return true;
        }
        else {
            return false;
        }
    }

    fn fileProject(s: *Self, file_uri: []const u8) ?*Project {
        var projects = s.projects.valueIterator();
        while (projects.next()) |ptr| {
            const project = ptr.*;
            if (std.mem.startsWith(u8, file_uri, project.uri)) {
                return project;
            }
        }
        else {
            return null;
        }
    }

};

pub const Project = struct {
    uri: []const u8,
    files: FileMap,

    workspace: *Workspace,

    const Self = @This();

    fn init(ws: *Workspace, uri: []const u8) !Self {
        return Self {
            .uri = try ws.dupes(uri),
            .files = .{},
            .workspace = ws,
        };
    }

    pub fn allocator(self: Self) Allocator {
        return self.workspace.allocator;
    }

    fn deinit(self: *Self) void {
        self.files.deinit(self.allocator());
        self.allocator().free(self.uri);
    }

};

pub const File = struct {
    uri: []const u8,

    workspace: *Workspace,
    project: ?*Project,

    name: []const u8,
    text: []u8,

    const Self = @This();

    fn init(ws: *Workspace, project: ?*Project, uri: []const u8, text: String) !Self {
        const uri_f = try ws.dupes(uri);
        const name = if (project) |p| uri_f[p.uri.len+1..] else uri_f;
        return Self {
            .uri = uri_f,
            .workspace = ws,
            .project = project,
            .name = name,
            .text = try text.unescapeAlloc(ws.allocator),
        };
    }

    fn updateText(self: *Self, text: String) !void {
        // var len = text.realLen();
        const a = self.allocator();
        a.free(self.text);
        self.text = try text.unescapeAlloc(a);
        // self.text = try allocator.alloc(u8, len);
        // if (len != self.text.len) {
        //     self.text = try self.allocator().realloc(self.text, len);
        // }
        // text.unescapeAssumeLen(self.text);
    }

    fn allocator(self: Self) Allocator {
        return self.workspace.allocator;
    }

    fn deinit(self: *Self) void {
        self.allocator().free(self.uri);
        self.allocator().free(self.text);
    }

    
};