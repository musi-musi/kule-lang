const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;

const meta = std.meta;

const Semantics = language.Semantics;

const Module = Semantics.Module;
const Function = Semantics.Function;

pub const taips = struct {
    pub const taip: Taip = .taip;
    pub const module: Taip = .module;
    pub fn number(kind: ?Taip.Number.Kind, cols: usize, rows: usize) Taip {
        return .{
            .number = Taip.Number.init(kind, cols, rows),
        };
    }
    pub fn function(allocator: Allocator, param_taips: []Taip, return_taip: Taip) Allocator.Error!Taip {
        const func = try allocator.create(Taip.Func);
        func.* = Taip.Func {
            .param_taips = param_taips,
            .return_taip = return_taip,
        };
        return Taip {
            .function = func,
        };
    }
};

pub const Taip = union(enum) {
    
    taip: void,
    
    module: void,
    
    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    function: *Func,

    pub fn taipOfFunction(allocator: Allocator, function: *Function) !Taip {
        const params = function.params;
        const param_taips = try allocator.alloc(Function.Param, params.len);
        for (params) |param, i| {
            param_taips[i] = param.taip;
        }
        return try taips.function(allocator, param_taips, function.return_taip);
    }

    pub fn format(self: Taip, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .taip => try writer.writeAll("type"),
            .module => try writer.writeAll("module"),
            .scalar => |scalar| try scalar.format(writer),
            .vector => |vector| try vector.format(writer),
            .matrix => |matrix| try matrix.format(writer),
            .function => |function| try function.format(writer),
        }
    }


    pub const Func = struct {
        param_taips: []Taip,
        return_taip: Taip,

        pub fn init(param_taips: []Taip, return_taip: Taip) Func {
            return .{
                .param_taips = param_taips,
                .return_taip = return_taip,
            };
        }


        fn format(f: Func, writer: anytype) @TypeOf(writer).Error!void {
            try writer.writeAll("fn(");
            for (f.param_taips) |taip, i| {
                if (i > 0) {
                    try writer.writeAll(", ");
                }
                try taip.format("", .{}, writer);
            }
            try writer.writeAll("): ");
            try f.return_taip.format("", .{}, writer);
        }
    };

    pub const ScalarTaip = enum(u32) {
        float,
        signed,
        unsigned,

        pub fn Type(comptime s: ScalarTaip) type {
            return switch (s) {
                .float => f32,
                .signed => i32,
                .unsigned => u32,
            };
        }

        pub fn Union(comptime Field: anytype) type {
            const values = std.enums.values(ScalarTaip);
            var fields: [values.len]std.builtin.TypeInfo.UnionField = undefined;
            for (values) |value, i| {
                fields[i].name = @tagName(value);
                fields[i].field_type = Field(value.Type());
                fields[i].alignment = @alignOf(fields[i].field_type);
            }
            return @Type(.{
                .Union = .{
                    .layout = .Auto,
                    .tag_type = ScalarTaip,
                    .fields = &fields,
                    .decls = &.{},
                },
            });
        }

    };

    pub const Scalar = struct {
        taip: ScalarTaip,
        mode: Mode = .static,

        pub const Mode = enum {
            static,
            dynamic,
        };

        fn format(self: Scalar, writer: anytype) @TypeOf(writer).Error!void {
            switch (self.mode) {
                .static => try writer.writeAll(@tagName(self.taip)),
                .dynamic => try writer.writeAll("number"),
            }
        }

        pub fn Type(comptime self: Scalar) type {
            switch (self.mode) {
                .static => self.taip.Type(),
                .dynamic => self.taip.Union(struct{
                    fn f(T: type) type { return T; }
                }.f),
            }
        }
    };

    pub const Vector = struct {
        scalar: Scalar,
        len: Dims,

        pub fn Type(comptime v: Vector) type {
            return [@enumToInt(v.len)]v.scalar.Type();
        }

        fn format(self: Vector, writer: anytype) @TypeOf(writer).Error!void {
            try self.scalar.format(writer);
            try writer.writeByte(@tagName(self.len)[1]);
        }

    };

    pub const Matrix = struct {
        scalar: Scalar,
        rows: Dims,
        cols: Dims,

        pub fn Type(comptime v: Vector) type {
            return [@enumToInt(v.rows)][@enumToInt(v.cols)]v.scalar.Type();
        }
        
        fn format(self: Matrix, writer: anytype) @TypeOf(writer).Error!void {
            try self.scalar.format(writer);
            try writer.writeByte(@tagName(self.rows)[1]);
            try writer.writeByte('x');
            try writer.writeByte(@tagName(self.cols)[1]);
        }
    };

    pub const Dims = enum(u32) {
        d2 = 2,
        d3 = 3,
        d4 = 4,
    };



};