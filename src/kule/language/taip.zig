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

    pub fn init(value: anytype) Taip {
        const V = @TypeOf(value);
        switch (V) {
            Scalar => return Taip{ .scalar = value },
            Vector => return Taip{ .vector = value },
            Matrix => return Taip{ .matrix = value },
            *Func => return Taip{ .function = value },
            else => {
                if (comptime meta.trait.is(.EnumLiteral)(V)) {
                    switch (value) {
                        .taip => return .taip,
                        .module => return .module,
                        .number => return init(Scalar{ .taip = .float, .mode = .dynamic }),
                        else => @compileError(@tagName(value) ++ " is not an accepted Taip tag"),
                    }
                }
                else {
                    @compileError(@typeName(value) ++ " is not a valid Taip value");
                }
            }
        }
    }

    pub fn taipOfFunction(allocator: Allocator, function: *Function) !?Taip {
        const params = function.params;
        const param_taips = try allocator.alloc(Taip, params.len);
        for (params) |param, i| {
            if (param.taip) |param_taip| {
                param_taips[i] = param_taip;
            }
            else {
                allocator.free(param_taips);
                return null;
            }
        }
        if (function.return_taip) |return_taip| {
            return try taips.function(allocator, param_taips, return_taip);
        }
        else {
            allocator.free(param_taips);
            return null;
        }
    }

    pub fn canCoerceTo(have: Taip, want: Taip) bool {
        switch (have) {
            .scalar => {
                if (want.scalarComponent()) |want_scalar| {
                    return have.scalar.canCoerceTo(want_scalar);
                }
                else {
                    return false;
                }
            },
            .vector => return (
                want == .vector
                and have.vector.scalar.canCoerceTo(want.vector.scalar)
                and have.vector.len == want.vector.len
            ),
            .matrix => return (
                want == .matrix
                and have.matrix.scalar.canCoerceTo(want.matrix.scalar)
                and have.matrix.rows == want.matrix.rows
                and have.matrix.cols == want.matrix.cols
            ),
            else => return have.eql(want),
        }
    }

    fn eql(a: Taip, b: Taip) bool {
        return meta.activeTag(a) == meta.activeTag(b) and switch(a) {
            .scalar => (
                a.scalar.taip == b.scalar.taip
                and a.scalar.mode == b.scalar.mode
            ),
            .vector => (
                a.vector.scalar.taip == b.vector.scalar.taip
                and a.vector.scalar.mode == b.vector.scalar.mode
                and a.vector.len == b.vector.len
            ),
            .matrix => (
                a.matrix.scalar.taip == b.matrix.scalar.taip
                and a.matrix.scalar.mode == b.matrix.scalar.mode
                and a.matrix.rows == b.matrix.rows
                and a.matrix.cols == b.matrix.cols
            ),
            .function => (
                a.function.return_taip.eql(b.function.return_taip)
                and for (a.function.param_taips) |param_taip, i| {
                    if (!param_taip.eql(b.function.param_taips[i])) {
                        break false;
                    }
                }
                else true
            ),
            else => true,
        };
    }

    fn scalarComponent(self: Taip) ?Scalar {
        switch (self) {
            .scalar => return self.scalar,
            .vector => return self.vector.scalar,
            .matrix => return self.matrix.scalar,
            else => return null,
        }
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
        float = 0,
        signed = 1,
        unsigned = 2,

        pub fn Type(comptime s: ScalarTaip) type {
            return switch (s) {
                .float => f32,
                .signed => i32,
                .unsigned => u32,
            };
        }

        pub fn Union(comptime Field: fn(type) type) type {
            return union(ScalarTaip) {
                float: Field(Type(.float)),
                signed: Field(Type(.signed)),
                unsigned: Field(Type(.unsigned)),
            };
        }

    };

    pub const Scalar = struct {
        taip: ScalarTaip,
        mode: Mode = .static,

        pub const Mode = enum {
            static,
            dynamic,
        };

        pub fn vector(self: Scalar, len: Dims) Vector {
            return Vector{
                .scalar = self,
                .len = len,
            };
        }

        pub fn matrix(self: Scalar, rows: Dims, cols: Dims) Matrix {
            return Matrix{
                .scalar = self,
                .rows = rows,
                .cols = cols,
            };
        }

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

        fn canCoerceTo(have: Scalar, want: Scalar) bool {
            if (have.mode == .dynamic) {
                return true;
            }
            else if (want.mode == .dynamic) {
                return false;
            } 
            else {
                return @enumToInt(have.taip) >= @enumToInt(want.taip);
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