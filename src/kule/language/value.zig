const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;


const Semantics = language.Semantics;

const Module = Semantics.Module;
const Function = Semantics.Function;

const Taip = language.Taip;

const ScalarTp = Taip.Scalar;
const VectorTp = Taip.Vector;
const MatrixTp = Taip.Matrix;

const Dims = Taip.Dims;

pub const Value = struct {
    taip: Taip,
    data: Data,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (self.data) {
            .taip => |taip| try taip.format("", .{}, writer),
            .module => |module| try module.format("", .{}, writer),
            .scalar => |scalar| switch (scalar) {
                .float => |float| try writer.print("{d}", .{float}),
                .signed => |signed| try writer.print("{d}", .{signed}),
                .unsigned => |unsigned| try writer.print("{d}", .{unsigned}),
            },
            .vector => |vector| try formatVector(writer, self.taip.vector, vector),
            .matrix => |matrix| try formatMatrix(writer, self.taip.matrix, matrix),
            .function => try self.taip.format("", .{}, writer),
        }
    }

    fn formatVector(writer: anytype, vec_taip: Taip.Vector, vec: Data.Vector) @TypeOf(writer).Error!void {
        switch (vec_taip.scalar.taip) {
            .float => switch (vec_taip.len) {
                .d2 => try formatVectorValue(writer, @enumToInt(Dims.d2), vec.float),
                .d3 => try formatVectorValue(writer, @enumToInt(Dims.d3), vec.float),
                .d4 => try formatVectorValue(writer, @enumToInt(Dims.d4), vec.float),
            },
            .signed => switch (vec_taip.len) {
                .d2 => try formatVectorValue(writer, @enumToInt(Dims.d2), vec.signed),
                .d3 => try formatVectorValue(writer, @enumToInt(Dims.d3), vec.signed),
                .d4 => try formatVectorValue(writer, @enumToInt(Dims.d4), vec.signed),
            },
            .unsigned => switch (vec_taip.len) {
                .d2 => try formatVectorValue(writer, @enumToInt(Dims.d2), vec.unsigned),
                .d3 => try formatVectorValue(writer, @enumToInt(Dims.d3), vec.unsigned),
                .d4 => try formatVectorValue(writer, @enumToInt(Dims.d4), vec.unsigned),
            },
        }
    }

    fn formatVectorValue(writer: anytype, comptime len: usize, value: anytype) @TypeOf(writer).Error!void {
        try writer.writeByte('[');
        for (value[0..len]) |v, i| {
            if (i > 0) {
                try writer.writeAll(", ");
            }
            try writer.print("{d}", .{v});
        }
        try writer.writeByte(']');
    }

    fn formatMatrix(writer: anytype, mat_taip: Taip.Matrix, mat: Data.Matrix) @TypeOf(writer).Error!void {
        switch (mat_taip.scalar.taip) {
            .float => switch (mat_taip.rows) {
                .d2 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d2), mat.float),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d3), mat.float),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d4), mat.float),
                },
                .d3 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d2), mat.float),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d3), mat.float),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d4), mat.float),
                },
                .d4 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d2), mat.float),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d3), mat.float),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d4), mat.float),
                },
            },
            .signed => switch (mat_taip.rows) {
                .d2 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d2), mat.signed),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d3), mat.signed),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d4), mat.signed),
                },
                .d3 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d2), mat.signed),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d3), mat.signed),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d4), mat.signed),
                },
                .d4 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d2), mat.signed),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d3), mat.signed),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d4), mat.signed),
                },
            },
            .unsigned => switch (mat_taip.rows) {
                .d2 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d2), mat.unsigned),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d3), mat.unsigned),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d2), @enumToInt(Dims.d4), mat.unsigned),
                },
                .d3 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d2), mat.unsigned),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d3), mat.unsigned),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d3), @enumToInt(Dims.d4), mat.unsigned),
                },
                .d4 => switch (mat_taip.cols) {
                    .d2 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d2), mat.unsigned),
                    .d3 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d3), mat.unsigned),
                    .d4 => try formatMatrixValue(writer, @enumToInt(Dims.d4), @enumToInt(Dims.d4), mat.unsigned),
                },
            },
        }
    }

    fn formatMatrixValue(writer: anytype, comptime rows: usize, comptime cols: usize, value: anytype) @TypeOf(writer).Error!void {
        try writer.writeByte('[');
        for (value[0..rows]) |row| {
            try writer.writeByte('[');
            for (row[0..cols]) |v, c| {
                if (c > 0) {
                    try writer.writeAll(", ");
                }
                try writer.print("{d}", .{v});
            }
            try writer.writeByte(']');
        }
        try writer.writeByte(']');
    }

    pub const Data = union(enum) {
    
        taip: Taip,
        module: *Module,
        scalar: Scalar,
        vector: Vector,
        matrix: Matrix,
        function: *Function,

        pub const Scalar = Taip.ScalarTaip.Union(struct {
            fn Field(comptime T: type) type {
                return T;
            }
        }.Field);

        pub const Vector = Taip.ScalarTaip.Union(struct {
            fn Field(comptime T: type) type {
                return [4]T;
            }
        }.Field);

        pub const Matrix = Taip.ScalarTaip.Union(struct {
            fn Field(comptime T: type) type {
                return [4][4]T;
            }
        }.Field);
    
    };

    pub fn init(taip: Taip, value: anytype) Value{
        const T = @TypeOf(value);
        switch (T) {
            Taip => return .{
                .taip = taip,
                .data = .{ .taip = value },
            },
            *Module => return .{
                .taip = taip,
                .data = .{ .module = value },
            },
            Data.Scalar => return .{
                .taip = taip,
                .data = .{ .scalar = value, },
            },
            Data.Vector => return .{
                .taip = taip,
                .data = .{ .vector = value, },
            },
            Data.Matrix => return .{
                .taip = taip,
                .data = .{ .matrix = value, },
            },
            *Function => return .{
                .taip = taip,
                .data = .{ .function = value, },
            },
            else => @compileError(@typeName(T) ++ " is not a value"),
        }
    }
        
};