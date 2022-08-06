const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;


const Semantics = language.Semantics;

const Module = Semantics.Module;
const Function = Semantics.Function;

const Taip = language.Taip;


const DimCount = Taip.DimCount;

pub const Value = struct {
    taip: Taip,
    data: Data,

    pub const Data = ValueData;

    pub fn init(taip: Taip, value: anytype) Value {
        const T = @TypeOf(value);
        const data = switch (T) {
            Data => value,
            Taip => Data { .taip = value },
            *Module => Data { .module = value },
            Data.Numeric => switch (taip) {
                .scalar => Data { .scalar = value, },
                .vector => Data { .vector = value, },
                .matrix => Data { .matrix = value, },
                // set to some random shit
                else => Data { .scalar = value },
            },
            *Function => Data { .function = value, },
            else => @compileError(@typeName(T) ++ " is not a value"),
        };
        if (std.debug.runtime_safety and std.meta.activeTag(taip) != std.meta.activeTag(data)) {
            @panic("value initialization with incorect type");
        }
        return Value {
            .taip = taip,
            .data = data,
        };
    }

    pub fn initConstScalar(comptime taip: Taip.Scalar, value: taip.Type()) Value {
        var numeric: Data.Numeric = undefined;
        numeric.setAs(taip.Type(), 0, 0, value);
        return init(Taip.init(taip), numeric);
    }

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (self.data) {
            .taip => |taip| try taip.format("", .{}, writer),
            .module => |module| try module.format("", .{}, writer),
            .scalar => |scalar| switch (self.taip.scalar) {
                .float => try writer.print("{d}", .{scalar.getAsScalar(.float, 0, 0)}),
                .signed => try writer.print("{d}", .{scalar.getAsScalar(.signed, 0, 0)}),
                .unsigned => try writer.print("{d}", .{scalar.getAsScalar(.unsigned, 0, 0)}),
            },
            .vector => |vector| try formatVector(writer, self.taip.vector, vector),
            .matrix => |matrix| try formatMatrix(writer, self.taip.matrix, matrix),
            .function => try self.taip.format("", .{}, writer),
        }
    }

    fn formatVector(writer: anytype, taip: Taip.Vector, numeric: Data.Numeric) @TypeOf(writer).Error!void {
        const dim_count = taip.dimCount();
        switch (taip.scalar) {
            .float => try formatVectorValue(writer, .float, dim_count, numeric),
            .signed => try formatVectorValue(writer, .signed, dim_count, numeric),
            .unsigned => try formatVectorValue(writer, .unsigned, dim_count, numeric),
        }
    }

    fn formatVectorValue(writer: anytype, comptime scalar: Taip.Scalar, dim_count: usize, numeric: Data.Numeric) @TypeOf(writer).Error!void {
        try writer.writeByte('[');
        var i: usize = 0;
        while (i < dim_count) : (i += 1) {
            if (i > 0) {
                try writer.writeAll(", ");
            }
            try writer.print("{d}", .{numeric.getAs(scalar.Type(), 0, i)});
        }
        try writer.writeByte(']');
    }

    fn formatMatrix(writer: anytype, taip: Taip.Matrix, numeric: Data.Numeric) @TypeOf(writer).Error!void {
        const row_count = @enumToInt(taip.row_count);
        const col_count = @enumToInt(taip.col_count);
        switch (taip.scalar) {
            .float => try formatMatrixValue(writer, .float, row_count, col_count, numeric),
            .signed => try formatMatrixValue(writer, .signed, row_count, col_count, numeric),
            .unsigned => try formatMatrixValue(writer, .unsigned, row_count, col_count, numeric),
        }
    }

    fn formatMatrixValue(writer: anytype, comptime scalar: Taip.Scalar, row_count: usize, col_count: usize, numeric: Data.Numeric) @TypeOf(writer).Error!void {
        try writer.writeByte('[');
        var r: usize = 0;
        while (r < row_count) : (r += 1) {
            try writer.writeByte('[');
            var c: usize = 0;
            while (c < col_count) : (c += 1) {
                try writer.print("{d}", .{numeric.getAs(scalar.Type(), r, c)});
            }
            try writer.writeByte(']');
        }
        try writer.writeByte(']');
    }

};

pub const ValueData = union(Taip.Tag) {

    taip: Taip,
    module: *Module,
    scalar: Numeric,
    vector: Numeric,
    matrix: Numeric,
    function: *Function,

    pub fn numeric(self: ValueData) Numeric {
        return switch (self) {
            .scalar => self.scalar,
            .vector => self.vector,
            .matrix => self.matrix,
            else => @panic("value is not numeric"),
        };
    }

    pub const Numeric = struct {
        n: [4][4]N,
        pub const N = u32;

        pub fn getAs(self: Numeric, comptime T: type, r: usize, c: usize) T {
            return fromBits(T, self.n[r][c]);
        }

        pub fn setAs(self: *Numeric, comptime T: type, r: usize, c: usize, val: T) void {
            if (T != void) {
                self.n[r][c] = toBits(T, val);
            }
        }

        pub fn getAsScalar(self: Numeric, comptime scalar: Taip.Scalar, r: usize, c: usize) scalar.Type() {
            return self.getAs(scalar.Type(), r, c);
        }

        pub fn toBits(comptime T: type, val: T) N {
            const Bits = std.meta.Int(.unsigned, @bitSizeOf(T));
            const bits = @bitCast(Bits, val);
            return @intCast(N, bits);
        }

        pub fn fromBits(comptime T: type, val: N) T {
            const Bits = std.meta.Int(.unsigned, @bitSizeOf(T));
            const bits = @truncate(Bits, val);
            return @bitCast(T, bits);
        }

    };
};

