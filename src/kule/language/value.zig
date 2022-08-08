const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;


const Semantics = language.Semantics;

const Module = Semantics.Module;
const Function = Semantics.Function;

const KType = language.KType;


const DimCount = KType.DimCount;

pub const Value = struct {
    ktype: KType,
    data: Data,

    pub const Data = ValueData;

    pub fn init(type_init: anytype, value: anytype) Value {
        const ktype = KType.init(type_init);
        const T = @TypeOf(value);
        const data = switch (T) {
            Data => value,
            KType => Data { .ktype = value },
            *Module => Data { .module = value },
            Data.Numeric => switch (ktype) {
                .scalar => Data { .scalar = value, },
                .vector => Data { .vector = value, },
                .matrix => Data { .matrix = value, },
                // set to some random shit
                else => Data { .scalar = value },
            },
            *Function => Data { .function = value, },
            else => @compileError(@typeName(T) ++ " is not a value"),
        };
        if (std.debug.runtime_safety) {
            const type_tag =  std.meta.activeTag(ktype);
            const data_tag =  std.meta.activeTag(data);
            if (type_tag != data_tag) {
                std.debug.panic("value initialization with incorrect type\ntype: {}\n value: {s}", .{ktype, @tagName(data_tag)});
            }
        }
        return Value {
            .ktype = ktype,
            .data = data,
        };
    }

    pub fn initConstScalar(comptime ktype: KType.Scalar, value: ktype.Type()) Value {
        var numeric: Data.Numeric = undefined;
        numeric.setAs(ktype.Type(), 0, 0, value);
        return init(KType.init(ktype), numeric);
    }

    pub fn format(self: Value, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        const multiline = fmt.len == 1 and fmt[0] == '~';
        switch (self.data) {
            .ktype => |ktype| try ktype.format("", .{}, writer),
            .module => |module| try module.format("", .{}, writer),
            .scalar => |scalar| switch (self.ktype.scalar) {
                .float => try writer.print("{d}", .{scalar.getAsScalar(.float, 0, 0)}),
                .signed => try writer.print("{d}", .{scalar.getAsScalar(.signed, 0, 0)}),
                .unsigned => try writer.print("{d}", .{scalar.getAsScalar(.unsigned, 0, 0)}),
            },
            .vector => |vector| try formatVector(writer, self.ktype.vector, vector),
            .matrix => |matrix| try formatMatrix(writer, multiline, self.ktype.matrix, matrix),
            .function => try self.ktype.format("", .{}, writer),
        }
    }

    fn formatVector(writer: anytype, ktype: KType.Vector, numeric: Data.Numeric) @TypeOf(writer).Error!void {
        const dim_count = ktype.dimCount();
        switch (ktype.scalar) {
            .float => try formatVectorValue(writer, .float, dim_count, numeric),
            .signed => try formatVectorValue(writer, .signed, dim_count, numeric),
            .unsigned => try formatVectorValue(writer, .unsigned, dim_count, numeric),
        }
    }

    fn formatVectorValue(writer: anytype, comptime scalar: KType.Scalar, dim_count: usize, numeric: Data.Numeric) @TypeOf(writer).Error!void {
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

    fn formatMatrix(writer: anytype, multiline: bool, ktype: KType.Matrix, numeric: Data.Numeric) @TypeOf(writer).Error!void {
        const row_count = @enumToInt(ktype.row_count);
        const col_count = @enumToInt(ktype.col_count);
        switch (ktype.scalar) {
            .float => try formatMatrixValue(writer, multiline, .float, row_count, col_count, numeric),
            .signed => try formatMatrixValue(writer, multiline, .signed, row_count, col_count, numeric),
            .unsigned => try formatMatrixValue(writer, multiline, .unsigned, row_count, col_count, numeric),
        }
    }

    fn formatMatrixValue(writer: anytype, multiline: bool, comptime scalar: KType.Scalar, row_count: usize, col_count: usize, numeric: Data.Numeric) @TypeOf(writer).Error!void {
        if (!multiline) try writer.writeByte('[');
        var r: usize = 0;
        while (r < row_count) : (r += 1) {
            try writer.writeByte('[');
            var c: usize = 0;
            while (c < col_count) : (c += 1) {
                if (c > 0) {
                    try writer.writeAll(", ");
                }
                try writer.print("{d}", .{numeric.getAs(scalar.Type(), r, c)});
            }
            try writer.writeByte(']');
            if (multiline and r < (row_count - 1)) try writer.writeByte('\n');
        }
        if (!multiline) try writer.writeByte(']');
    }

};

pub const ValueData = union(KType.Tag) {

    ktype: KType,
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

        pub fn getAsScalar(self: Numeric, comptime scalar: KType.Scalar, r: usize, c: usize) scalar.Type() {
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

