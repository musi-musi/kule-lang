const std = @import("std");

const meta = std.meta;

pub const Type = union(enum) {
    type_value: void,
    scalar: Scalar,
    vector: Vector,
    module: void,
};

pub const Scalar = union(enum) {
    number: void,
    float: ScalarSize,
    sign: ScalarSize,
    unsign: ScalarSize,



    pub fn Val(comptime s: Scalar) type {
        return switch (s) {
            .number => f64,
            .float => |size| meta.Float(size.bits()),
            .sign => |size| meta.Int(.signed, size.bits()),
            .unsign => |size| meta.Int(.unsigned, size.bits()),
        };
    }
};

pub const ScalarSize = enum(u32) {
    s8  = 1,
    s16 = 2,
    s32 = 3,
    s64 = 4,

    pub fn bits(s: ScalarSize) usize {
        return @enumToInt(s) * 8;
    }
};


pub const Vector = struct {
    scalar: Scalar,
    size: VectorSize,

    pub fn Val(comptime v: Vector) type {
        return [@enumToInt(v.size)]v.scalar.Val();
    }
};

pub const VectorSize = enum(u32) {
    v1 = 1,
    v2 = 2,
    v3 = 3,
    v4 = 4,
};