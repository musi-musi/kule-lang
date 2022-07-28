const std = @import("std");
const expression = @import("expression.zig");

const meta = std.meta;

const Module = expression.Module;

const Operator = expression.Operator;

pub const number_type = Type {
    .scalar = .number,
};

pub const builtin_types = std.ComptimeStringMap(Type, .{
    .{"type", Type{ .type_value = {}}},
    .{"module", Type{ .module = {}}},
    .{"number", number_type },
    .{"f8",  floatType(.s8)},
    .{"f16",  floatType(.s16)},
    .{"f32",  floatType(.s32)},
    .{"f64",  floatType(.s64)},
    .{"i8",  signType(.s8)},
    .{"i16",  signType(.s16)},
    .{"i32",  signType(.s32)},
    .{"i64",  signType(.s64)},
    .{"u8",  unsignType(.s8)},
    .{"u16",  unsignType(.s16)},
    .{"u32",  unsignType(.s32)},
    .{"u64",  unsignType(.s64)},
    .{"numberv1", vectorType(number_type.scalar, .v1)},
    .{"numberv2", vectorType(number_type.scalar, .v2)},
    .{"numberv3", vectorType(number_type.scalar, .v3)},
    .{"numberv4", vectorType(number_type.scalar, .v4)},
    .{"f8v1", vectorType(floatType(.s8).scalar, .v1)},
    .{"f8v2", vectorType(floatType(.s8).scalar, .v2)},
    .{"f8v3", vectorType(floatType(.s8).scalar, .v3)},
    .{"f8v4", vectorType(floatType(.s8).scalar, .v4)},
    .{"f16v1", vectorType(floatType(.s16).scalar, .v1)},
    .{"f16v2", vectorType(floatType(.s16).scalar, .v2)},
    .{"f16v3", vectorType(floatType(.s16).scalar, .v3)},
    .{"f16v4", vectorType(floatType(.s16).scalar, .v4)},
    .{"f32v1", vectorType(floatType(.s32).scalar, .v1)},
    .{"f32v2", vectorType(floatType(.s32).scalar, .v2)},
    .{"f32v3", vectorType(floatType(.s32).scalar, .v3)},
    .{"f32v4", vectorType(floatType(.s32).scalar, .v4)},
    .{"f64v1", vectorType(floatType(.s64).scalar, .v1)},
    .{"f64v2", vectorType(floatType(.s64).scalar, .v2)},
    .{"f64v3", vectorType(floatType(.s64).scalar, .v3)},
    .{"f64v4", vectorType(floatType(.s64).scalar, .v4)},
    .{"i8v1", vectorType(signType(.s8).scalar, .v1)},
    .{"i8v2", vectorType(signType(.s8).scalar, .v2)},
    .{"i8v3", vectorType(signType(.s8).scalar, .v3)},
    .{"i8v4", vectorType(signType(.s8).scalar, .v4)},
    .{"i16v1", vectorType(signType(.s16).scalar, .v1)},
    .{"i16v2", vectorType(signType(.s16).scalar, .v2)},
    .{"i16v3", vectorType(signType(.s16).scalar, .v3)},
    .{"i16v4", vectorType(signType(.s16).scalar, .v4)},
    .{"i32v1", vectorType(signType(.s32).scalar, .v1)},
    .{"i32v2", vectorType(signType(.s32).scalar, .v2)},
    .{"i32v3", vectorType(signType(.s32).scalar, .v3)},
    .{"i32v4", vectorType(signType(.s32).scalar, .v4)},
    .{"i64v1", vectorType(signType(.s64).scalar, .v1)},
    .{"i64v2", vectorType(signType(.s64).scalar, .v2)},
    .{"i64v3", vectorType(signType(.s64).scalar, .v3)},
    .{"i64v4", vectorType(signType(.s64).scalar, .v4)},
    .{"u8v1", vectorType(unsignType(.s8).scalar, .v1)},
    .{"u8v2", vectorType(unsignType(.s8).scalar, .v2)},
    .{"u8v3", vectorType(unsignType(.s8).scalar, .v3)},
    .{"u8v4", vectorType(unsignType(.s8).scalar, .v4)},
    .{"u16v1", vectorType(unsignType(.s16).scalar, .v1)},
    .{"u16v2", vectorType(unsignType(.s16).scalar, .v2)},
    .{"u16v3", vectorType(unsignType(.s16).scalar, .v3)},
    .{"u16v4", vectorType(unsignType(.s16).scalar, .v4)},
    .{"u32v1", vectorType(unsignType(.s32).scalar, .v1)},
    .{"u32v2", vectorType(unsignType(.s32).scalar, .v2)},
    .{"u32v3", vectorType(unsignType(.s32).scalar, .v3)},
    .{"u32v4", vectorType(unsignType(.s32).scalar, .v4)},
    .{"u64v1", vectorType(unsignType(.s64).scalar, .v1)},
    .{"u64v2", vectorType(unsignType(.s64).scalar, .v2)},
    .{"u64v3", vectorType(unsignType(.s64).scalar, .v3)},
    .{"u64v4", vectorType(unsignType(.s64).scalar, .v4)},
});

fn floatType(size: ScalarSize) Type {
    return .{ .scalar = .{ .float = size } };
}
fn signType(size: ScalarSize) Type {
    return .{ .scalar = .{ .sign = size } };
}
fn unsignType(size: ScalarSize) Type {
    return .{ .scalar = .{ .unsign = size } };
}

fn vectorType(scalar: Scalar, size: VectorSize) Type {
    return .{ .vector = .{ .scalar = scalar, .size = size, } };
}

pub fn valueType(value: Value) ?Type {
    return builtin_types.get(@tagName(meta.activeTag(value)));
}


pub const Value = union(enum) {
    num: [4]u64,    // raw bits for everything from a u8 to a f64_4
    type_value: Type,
    module: *Module,

    pub fn getNumAs(v: Value, comptime T: type, index: usize) T {
        const Bits = std.meta.Int(.unsigned, @sizeOf(T));
        const bits = @truncate(Bits, v.num[index]);
        return @bitCast(T, bits);
    }

    pub fn setNumAs(v: Value, comptime T: type, index: usize, val: T) void {
        const Bits = std.meta.Int(.unsigned, @sizeOf(T));
        const bits = @bitCast(Bits, val);
        v.num[index] = @as(u64, bits);
    }

    pub fn fromBits(comptime T: type, v: u64) T {
        const Bits = std.meta.Int(.unsigned, @sizeOf(T));
        const bits = @truncate(Bits, v);
        return @bitCast(T, bits);
    }

    pub fn toBits(comptime T: type, v: T) u64 {
        const Bits = std.meta.Int(.unsigned, @sizeOf(T));
        const bits = @bitCast(Bits, v);
        return @as(u64, bits);
    }

    pub const Formatted = struct {
        val: Value,
        val_type: Type,

        fn n(v: Formatted, comptime T: type, i: usize) T {
            return v.val.getNumAs(T, i);
        }

        pub fn format(v: Formatted, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
            switch (v.val) {
                .type_value => |t| try w.print("[type {s}]", .{t.name()}),
                .module => try w.writeAll("[module]"),
                .num => {
                    const scalar = v.val_type.scalarComponent().?;
                    const vsize = @enumToInt(v.val_type.vectorSize().?);
                    if (vsize > 1) {
                        try w.writeByte('[');
                    }
                    for (([4]void{})[0..vsize]) |_, i| {
                        if (i > 0) {
                            try w.writeAll(", ");
                        }
                        switch (scalar) {
                            .number => try w.print("{d}", v.n(f64, i)),
                            .float => |size| {
                                switch (size) {
                                    .s8 => {},
                                    .s16 => try w.print("{d}", v.n(f16, i)),
                                    .s32 => try w.print("{d}", v.n(f32, i)),
                                    .s64 => try w.print("{d}", v.n(f64, i)),
                                }
                            },
                            .sign => |size| {
                                switch (size) {
                                    .s8 => try w.print("{d}", v.n(i8, i)),
                                    .s16 => try w.print("{d}", v.n(i16, i)),
                                    .s32 => try w.print("{d}", v.n(i32, i)),
                                    .s64 => try w.print("{d}", v.n(i64, i)),
                                }
                            },
                            .unsign => |size| {
                                switch (size) {
                                    .s8 => try w.print("{d}", v.n(u8, i)),
                                    .s16 => try w.print("{d}", v.n(u16, i)),
                                    .s32 => try w.print("{d}", v.n(u32, i)),
                                    .s64 => try w.print("{d}", v.n(u64, i)),
                                }
                            },
                        }
                    }
                    if (vsize > 1) {
                        try w.writeByte(']');
                    }
                }
            }
        }


    };

    pub fn formatted(val: Value, val_type: Type) Formatted {
        return Formatted {
            .val = val,
            .val_type = val_type,
        };
    }

};

pub const Type = union(enum) {
    type_value: void,
    module: void,
    scalar: Scalar,
    vector: Vector,

    pub fn Val(comptime t: Type) type {
        return switch(t) {
            .type_value => Type,
            .module => *Module,
            .scalar => |scalar| scalar.Val(),
            .vector => |vector| vector.Val(),
        };
    }

    pub fn canCoerceTo(have: Type, want: Type) bool {
        if (have == want) return true;
        switch (have) {
            .type_value, .module => return false,
            else => {},
        }
        switch (want) {
            .type_value, .module => return false,
            else => {},
        }
        const have_vs = have.vectorSize().?;
        const want_vs = want.vectorSize().?;
        if ((have_vs != .v1 or want_vs == .v1) and have_vs != want_vs) {
            return false;
        }
        const have_s = have.scalarComponent().?;
        const want_s = want.scalarComponent().?;
        if (have_s == .number or want_s == .number) {
            return true;
        }
        if (@enumToInt(want_s.size()) > @enumToInt(have_s.size())) {
            return false;
        }
        switch (have_s) {
            .float => return want_s == .float,
            .sign => return want_s != .unsign,
            .unsign => return true,
            .number => return true,
        }
    }

    pub fn binaryResult(lhs: Type, rhs: Type, op: Operator) ?Type {
        if (op == .cast and rhs == .type_value) {
            return Type { .type_value = {}};
        }
        const lhs_size = lhs.vectorSize() orelse return null;
        const rhs_size = rhs.vectorSize() orelse return null;
        if (lhs_size == .v1 or rhs_size == .v1 or lhs_size == rhs_size) {
            const lhs_s = lhs.scalarComponent().?;
            const rhs_s = rhs.scalarComponent().?;
            const vsize = @intToEnum(VectorSize, std.math.max(
                @enumToInt(lhs_size),
                @enumToInt(rhs_size),
            ));
            if (lhs_s == rhs_s) {
                return vector(lhs_s, vsize);
            }
            else if (lhs_s == .number) {
                return vector(rhs_s, vsize);
            }
            else if (rhs_s == .number) {
                return vector(lhs_s, vsize);
            }
            else {
                const lss = lhs_s.size();
                const rss = rhs_s.size();
                const ssize = @intToEnum(ScalarSize, std.math.max(
                    @enumToInt(lss),
                    @enumToInt(rss),
                ));
                const lst = meta.activeTag(lhs_s);
                const rst = meta.activeTag(rhs_s);
                switch (lst) {
                    .float => return vector(
                        Scalar{ .float = ssize },
                    vsize),
                    .sign => return vector(switch (rst) {
                        .float => Scalar{ .float = ssize},
                        else => Scalar{ .sign = ssize},
                    }, vsize),
                    .unsign => return vector(switch (rst) {
                        .float => Scalar{ .float = ssize},
                        .sign => Scalar{ .sign = ssize},
                        else => Scalar{ .unsign = ssize},
                    }, vsize),
                    else => unreachable,
                }
            }

        }
        else {
            return null;
        }
    }

    pub fn unaryResult(operand: Type, operator: Operator) ?Type {
        const scalar = operand.scalarComponent() orelse return null;
        const size = operand.vectorSize() orelse return null;
        switch (operator) {
            .pos => return operand,
            .neg => {
                if (scalar == .unsign) {
                    return vector(.{.sign = scalar.size()}, size);
                }
                else {
                    return operand;
                }
            },
            else => return null,
        }
    }

    pub fn scalarComponent(self: Type) ?Scalar {
        return switch (self) {
            .scalar => |scalar| scalar,
            .vector => |vector| vector.scalar,
            else => null,
        };
    }

    pub fn vectorSize(self: Type) ?VectorSize {
        return switch (self) {
            .scalar => .v1,
            .vector => |vector| vector.size,
            else => null,
        };
    }

    pub fn vector(scalar: Scalar, size: VectorSize) Type {
        if (size == .v1) {
            return Type { .scalar = scalar, };
        }
        else {
            return Type {
                .vector = .{
                    .scalar = scalar,
                    .size = size,
                },
            };
        }
    }

    pub fn name(t: Type) []const u8 {
        return switch (t) {
            .type_val => "type",
            .module => "module",
            .scalar => |scalar| scalar.name(),
            .vector => |vector| vector.name(),
        };
    }

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

    pub fn size(s: Scalar) ScalarSize {
        return switch (s) {
            .number => .s64,
            .float => |float| float,
            .sign => |sign| sign,
            .unsign => |unsign| unsign,
        };
    }

    pub fn name(s: Scalar) []const u8 {
        const names = switch (s) {
            .number => return "number",
            .float => comptime ScalarSize.names("f"),
            .sign => comptime ScalarSize.names("i"),
            .unsign => comptime ScalarSize.names("u"),
        };
        return switch (s.size) {
            .s8 => names[0],
            .s16 => names[1],
            .s32 => names[2],
            .s64 => names[3],
        };
    }

    pub const all: []const Scalar = blk: {
        var list: []const Scalar = &.{ .number = {}};
        for (std.enums.Values(ScalarSize)) |scalar_size| {
            list = list ++ &[_]Scalar{
                Scalar{ .float = scalar_size },
                Scalar{ .sign = scalar_size },
                Scalar{ .unsign = scalar_size },
            };
        }
        break :blk list;
    };

    fn indexInAll(s: Scalar) usize {
        const tag_i = switch (s) {
            .number => return 0,
            .float => 1,
            .sign => 2,
            .unsign => 3,
        };
        const size_i = switch (s.size()) {
            .s8 => 0,
            .s16 => 1,
            .s32 => 2,
            .s64 => 3,
        };
        return 1 + (size_i * 3) + tag_i;
    }
};

pub const ScalarSize = enum(u32) {
    s8  = 1,
    s16 = 2,
    s32 = 4,
    s64 = 8,

    pub fn bits(s: ScalarSize) usize {
        return @enumToInt(s) * 8;
    }

    fn names(comptime prefix: []const u8) [4][]const u8 {
        var n: [4][]const u8 = undefined;
        for (std.enums.values(ScalarSize)) |size, i| {
            n[i] = prefix ++ @tagName(size)[1..];
        }
        return n;
    }

};


pub const Vector = struct {
    scalar: Scalar,
    size: VectorSize,

    pub fn Val(comptime v: Vector) type {
        return [@enumToInt(v.size)]v.scalar.Val();
    }

    const names = blk: {
        var n: [Scalar.all.len * 4][]const u8 = undefined;
        for (Scalar.all) |scalar, i| {
            for (std.enums.values(VectorSize)) |vector_size, v| {
                n[i*4 + v] = scalar.name() ++ @tagName(vector_size);
            }
        }
        break :blk n;
    };

    pub fn name(v: Vector) []const u8 {
        return names[v.scalar.indexInAll() * 4 + (@enumToInt(v.size) - 1)];
    }

};

pub const VectorSize = enum(u32) {
    v1 = 1,
    v2 = 2,
    v3 = 3,
    v4 = 4,
};