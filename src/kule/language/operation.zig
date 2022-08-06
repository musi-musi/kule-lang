const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;

const Semantics = language.Semantics;
const Token = language.Token;
const Tag = Token.Tag;

const Module = Semantics.Module;
const Function = Semantics.Function;

const Taip = language.Taip;
const Value = language.Value;
const Data = Value.Data;
const Numeric = Data.Numeric;

const Vd = Value.Data;
const VdSclr = Vd.Scalar;
const VdVctr = Vd.Vector;
const VdMtrx = Vd.Matrix;

const DimCount = Taip.DimCount;


pub fn coerceValue(a: Value, b: Taip) CoerceError!Value {
    if (a.taip.eql(b)) {
        return a;
    }
    try checkCoerceTaip(a.taip, b);
    return try coerceValueAssumeSafe(a, b);
}
pub fn coerceValueAssumeSafe(a: Value, b: Taip) CoerceValueError!Value {
    return try castValueAssumeSafe(a, b);
}

pub const CastTaip = error {
    Incompatable,
    DimensionMismatch,
};

pub fn checkCastTaip(a: Taip, b: Taip) CastTaip!void {
    switch (a) {
        .taip, .module, .function => {
            if (!a.eql(b)) {
                return CastTaip.Incompatable;
            }
        },
        .scalar => switch (b) {
            .scalar, .vector, .matrix => {},
            else => return CastTaip.Incompatable,
        },
        .vector => switch (b) {
            .vector => if (a.vector.dim_count != b.vector.dim_count) {
                return CastTaip.DimensionMismatch;
            },
            else => return CastTaip.Incompatable,
        },
        .matrix => switch (b) {
            .matrix => if (a.matrix.row_count != b.matrix.row_count or a.matrix.col_count != b.matrix.col_count) {
                return CastTaip.DimensionMismatch;
            },
            else => return CastTaip.Incompatable,
        }
    }
}

pub const CoerceTaipError = error {
    ScalarModeMismatch,
    ScalarInformationLoss,
} || CastTaip;

pub fn checkCoerceTaip(a: Taip, b: Taip) CoerceTaipError!void {
    try checkCastTaip(a, b);
    if (a.isNumeric() and b.isNumeric()) {
        const as = a.numericScalar();
        const bs = b.numericScalar();
        
        if (@enumToInt(as) < @enumToInt(bs)) {
            return CoerceTaipError.ScalarInformationLoss;
        }
    }
}

pub const CastValueError = error {
    IntegerOverflow,
    NegativeToUnsigned,
    NanToInteger,
    InfinityToInteger,
};

pub const CastError = CastValueError || CastTaip;

pub const CoerceValueError = error {
} || CastValueError;

pub const CoerceError = CoerceValueError || CoerceTaipError;

pub fn castValue(a: Value, b: Taip) CastError!Value {
    if (a.taip.eql(b)) {
        return a;
    }
    try checkCastTaip(a, b);
    return try castValueAssumeSafe(a, b);
}

pub fn castValueAssumeSafe(a: Value, b: Taip) CastValueError!Value {
    switch (a.taip) {
        .taip, .module, .function => return a,
        .scalar, .vector, .matrix => {
            const row_count = b.numericRowCount();
            const col_count = b.numericColCount();
            const a_numeric = a.data.numeric();
            var result_numeric: Numeric = undefined;
            var r: usize = 0;
            while (r < row_count) : (r += 1) {
                var c: usize = 0;
                while (c < col_count) : (c += 1) {
                    const a_num = (
                        if (a.taip == .scalar) a_numeric.n[0][0]
                        else a_numeric.n[r][c]
                    );
                    result_numeric.n[r][c] = switch (a.taip.numericScalar()) {
                        .float => switch (b.numericScalar()) {
                            .float => try castScalar(.float, .float, a_num),
                            .signed => try castScalar(.float, .signed, a_num),
                            .unsigned => try castScalar(.float, .unsigned, a_num),
                        },
                        .signed => switch (b.numericScalar()) {
                            .float => try castScalar(.signed, .float, a_num),
                            .signed => try castScalar(.signed, .signed, a_num),
                            .unsigned => try castScalar(.signed, .unsigned, a_num),
                        },
                        .unsigned => switch (b.numericScalar()) {
                            .float => try castScalar(.unsigned, .float, a_num),
                            .signed => try castScalar(.unsigned, .signed, a_num),
                            .unsigned => try castScalar(.unsigned, .unsigned, a_num),
                        },
                    };
                }
            }
            return Value.init(b, result_numeric);
        },
    }
}

fn castScalar(comptime a_scalar: Taip.Scalar, comptime b_scalar: Taip.Scalar, a_num: Numeric.N) CastValueError!Numeric.N {
    const Float = comptime Taip.Scalar.float.Type();
    const Signed = comptime Taip.Scalar.signed.Type();
    const Unsigned = comptime Taip.Scalar.unsigned.Type();
    const umax = comptime std.math.maxInt(Unsigned);
    const smax = comptime std.math.maxInt(Signed);
    const smin = comptime std.math.minInt(Signed);

    const umaxf = @intToFloat(comptime_float, umax);
    const smaxf = @intToFloat(comptime_float, smax);
    const sminf = @intToFloat(comptime_float, smin);

    const isNan = std.math.isNan;
    const isPosInf = std.math.isInf;
    const isNegInf = std.math.isNegativeInf;

    const a = Numeric.fromBits(a_scalar.Type(), a_num);

    const b = switch (a_scalar) {
        .float => switch (b_scalar) {
            .float => a,
            .signed => (
                if (isNan(a)) return CastValueError.NanToInteger
                else if (isPosInf(a) or isNegInf(a)) return CastValueError.InfinityToInteger
                else if (a > smaxf or a < sminf) return CastValueError.IntegerOverflow
                else @floatToInt(Signed, a)
            ),
            .unsigned => (
                if (isNan(a)) return CastValueError.NanToInteger
                else if (isPosInf(a) or isNegInf(a)) return CastValueError.InfinityToInteger
                else if (a > umaxf) return CastValueError.IntegerOverflow
                else if (a < 0) return CastValueError.NegativeToUnsigned
                else @floatToInt(Unsigned, a)
            ),
        },
        .signed => switch (b_scalar) {
            .float => @intToFloat(Float, a),
            .signed => a,
            .unsigned => (
                if (a < 0) return CastValueError.NegativeToUnsigned
                else @intCast(Unsigned, a)
            ),
        },
        .unsigned => switch (b_scalar) {
            .float => @intToFloat(Float, a),
            .signed => (
                if (a > smax) return CastValueError.IntegerOverflow
                else @intCast(Signed, a)
            ),
            .unsigned => a,
        }
    };
    return Numeric.toBits(b_scalar.Type(), b);

}

pub const ResultTaipError = error {
    UnsupportedOperation,
};

pub const BinaryOpTaipError = ResultTaipError || CoerceTaipError;


pub fn binaryResultTaip(op: Tag, a: Taip, b: Taip) BinaryOpTaipError!Taip {
    switch (op) {
        .plus, .minus, .aster, .fslash, => {

            const taip = blk: {
                checkCoerceTaip(a, b) catch {
                    try checkCoerceTaip(b, a);
                    break :blk a;
                };
                break :blk b;
            };
            if (!taip.isNumeric()) {
                return BinaryOpTaipError.UnsupportedOperation;
            }
            else {
                return taip;
            }
        },
        else => unreachable,
    }
}

pub const UnaryOpTaipError = error {
    NegateUnsigned,
} || ResultTaipError;

pub fn unaryResultTaip(op: Tag, a: Taip) UnaryOpTaipError!Taip {
    switch (op) {
        .plus, .minus => {
            if (!a.isNumeric()) {
                return UnaryOpTaipError.UnsupportedOperation;
            }
            else if (op == .minus and a.numericScalar() == .unsigned) {
                return UnaryOpTaipError.NegateUnsigned;
            }
            else {
                return a;
            }
        },
        else => unreachable,
    }
}


pub const BinaryOpValueError = error {
    DivideByZero,
} || CoerceError;
pub const BinaryOpError = BinaryOpTaipError || BinaryOpValueError;

pub fn doBinaryOp(op: Tag,  a: Value, b: Value) BinaryOpError!Value {
    const res = try binaryResultTaip(op, a.taip, b.taip);
    return try doBinaryOpAssumeSafe(op, res, a, b);
}

pub fn doBinaryOpAssumeSafe(op: Tag, res: Taip, a: Value, b: Value) BinaryOpValueError!Value {
    switch (op) {
        .plus, .minus, .aster, .fslash => {
            const row_count = res.numericRowCount();
            const col_count = res.numericColCount();
            const a_numeric = (try coerceValue(a, res)).data.numeric();
            const b_numeric = (try coerceValue(b, res)).data.numeric();
            var result_numeric: Numeric = undefined;
            var r: usize = 0;
            while (r < row_count) : (r += 1) {
                var c: usize = 0;
                while (c < col_count) : (c += 1) {
                    switch (res.numericScalar()) {
                        .float => result_numeric.n[r][c] = try doScalarBinaryOp(op, .float, a_numeric.n[r][c], b_numeric.n[r][c]),
                        .signed => result_numeric.n[r][c] = try doScalarBinaryOp(op, .signed, a_numeric.n[r][c], b_numeric.n[r][c]),
                        .unsigned => result_numeric.n[r][c] = try doScalarBinaryOp(op, .unsigned, a_numeric.n[r][c], b_numeric.n[r][c]),
                    }
                }
            }
            return Value.init(res, result_numeric);
        },
        else => unreachable,
    }
}


fn doScalarBinaryOp(op: Tag, comptime scalar: Taip.Scalar, a_num: Numeric.N, b_num: Numeric.N) BinaryOpValueError!Numeric.N{
    const a = Numeric.fromBits(scalar.Type(), a_num);
    const b = Numeric.fromBits(scalar.Type(), b_num);
    if (op == .fslash and b == 0) {
        return BinaryOpValueError.DivideByZero;
    }
    const out_num = (
        if (scalar == .float) switch (op) {
            .plus => a + b,
            .minus => a - b,
            .aster => a * b,
            .fslash => a / b,
            else => unreachable,
        }
        else switch (op) {
            .plus => a +% b,
            .minus => a -% b,
            .aster => a *% b,
            .fslash => @divFloor(a, b),
            else => unreachable,
        }
    );
    return Numeric.toBits(scalar.Type(), out_num);
}

pub const UnaryOpValueError = error {
} || CoerceError;
pub const UnaryOpError = UnaryOpTaipError || UnaryOpValueError;

pub fn doUnaryOp(op: Tag, value: Value) UnaryOpError!Value {
    const res = try unaryResultTaip(op, value);
    return try doUnaryOpAssumeSafe(op, res, value);
}

pub fn doUnaryOpAssumeSafe(op: Tag, res: Taip, value: Value) UnaryOpValueError!Value {
    switch (op) {
        .plus => return value,
        else => {
            const row_count = res.numericRowCount();
            const col_count = res.numericColCount();
            const numeric = (try coerceValue(value, res)).data.numeric();
            var result_numeric: Numeric = undefined;
            var r: usize = 0;
            while (r < row_count) : (r += 1) {
                var c: usize = 0;
                while (c < col_count) : (c += 1) {
                    switch (res.numericScalar()) {
                        .float => result_numeric.n[r][c] = try doScalarUnaryOp(op, .float, numeric.n[r][c]),
                        .signed => result_numeric.n[r][c] = try doScalarUnaryOp(op, .signed, numeric.n[r][c]),
                        else => unreachable,
                    }
                }
            }
            return Value.init(res, result_numeric);
        },
    }
}

fn doScalarUnaryOp(op: Tag, comptime scalar: Taip.Scalar, value_num: Numeric.N) UnaryOpValueError!Numeric.N {
    if (op == .minus) {
        const T = scalar.Type();
        return Numeric.toBits(
            T,
            -Numeric.fromBits(T, value_num),
        );
    }
    else {
        unreachable;
    }
}

pub fn taipValueSupportsEval(a: Taip) bool {
    return a == .function;
}

pub fn taipValueEvalReturnTaip(a: Taip) ?Taip {
    switch (a) {
        .function => return a.function.return_taip,
        else => return null,
    }
}

pub fn taipSupportsStaticMemberAccess(a: Taip) bool {
    switch (a) {
        .vector, .matrix => return true,
        else => return false,
    }
}
pub fn taipSupportsValueMemberAccess(a: Taip) bool {
    switch (a) {
        .module, .vector => return true,
        else => return false,
    }
}

fn eql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn taipStaticMemberValue(a: Taip, member_name: []const u8) ?Value {
    const number = Taip.init(.number);
    switch (a) {
        .vector => {
            if (eql(member_name, "len")) {
                return Value.init(
                    number, 
                    VdSclr{ .unsigned = @enumToInt(a.vector.len),
                });
            }
            else if (eql(member_name, "scalar")) {
                return Value.init(.taip, Taip.init(a.vector.scalar));
            }
        },
        .matrix => {
            if (eql(member_name, "row_count")) {
                return Value.init(
                    number, 
                    VdSclr{ .unsigned = @enumToInt(a.matrix.row_count),
                });
            }
            else if (eql(member_name, "col_count")) {
                return Value.init(
                    number, 
                    VdSclr{ .unsigned = @enumToInt(a.matrix.col_count),
                });
            }
            else if (eql(member_name, "scalar")) {
                return Value.init(.taip, Taip.init(a.matrix.scalar));
            }
        },
        else => {},
    }
    return null;
}

pub fn taipValueMemberTaip(a: Taip, member_name: []const u8) ?Taip {
    switch (a) {
        .vector => {
            if (member_name.len > 1) {
                return null;
            }
            const c = member_name[0];
            if(switch (a.vector.len) {
                .d2 => switch (c) {
                    'x', 'y', => true,
                    else => false
                },
                .d3 => switch (c) {
                    'x', 'y', 'z', => true,
                    else => false
                },
                .d4 => switch (c) {
                    'x', 'y', 'z', 'w', => true,
                    else => false
                },
            }) {
                return a.vector.scalar;
            }
            else {
                return null;
            }
        },
        else => return null,
    }
}