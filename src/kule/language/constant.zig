const std = @import("std");
const language = @import("../language.zig");

const KType = language.KType;
const Value = language.Value;

pub const Constant = struct {
    name: []const u8,
    ktype: KType,
    value: Value,
};

pub fn lookupConstant(name: []const u8) ?*const Constant {
    return constant_map.get(name);
}

pub const constants = blk: {
    @setEvalBranchQuota(10_000);
    var list: []const Constant = &.{};
    list = list ++ builtin_ktypes;
    break :blk list;
};

const constant_map = blk: {
    const Pair = std.meta.Tuple(&.{[]const u8, *const Constant});
    @setEvalBranchQuota(10_000);
    var pairs: [constants.len]Pair = undefined;
    for (constants) |*constant, i| {
        pairs[i] = Pair{ constant.name, constant};
    }
    break :blk std.ComptimeStringMap(*const Constant, pairs);
};

const builtin_ktypes = blk: {
    var ktypes: []const Constant = &.{
        ktypeConst(.ktype),
        ktypeConst(.module),
    };
    for (std.enums.values(KType.Scalar)) |scalar| {
        ktypes = ktypes ++ &[_]Constant{ktypeConst(KType.init(scalar))};
        for (std.enums.values(KType.DimCount)) |row_count| {
            ktypes = ktypes ++ &[_]Constant{ktypeConst(KType.init(scalar.vector(row_count)))};
            for (std.enums.values(KType.DimCount)) |col_count| {
                ktypes = ktypes ++ &[_]Constant{ktypeConst(KType.init(scalar.matrix(row_count, col_count)))};
            }
        }
    }
    break :blk ktypes;
};

fn ktypeConst(comptime ktype: KType) Constant {
    return Constant {
        .name = std.fmt.comptimePrint("{}", .{ktype}),
        .ktype = .ktype,
        .value = Value.init(.ktype, ktype),
    };
}