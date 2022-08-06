const std = @import("std");
const language = @import("../language.zig");

const Taip = language.Taip;
const Value = language.Value;

pub const Constant = struct {
    name: []const u8,
    taip: Taip,
    value: Value,
};

pub fn lookupConstant(name: []const u8) ?*const Constant {
    return constant_map.get(name);
}

pub const constants = blk: {
    @setEvalBranchQuota(10_000);
    var list: []const Constant = &.{};
    list = list ++ builtin_taips;
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

const builtin_taips = blk: {
    var taips: []const Constant = &.{
        taipConst(.taip),
        taipConst(.module),
    };
    for (std.enums.values(Taip.Scalar)) |scalar| {
        taips = taips ++ &[_]Constant{taipConst(Taip.init(scalar))};
        for (std.enums.values(Taip.DimCount)) |row_count| {
            taips = taips ++ &[_]Constant{taipConst(Taip.init(scalar.vector(row_count)))};
            for (std.enums.values(Taip.DimCount)) |col_count| {
                taips = taips ++ &[_]Constant{taipConst(Taip.init(scalar.matrix(row_count, col_count)))};
            }
        }
    }
    break :blk taips;
};

fn taipConst(comptime taip: Taip) Constant {
    return Constant {
        .name = std.fmt.comptimePrint("{}", .{taip}),
        .taip = .taip,
        .value = Value.init(.taip, taip),
    };
}