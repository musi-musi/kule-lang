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
    const scalars = [_]Taip.Scalar {
        Taip.Scalar { .taip = .float,       .mode = .dynamic },
        Taip.Scalar { .taip = .float,       .mode = .static },
        Taip.Scalar { .taip = .signed,      .mode = .static },
        Taip.Scalar { .taip = .unsigned,    .mode = .static },
    };
    for (scalars) |scalar| {
        taips = taips ++ &[_]Constant{taipConst(Taip.init(scalar))};
        for (std.enums.values(Taip.Dims)) |rows| {
            taips = taips ++ &[_]Constant{taipConst(Taip.init(scalar.vector(rows)))};
            for (std.enums.values(Taip.Dims)) |cols| {
                taips = taips ++ &[_]Constant{taipConst(Taip.init(scalar.matrix(rows, cols)))};
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