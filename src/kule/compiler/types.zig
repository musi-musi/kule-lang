const std = @import("std");
const expression = @import("expression.zig");
const lexer = @import("lexer.zig");

const Allocator = std.mem.Allocator;

const meta = std.meta;

const Token = lexer.Token;

const Module = expression.Module;
const Param = expression.Param;
const ExprNode = expression.ExprNode;
const Expr = expression.Expr;
const Function = expression.Function;

const Operator = expression.Operator;

pub fn numberType(encoding: NumberType.Encoding, dimensions: NumberType.Dimensions) Type {
    return Type.init(NumberType {
        .encoding = encoding,
        .dimensions = dimensions,
        .is_dynamic = false,
    });
}

pub fn dynamicNumberType(encoding: NumberType.Encoding, dimensions: NumberType.Dimensions) Type {
    return Type.init(NumberType {
        .encoding = encoding,
        .dimensions = dimensions,
        .is_dynamic = true,
    });
}

fn NamePair(comptime T: type) type {
    return std.meta.Tuple(&.{[]const u8, T});
}

pub const language_types = blk: {
    const Pair = NamePair(Value);
    var pairs: []const Pair = &[_]Pair {
        .{"type", Value.typeValue(Type.type_value)},
        .{"module", Value.typeValue(Type.module)},
    };
    for (NumberType.Dimensions.all) |d| {
        const dynamic = NumberType{
            .encoding = .float,
            .dimensions = d,
            .is_dynamic = true,
        };
        pairs = pairs ++ &[_]Pair{
            .{dynamic.name(), Value.typeValue(Type.init(dynamic))}
        };
        for (NumberType.Encoding.all) |f| {
            const n = NumberType {
                .encoding = f,
                .dimensions = d,
                .is_dynamic = false,
            };
            pairs = pairs ++ &[_]Pair{
                .{n.name(), Value.typeValue(Type.init(n))}
            };
        }
    }
    break :blk pairs;
};

pub const Value = struct {
    data: Data,// align(data_align),
    value_type: Type,

    pub const Data = union(enum) {
        data: DataBytes,
        type_value: Type,
        module: *Module,
        function: *Function,
    };


    pub const data_align = @alignOf(NumberType.initMatrix(.float, 4, 4, false).ZigType());
    pub const DataBytes = [
        8 * // largest possible number width
        4 * // largest possible matrix row count/vector len
        4   // largest possible matrix column
    ]u8;

    pub fn init(data: anytype, value_type: Type) Value {
        switch(@TypeOf(data)) {
            Type => return Value {
                .data = .{
                    .type_value = data,
                },
                .value_type = Type.type_value,
            },
            *Module => return Value {
                .data = .{
                    .module = data,
                },
                .value_type = Type.module,
            },
            *Function => return Value {
                .data = .{
                    .function = data,
                },
                .value_type = value_type,
            },
            else => {
                const bytes = dataToBytes(data);
                return Value {
                    .data = .{
                        .data = bytes
                    },
                    .value_type = value_type,
                };
            },
        }
    }

    pub fn dataToBytes(data: anytype) DataBytes {
        const T = @TypeOf(data);
        const size = @sizeOf(T);
        if (size > @sizeOf(DataBytes)) {
            @compileError(@typeName(T) ++ " is too large to store in a Value. consider using a pointer");
        }
        const slice = @ptrCast([*]const u8, &data)[0..size];
        var result: DataBytes = undefined;
        std.mem.copy(u8, result[0..size], slice);
        return result;
    }

    pub fn typeValue(t: Type) Value {
        return init(t, Type.type_value);
    }

    pub fn initModule(m: *Module) Value {
        return init(m, Type.module);
    }

    pub fn initNumber(n: anytype, num: NumberType) Value {
        return init(n, Type.number(num));
    }

    pub fn initDynamicNumber(encoding: NumberEncoding, dimensions: NumberDimensions, value: NumberType.init(encoding, dimensions, true).ZigType()) Value {
        return init(value, Type.init(NumberType.init(encoding, dimensions, true)));
    }

    pub fn toZigType(value: Value, comptime value_type: Type) value_type.ZigType() {
        switch (value_type) {
            .type_value => {
                return value.data.type_value;
            },
            .module => {
                return value.data.module;
            },
            .function => {
                return value.data.function;
            },
            else => {
                const T = value_type.ZigType();
                const size = @sizeOf(T);
                var val: T = undefined;
                const slice = @ptrCast([*]u8, &val)[0..@sizeOf(T)];
                std.mem.copy(u8, slice, value.data.data[0..size]);
                return val;
            },
        }
    }

    pub fn UnaryFn(comptime Ctx: type, comptime Ret: type) type {
        return fn (Ctx, Value) Ret;
    }

    pub fn BinaryFn(comptime Ctx: type, comptime Ret: type) type {
        return fn (Ctx, Value, Value) Ret;
    }

    pub fn UnaryFnGen(comptime Ctx: type, comptime Ret: type) type {
        return fn(comptime Type) UnaryFn(Ctx, Ret);
    }

    pub fn BinaryFnGen(comptime Ctx: type, comptime Ret: type) type {
        return fn(comptime Type, comptime Type) BinaryFn(Ctx, Ret);
    }

    pub fn UnaryFnTable(comptime Ctx: type, comptime Ret: type) type {
        return [Type.all.len]UnaryFn(Ctx, Ret);
    }
    
    pub fn BinaryFnTable(comptime Ctx: type, comptime Ret: type) type {
        return [Type.all.len][Type.all.len]BinaryFn(Ctx, Ret);
    }

    pub fn unaryFnTable(comptime Ctx: type, comptime Ret: type, comptime generator: anytype) UnaryFnTable(Ctx, Ret) {
        @setEvalBranchQuota(100_000);
        var table: UnaryFnTable(Ctx, Ret) = undefined;
        for (Type.all) |t, i| {
            table[i] = generator(t);
        }
        return table;
    }

    pub fn binaryFnTable(comptime Ctx: type, comptime Ret: type, comptime generator: anytype) BinaryFnTable(Ctx, Ret) {
        @setEvalBranchQuota(100_000);
        var table: BinaryFnTable(Ctx, Ret) = undefined;
        for (Type.all) |at, i| {
            for (Type.all) |bt, j| {
                table[i][j] = generator(at, bt);
            }
        }
        return table;
    }

    


    pub fn format(v: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const format_fn = format_fn_table[v.value_type.index()];
        var buffer: [512]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buffer);
        format_fn(&fbs, v);
        try writer.writeAll(fbs.buffer);
    }

    const FBS = std.io.FixedBufferStream([]u8);

    const format_fn_table = unaryFnTable(*FBS, void, struct {
        fn gen(comptime t: Type) UnaryFn(*FBS, void) {
            return struct {

                const T = t.ZigType();

                fn fmt(fbs: *FBS, v: Value) !void {
                    const writer = fbs.writer();
                    if (comptime t.tag().isFinite()) {
                        const val: T = v.toZigType(t);
                        switch (t) {
                            .type_value => try val.format("", .{}, writer),
                            .module => try val.format("", .{}, writer),
                            .function => {},
                            .number => |num| {
                                switch (num.kind()) {
                                    .scalar => try writer.print("{d}", .{val}),
                                    .vector => {
                                        try writer.writeByte('[');
                                        for (val) |n, i| {
                                            if (i > 0) {
                                                try writer.writeAll(", ");
                                            }
                                            try writer.print("{d}", .{n});
                                        }
                                        try writer.writeByte(']');
                                    },
                                    .matrix => {
                                        try writer.writeByte('[');
                                        for (val) |row| {
                                            try writer.writeByte('[');
                                            for (row) |n, i| {
                                                if (i > 0) {
                                                    try writer.writeAll(", ");
                                                }
                                                try writer.print("{d}", .{n});
                                            }
                                            try writer.writeByte(']');
                                        }
                                        try writer.writeByte(']');
                                    }
                                }
                            },
                        }
                    }
                    else {
                        try v.value_type.format("", .{}, writer);
                    }
                }

                fn f(fbs: *FBS, v: Value) void {
                    fmt(fbs, v) catch {
                        const buffer = fbs.buffer;
                        std.mem.copy(u8, buffer[buffer.len-3..], "...");
                    };
                }

            }.f;
        }
    }.gen);

};

pub const TypeTag = enum(u8) {
    type_value = 0,
    module = 1,
    function = 2,
    number = 3,

    /// return true if there is a finate number of types that can exist with this tag
    /// return false if there is an infinite number
    pub fn isFinite(tag: TypeTag) bool {
        return switch(tag) {
            .function => false,
            else => true,
        };
    }

};

pub const Type = union(TypeTag) {
    type_value: void,
    module: void,
    function: *FunctionType,
    number: NumberType,

    pub fn init(t: anytype) Type {
        const T = @TypeOf(t);
        if (@typeInfo(T) == .EnumLiteral) {
            return switch (t) {
                .type_value => type_value,
                .module => module,
                .number => init(NumberType.scalar(.float, true)),
                .float => init(NumberType.scalar(.float, false)),
                .signed => init(NumberType.scalar(.signed, false)),
                .unsigned => init(NumberType.scalar(.unsigned, false)),
                else => @compileError(@tagName(t) ++ " is not a Type"),
            };
        }
        else switch (T) {
            NumberType => return Type {
                .number = t,
            },
            *FunctionType => return Type {
                .function = t,
            },
            else => @compileError(@typeName(T) ++ " is not a Type")
        }
    }

    pub fn eql(a: Type, b: Type) bool {
        if (a.tag() != b.tag()) {
            return false;
        }
        else {
            switch (a.tag()) {
                .function => {
                    const af = a.function;
                    const bf = b.function;
                    if (af.param_types.len != bf.param_types.len) {
                        return false;
                    }
                    for (af.param_types) |param_type, i| {
                        if (!param_type.eql(bf.param_types[i])) {
                            return false;
                        }
                    }
                    return af.return_type.eql(bf.return_type);
                },
                .number => {
                    return a.number.eql(b.number);
                },
                else => return true,
            }
        }
    }

    pub fn hasMemberAccess(t: Type) bool {
        switch (t) {
            .type_value => return false,
            .module => return true,
            .function => return false,
            .number => return true,
        }
    }

    pub const type_value = Type{ .type_value = {} };
    pub const module = Type{ .module = {} };
    
    pub const all = blk: {
        var list: [3 + NumberType.all.len]Type = undefined;
        list[0] = type_value;
        list[1] = module;
        list[2] = init(@as(*FunctionType, undefined));
        for (NumberType.all) |n, i| {
            list[i + 2] = init(n);
        }
        break :blk list;
    };

    pub fn ZigType(comptime t: Type) type {
        switch (t) {
            .type_value => return Type,
            .module => return *Module,
            .function => *Function,
            .number => |n| return n.ZigType(),
        }
    }

    pub fn tag(t: Type) TypeTag {
        return meta.activeTag(t);
    }

    pub fn index(t: Type) usize {
        return switch (t) {
            .type_value => 0,
            .module => 1,
            .function => 2,
            .number => |n| 3 + n.index(),
        };
    }

    pub fn format(t: Type, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (t) {
            .type_value => try writer.writeAll("type"),
            .module => try writer.writeAll("module"),
            .function => |f| {
                try writer.writeAll("fn(");
                for (f.param_types) |param_type, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try param_type.format(fmt, options, writer);
                }
                try writer.writeAll(")");
                try f.return_type.format(fmt, options, writer);
            },
            .number => |n| try n.format(fmt, options, writer),
        }
    }


};

pub const FunctionType = struct {
    param_types: []Type,
    return_type: Type,

    pub const Error = error {
        UnknownParamType,
        UnknownReturnType,
    } || Allocator.Error;

    pub fn init(allocator: Allocator, function: Function) Error!*FunctionType {
        const param_types = try allocator.alloc(Type, function.params.len);
        errdefer allocator.free(param_types);
        for (function.params) |param, i| {
            if (param.type_expr.expr.typeValue()) |type_value| {
                param_types[i] = type_value;
            }
            else {
                return Error.UnknownParamType;
            }
        }
        const return_type = function.expr.typeValue() orelse return Error.UnknownReturnType;
        const self = try allocator.create(FunctionType);
        self.* = FunctionType {
            .param_types = param_types,
            .return_type = return_type,
        };
        return self;
    }

};

pub const NumberType = struct {
    encoding: Encoding,
    dimensions: Dimensions,
    is_dynamic: bool = false,

    pub const Encoding = NumberEncoding;
    pub const Kind = NumberKind;
    pub const Dimensions = NumberDimensions;

    pub const all = blk: {
        const len = Encoding.all.len * Dimensions.all.len * 2;
        var list: [len]NumberType = undefined;
        var i: usize = 0;
        for ([_]bool{true, false}) |d| {
            for (Encoding.all) |f| {
                for (Dimensions.all) |s| {
                    list[i] = NumberType {
                        .encoding = f,
                        .dimensions = s,
                        .is_dynamic = d,
                    };
                    i += 1;
                }
            }
        }
        break :blk list;
    };

    pub const all_named = blk: {
        var list: [all.len]NamePair(NumberType) = undefined;
        for (all) |n, i| {
            list[i] = .{(
                if (n.is_dynamic) "number"
                else @tagName(n.encoding)
            ) ++ (
                if (n.dimensions == .scalar) ""
                else @tagName(n.dimensions)[3..]
            ), n};
        }
        break :blk list;
    };

    pub fn name(n: NumberType) []const u8 {
        return all_named[n.index()].@"0";
    }

    pub fn init(encoding: Encoding, dimensions: Dimensions, is_dynamic: bool) NumberType {
        return .{
            .encoding = encoding,
            .dimensions = dimensions,
            .is_dynamic = is_dynamic,
        };
    }

    pub fn initScalar(encoding: Encoding, is_dynamic: bool) NumberType {
        return init(encoding, .scalar, is_dynamic);
    }

    pub fn initVector(encoding: Encoding, len: u4, is_dynamic: bool) NumberType {
        return init(encoding, Dimensions.vec(len), is_dynamic);
    }

    pub fn initMatrix(encoding: Encoding, columns: u4, rows: u4, is_dynamic: bool) NumberType {
        return init(encoding, Dimensions.mat(columns, rows), is_dynamic);
    }

    pub fn kind(n: NumberType) NumberKind {
        return n.dimensions.kind();
    }
    pub fn size(n: NumberType) usize {
        return n.encoding.size() * n.dimensions.size();
    }

    pub fn eql(a: NumberType, b: NumberType) bool {
        return a.encoding == b.encoding and a.dimensions == b.dimensions;
    }

    pub fn ZigType(comptime n: NumberType) type {
        const Unit = n.encoding.ZigType();
        return switch (n.kind()) {
            .scalar => Unit,
            .vector => [n.dimensions.len()]Unit,
            .matrix => [n.dimensions.columns()][n.dimensions.rows()]Unit,
        };
    }

    pub fn index(n: NumberType) usize {
        const len = Dimensions.all.len * Encoding.all.len;
        const i = n.encoding.index() * Dimensions.all.len + n.dimensions.index();
        if (n.is_dynamic) {
            return i;
        }
        else {
            return i + len;
        }
    }

    pub fn format(n: NumberType, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll(n.name());
    }


};

pub const NumberEncoding = enum(u8) {
    float = 0x0_4,
    signed = 0x1_4,
    unsigned = 0x2_4,

    pub const all = [_]NumberEncoding{ .float, .signed, .unsigned };

    pub fn index(f: NumberEncoding) usize {
        return @enumToInt(f) >> 4;
    }

    pub fn size(n: NumberEncoding) usize {
        return @enumToInt(n) & 0xF;
    }

    pub fn ZigType(comptime f: NumberEncoding) type {
        return switch(f) {
            .float => f32,
            .signed => i32,
            .unsigned => u32,
        };
    }
};

pub const NumberKind =  enum(u2) {
    scalar = 0,
    vector = 1,
    matrix = 2,
};

pub const NumberDimensions = enum(u12) {
    scalar = 0x0_11, 
    
    vec1 = 0x1_11,
    vec2 = 0x1_12,
    vec3 = 0x1_13,
    vec4 = 0x1_14,

    mat1x1 = 0x2_11,
    mat2x1 = 0x2_21,
    mat3x1 = 0x2_31,
    mat4x1 = 0x2_41,
    
    mat1x2 = 0x2_12,
    mat2x2 = 0x2_22,
    mat3x2 = 0x2_32,
    mat4x2 = 0x2_42,
    
    mat1x3 = 0x2_13,
    mat2x3 = 0x2_23,
    mat3x3 = 0x2_33,
    mat4x3 = 0x2_43,

    mat1x4 = 0x2_14,
    mat2x4 = 0x2_24,
    mat3x4 = 0x2_34,
    mat4x4 = 0x2_44,

    pub const all = blk: {
        const D = NumberDimensions;
        var list: []const D = &[_]D{ .scalar };
        for ([_]u12{1, 2, 3, 4}) |r| {
            list = list ++ &[_]D{ @intToEnum(D, 0x110 | r)};
            for ([_]u12{1, 2, 3, 4}) |c| {
                list = list ++ &[_]D{ @intToEnum(D, 0x200 | r | c << 4)};
            }
        }
        break :blk list;
    };

    pub fn index(d: NumberDimensions) usize {
        return switch (d.kind()) {
            .scalar => 0,
            .vector => 1 + (d.len() - 1),
            .matrix => 5 + (d.columns() - 1) + (d.rows() - 1) * 4,
        };
    }

    pub fn vec(l: u12) NumberDimensions {
        return @intToEnum(NumberDimensions, 0x1_10 | l);
    }

    pub fn mat(c: u12, r: u12) NumberDimensions {
        return @intToEnum(NumberDimensions, 0x2_00 | c << 4 | r);
    }

    pub fn kind(d: NumberDimensions) NumberKind {
        return @intToEnum(NumberKind, @truncate(u2, @enumToInt(d) >> 8));
    }

    pub fn dimensionCode(d: NumberDimensions) u8 {
        return @truncate(u8, @enumToInt(d));
    }

    pub const len = rows;

    pub fn rows(d: NumberDimensions) usize {
        return (@enumToInt(d) >> 0) & 0xF;
    }

    pub fn columns(d: NumberDimensions) usize {
        return (@enumToInt(d) >> 4) & 0xF;
    }

    pub fn size(d: NumberDimensions) usize {
        return d.rows() * d.columns();
    }

};