const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;

const meta = std.meta;

const Semantics = language.Semantics;

const Module = Semantics.Module;
const Function = Semantics.Function;

pub const taips = struct {
    pub const taip: Taip = .taip;
    pub const module: Taip = .module;
    pub fn function(allocator: Allocator, param_taips: []Taip, return_taip: Taip) Allocator.Error!Taip {
        const func = try allocator.create(Taip.Func);
        func.* = Taip.Func {
            .param_taips = param_taips,
            .return_taip = return_taip,
        };
        return Taip {
            .function = func,
        };
    }
};

pub const Taip = union(enum) {
    
    taip: void,
    
    module: void,
    
    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    function: *Func,

    pub const Tag = meta.Tag(Taip);

    pub const Scalar = ScalarTaip;
    pub const Vector = VectorTaip;
    pub const Matrix = MatrixTaip;
    pub const Func = FuncTaip;
    pub const DimCount = TaipDimCount;

    pub fn init(value: anytype) Taip {
        const V = @TypeOf(value);
        switch (V) {
            Scalar => return Taip{ .scalar = value },
            Vector => return Taip{ .vector = value },
            Matrix => return Taip{ .matrix = value },
            *Func => return Taip{ .function = value },
            else => {
                if (comptime meta.trait.is(.EnumLiteral)(V)) {
                    switch (value) {
                        .taip => return .taip,
                        .module => return .module,
                        .float => return init(Scalar.float),
                        .signed => return init(Scalar.signed),
                        .unsigned => return init(Scalar.unsigned),
                        else => @compileError(@tagName(value) ++ " is not an accepted Taip tag"),
                    }
                }
                else {
                    @compileError(@typeName(value) ++ " is not a valid Taip value");
                }
            }
        }
    }

    pub fn TypeOf(comptime value: anytype) type {
        return init(value).Type();
    }

    pub fn Type(comptime self: Taip) type {
        switch (self) {
            .taip => return Taip,
            .module => return *Module,
            .function => return *Semantics.Function,
            else => {},
        }
        const field = @field(self, @tagName(self));
        return field.Type();
    }

    pub fn taipOfFunction(allocator: Allocator, function: *Function) !?Taip {
        const params = function.params;
        const param_taips = try allocator.alloc(Taip, params.len);
        for (params) |param, i| {
            if (param.taip) |param_taip| {
                param_taips[i] = param_taip;
            }
            else {
                allocator.free(param_taips);
                return null;
            }
        }
        if (function.return_taip) |return_taip| {
            return try taips.function(allocator, param_taips, return_taip);
        }
        else {
            allocator.free(param_taips);
            return null;
        }
    }

    pub fn eql(a: Taip, b: Taip) bool {
        return meta.activeTag(a) == meta.activeTag(b) and switch(a) {
            .scalar => (
                a.scalar == b.scalar
            ),
            .vector => (
                a.vector.scalar == b.vector.scalar
                and a.vector.dim_count == b.vector.dim_count
            ),
            .matrix => (
                a.matrix.scalar == b.matrix.scalar
                and a.matrix.row_count == b.matrix.row_count
                and a.matrix.col_count == b.matrix.col_count
            ),
            .function => (
                a.function.return_taip.eql(b.function.return_taip)
                and for (a.function.param_taips) |param_taip, i| {
                    if (!param_taip.eql(b.function.param_taips[i])) {
                        break false;
                    }
                }
                else true
            ),
            else => true,
        };
    }

    fn panicNonNumeric() noreturn {
        @panic("type is not numeric");
    }

    pub fn isNumeric(self: Taip) bool {
        return switch (self) {
            .scalar,
            .vector,
            .matrix, => true,
            else => false,
        };
    }

    pub fn numericRowCount(self: Taip) u32 {
        return switch (self) {
            .scalar, .vector => 1,
            .matrix => self.matrix.rowCount(),
            else => panicNonNumeric(),
        };
    }

    pub fn numericColCount(self: Taip) u32 {
        return switch (self) {
            .scalar => 1,
            .vector => self.vector.dimCount(),
            .matrix => self.matrix.colCount(),
            else => panicNonNumeric(),
        };
    }


    pub fn numericScalar(self: Taip) Scalar {
        return switch (self) {
            .scalar => self.scalar,
            .vector => self.vector.scalar,
            .matrix => self.matrix.scalar,
            else => panicNonNumeric(),
        };
    }

    pub fn format(self: Taip, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .taip => try writer.writeAll("type"),
            .module => try writer.writeAll("module"),
            .scalar => |scalar| try scalar.format(writer),
            .vector => |vector| try vector.format(writer),
            .matrix => |matrix| try matrix.format(writer),
            .function => |function| try function.format(writer),
        }
    }




};

const FuncTaip = struct {
    param_taips: []Taip,
    return_taip: Taip,

    pub fn init(param_taips: []Taip, return_taip: Taip) FuncTaip {
        return .{
            .param_taips = param_taips,
            .return_taip = return_taip,
        };
    }

    fn format(f: FuncTaip, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll("fn(");
        for (f.param_taips) |taip, i| {
            if (i > 0) {
                try writer.writeAll(", ");
            }
            try taip.format("", .{}, writer);
        }
        try writer.writeAll("): ");
        try f.return_taip.format("", .{}, writer);
    }
};

const ScalarTaip = enum(u32) {
    float = 0,
    signed = 1,
    unsigned = 2,

    pub const Mode = enum {
        static,
        dynamic,
    };

    pub fn vector(self: ScalarTaip, dim_count: TaipDimCount) VectorTaip {
        return VectorTaip{
            .scalar = self,
            .dim_count = dim_count,
        };
    }

    pub fn matrix(self: ScalarTaip, row_count: TaipDimCount, col_count: TaipDimCount) MatrixTaip {
        return MatrixTaip{
            .scalar = self,
            .row_count = row_count,
            .col_count = col_count,
        };
    }

    pub fn taip(self: ScalarTaip) Taip {
        return Taip {
            .scalar = self,
        };
    }

    fn format(self: ScalarTaip, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll(@tagName(self));
    }

    pub fn Type(comptime s: ScalarTaip) type {
        return switch (s) {
            .float => f32,
            .signed => i32,
            .unsigned => u32,
        };
    }


};

const VectorTaip = struct {
    scalar: ScalarTaip,
    dim_count: TaipDimCount,

    pub fn dimCount(self: VectorTaip) u32 {
        return @enumToInt(self.dim_count);
    }

    pub fn taip(self: VectorTaip) Taip {
        return Taip {
            .vector = self,
        };
    }

    pub fn Type(comptime v: VectorTaip) type {
        return [@enumToInt(v.len)]v.scalar.Type();
    }
    
    fn format(self: VectorTaip, writer: anytype) @TypeOf(writer).Error!void {
        try self.scalar.format(writer);
        try writer.writeByte(@tagName(self.dim_count)[1]);
    }



};

const MatrixTaip = struct {
    scalar: ScalarTaip,
    row_count: TaipDimCount,
    col_count: TaipDimCount,

    pub fn rowCount(self: MatrixTaip) u32 {
        return @enumToInt(self.row_count);
    }

    pub fn colCount(self: MatrixTaip) u32 {
        return @enumToInt(self.col_count);
    }

    pub fn taip(self: MatrixTaip) Taip {
        return Taip {
            .matrix = self,
        };
    }

    pub fn Type(comptime v: VectorTaip) type {
        return [@enumToInt(v.row_count)][@enumToInt(v.col_count)]v.scalar.Type();
    }
    
    fn format(self: MatrixTaip, writer: anytype) @TypeOf(writer).Error!void {
        try self.scalar.format(writer);
        try writer.writeByte(@tagName(self.row_count)[1]);
        try writer.writeByte('x');
        try writer.writeByte(@tagName(self.col_count)[1]);
    }

};

const TaipDimCount = enum(u32) {
    d2 = 2,
    d3 = 3,
    d4 = 4,
};