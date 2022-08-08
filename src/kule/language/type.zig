const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;

const meta = std.meta;

const Semantics = language.Semantics;

const Module = Semantics.Module;


pub const KType = union(enum) {
    
    ktype: void,
    
    module: void,
    
    scalar: Scalar,
    vector: Vector,
    matrix: Matrix,

    function: *Function,

    pub const Tag = meta.Tag(KType);

    pub const Scalar = ScalarKType;
    pub const Vector = VectorKType;
    pub const Matrix = MatrixKType;
    pub const Function = FuncKType;
    pub const DimCount = KTypeDimCount;

    pub fn init(value: anytype) KType {
        const V = @TypeOf(value);
        switch (V) {
            KType => return value,
            Scalar => return KType{ .scalar = value },
            Vector => return KType{ .vector = value },
            Matrix => return KType{ .matrix = value },
            *Function => return KType{ .function = value },
            else => {
                if (comptime meta.trait.is(.EnumLiteral)(V)) {
                    switch (value) {
                        .ktype => return .ktype,
                        .module => return .module,
                        .float => return init(Scalar.float),
                        .signed => return init(Scalar.signed),
                        .unsigned => return init(Scalar.unsigned),
                        else => @compileError(@tagName(value) ++ " is not an accepted KType tag"),
                    }
                }
                else {
                    @compileError(@typeName(V) ++ " is not a valid KType value");
                }
            }
        }
    }

    pub fn TypeOf(comptime value: anytype) type {
        return init(value).Type();
    }

    pub fn Type(comptime self: KType) type {
        switch (self) {
            .ktype => return KType,
            .module => return *Module,
            .function => return *Semantics.Function,
            else => {},
        }
        const field = @field(self, @tagName(self));
        return field.Type();
    }

    pub fn eql(a: KType, b: KType) bool {
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
                a.function.return_ktype.eql(b.function.return_ktype)
                and for (a.function.param_ktypes) |param_ktype, i| {
                    if (!param_ktype.eql(b.function.param_ktypes[i])) {
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

    pub fn isNumeric(self: KType) bool {
        return switch (self) {
            .scalar,
            .vector,
            .matrix, => true,
            else => false,
        };
    }

    pub fn numericRowCount(self: KType) u32 {
        return switch (self) {
            .scalar, .vector => 1,
            .matrix => self.matrix.rowCount(),
            else => panicNonNumeric(),
        };
    }

    pub fn numericColCount(self: KType) u32 {
        return switch (self) {
            .scalar => 1,
            .vector => self.vector.dimCount(),
            .matrix => self.matrix.colCount(),
            else => panicNonNumeric(),
        };
    }


    pub fn numericScalar(self: KType) Scalar {
        return switch (self) {
            .scalar => self.scalar,
            .vector => self.vector.scalar,
            .matrix => self.matrix.scalar,
            else => panicNonNumeric(),
        };
    }

    pub fn format(self: KType, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .ktype => try writer.writeAll("type"),
            .module => try writer.writeAll("module"),
            .scalar => |scalar| try scalar.format(writer),
            .vector => |vector| try vector.format(writer),
            .matrix => |matrix| try matrix.format(writer),
            .function => |function| try function.format(writer, fmt),
        }
    }




};

const FuncKType = struct {
    param_ktypes: []KType,
    return_ktype: KType,

    pub fn init(param_ktypes: []KType, return_ktype: KType) FuncKType {
        return .{
            .param_ktypes = param_ktypes,
            .return_ktype = return_ktype,
        };
    }

    fn format(f: FuncKType, writer: anytype, comptime fmt: []const u8 ) @TypeOf(writer).Error!void {
        if (fmt.len == 0 or fmt[0] != '~') {
            try writer.writeAll("fn");
        }
        try writer.writeAll("(");
        for (f.param_ktypes) |ktype, i| {
            if (i > 0) {
                try writer.writeAll(", ");
            }
            try ktype.format("", .{}, writer);
        }
        try writer.writeAll("): ");
        try f.return_ktype.format("", .{}, writer);
    }
};

const ScalarKType = enum(u32) {
    float = 0,
    signed = 1,
    unsigned = 2,

    pub const Mode = enum {
        static,
        dynamic,
    };

    pub fn vector(self: ScalarKType, dim_count: KTypeDimCount) VectorKType {
        return VectorKType{
            .scalar = self,
            .dim_count = dim_count,
        };
    }

    pub fn matrix(self: ScalarKType, row_count: KTypeDimCount, col_count: KTypeDimCount) MatrixKType {
        return MatrixKType{
            .scalar = self,
            .row_count = row_count,
            .col_count = col_count,
        };
    }

    pub fn ktype(self: ScalarKType) KType {
        return KType {
            .scalar = self,
        };
    }

    fn format(self: ScalarKType, writer: anytype) @TypeOf(writer).Error!void {
        try writer.writeAll(@tagName(self));
    }

    pub fn Type(comptime s: ScalarKType) type {
        return switch (s) {
            .float => f32,
            .signed => i32,
            .unsigned => u32,
        };
    }


};

const VectorKType = struct {
    scalar: ScalarKType,
    dim_count: KTypeDimCount,

    pub fn dimCount(self: VectorKType) u32 {
        return @enumToInt(self.dim_count);
    }

    pub fn ktype(self: VectorKType) KType {
        return KType {
            .vector = self,
        };
    }

    pub fn Type(comptime v: VectorKType) type {
        return [@enumToInt(v.len)]v.scalar.Type();
    }
    
    fn format(self: VectorKType, writer: anytype) @TypeOf(writer).Error!void {
        try self.scalar.format(writer);
        try writer.writeByte(@tagName(self.dim_count)[1]);
    }



};

const MatrixKType = struct {
    scalar: ScalarKType,
    row_count: KTypeDimCount,
    col_count: KTypeDimCount,

    pub fn rowCount(self: MatrixKType) u32 {
        return @enumToInt(self.row_count);
    }

    pub fn colCount(self: MatrixKType) u32 {
        return @enumToInt(self.col_count);
    }

    pub fn ktype(self: MatrixKType) KType {
        return KType {
            .matrix = self,
        };
    }

    pub fn Type(comptime v: VectorKType) type {
        return [@enumToInt(v.row_count)][@enumToInt(v.col_count)]v.scalar.Type();
    }
    
    fn format(self: MatrixKType, writer: anytype) @TypeOf(writer).Error!void {
        try self.scalar.format(writer);
        try writer.writeByte(@tagName(self.row_count)[1]);
        try writer.writeByte('x');
        try writer.writeByte(@tagName(self.col_count)[1]);
    }

};

const KTypeDimCount = enum(u32) {
    d2 = 2,
    d3 = 3,
    d4 = 4,
};