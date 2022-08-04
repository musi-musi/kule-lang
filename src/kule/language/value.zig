const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;


const Semantics = language.Semantics;

const Module = Semantics.Module;
const Function = Semantics.Function;

const Taip = language.Taip;

const ScalarTp = Taip.Scalar;
const VectorTp = Taip.Vector;
const MatrixTp = Taip.Matrix;

const Dims = Taip.Dims;

pub const Value = struct {
    taip: Taip,
    data: Data,

    pub const Data = union(enum) {
    
        taip: Taip,
        module: *Module,
        scalar: Scalar,
        vector: Vector,
        matrix: Matrix,
        function: *Function,

        pub const Scalar = Taip.ScalarTaip.Union(struct {
            fn Field(T: type) type {
                return T;
            }
        }.Field);

        pub const Vector = Taip.ScalarTaip.Union(struct {
            fn Field(T: type) type {
                return [4]T;
            }
        }.Field);

        pub const Matrix = Taip.ScalarTaip.Union(struct {
            fn Field(T: type) type {
                return [4][4]T;
            }
        }.Field);
    
    };
    
        
};