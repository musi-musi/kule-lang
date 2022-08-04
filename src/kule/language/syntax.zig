const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

const Token = language.Token;
const Tag = language.Token.Tag;

pub const Syntax = struct {

    root_module: RootModule = .{},
    is_partial: bool = false, 

    pub const RootModule = struct {
        statements: ?Statements = null,
    };

    pub const Statements = []Statement;

    pub const Statement = struct {
        kw_pub: ?Token,
        body: Body,

        pub const Body = union(enum) {
            binding: Binding,
        };

        pub const Binding = struct {
            kw_let: Token,
            decl: Decl,
            where_clauses: ?[]WhereClause,
        };

    };

    pub const WhereClause = struct {
        kw_where: Token,
        decl: Decl,
    };

    pub const Decl = struct {

        name: Token,
        param_list: ?ParamList,
        type_expr: ?TypeExpr,
        equal: Token, 
        expr: Expr,

        pub const ParamList = struct {
            lparen: Token,
            params: []Param,
            rparen: Token,
        };

        pub const Param = struct {
            name: Token,
            type_expr: TypeExpr,
        };
    };

    pub const TypeExpr = struct {
        colon: Token,
        expr: Expr,
    };

    pub const Expr = struct {
        expr: Add,

        fn Binary(comptime op_tags: []const Tag, comptime Operand: type) type {
            return struct {
                operand: Operand,
                terms: ?[]Term,

                
                pub const Term = struct {
                    op: Token,
                    operand: Operand,
                    pub const op = op_tags;
                };

            };
        }

        fn Prefix(comptime op_tags: []const Tag, comptime Operand: type) type {
            return struct {
                op: ?Token,
                operand: Operand,

                pub const op = op_tags;
            };
        }

        pub const Add = Binary(&[_]Tag{ .plus, .minus, }, Mul );
        pub const Mul = Binary(&[_]Tag{ .aster, .fslash, }, Neg );

        pub const Neg = Prefix(&[_]Tag{ .plus, .minus, }, Eval );

        pub const Eval = struct {
            function: Access,
            params: ?ParamList,

            pub const ParamList = struct {
                lparen: Token,
                params: []Expr,
                rparen: Token,
            };

        };

        pub const Access = struct {
            container: Atom,
            member: ?Member,

            pub const Member = struct {
                dot: Token,
                name: Token,
            };
        };

        pub const Atom = union(enum) {
            number: Token,
            name: Token,
            module: Module,
            import: Import,
            parens: Parens,

            pub const Module = struct {
                kw_module: Token,
                lcurly: Token,
                statements: ?Statements,
                rcurly: Token,

            };

            pub const Import = struct {
                kw_import: Token,
                import_path: Token,
            };

            pub const Parens = struct {
                lparen: Token,
                expr: *Expr,
                rparen: Token,
            };
        };

    };

};

