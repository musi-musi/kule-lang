const std = @import("std");
const compiler = @import("../compiler.zig");
const language = @import("../language.zig");
const diagnostics = @import("../diagnostics.zig");


const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const CompilationUnit = compiler.CompilationUnit;

// const Source = compiler.Source;
// const SourceLocation = compiler.SourceLocation;
const TokenStream = compiler.TokenStream;

const Token = language.Token;
const Tag = Token.Tag;

const Syntax = language.Syntax;

const Statement = Syntax.Statement;
const WhereClause = Syntax.WhereClause;
const Decl = Syntax.Decl;
const TypeExpr = Syntax.TypeExpr;
const Expr = Syntax.Expr;



const RuleError = error {
    ListEmpty,
} || ExpectError || AllocError;

const ExpectError = error {
    Unexpected,
};

const AllocError = Allocator.Error;

pub fn parseUnit(unit: *CompilationUnit) RuleError!void {
    var tokens = TokenStream.init(unit.source, &unit.diagnostics);
    const parser = Parser.init(unit, &tokens);
    unit.syntax = try RSyntax.accept(parser);
    // return try parser.parse();
}

fn Rule(comptime Node: type, comptime parse_fn: fn(Parser)RuleError!Node) type 
    { return RuleExt(Node, null, {}, parse_fn); }

fn RuleExt(comptime Node: type, comptime name: ?[]const u8, comptime expecting_items: anytype, comptime parse_fn: fn(Parser)RuleError!Node) type {
    return struct {
        const start_tags = nodeStartTags(Node);
        const expecting_list = FormatList.init(expecting_items);

        fn Named(comptime new_name: []const u8) type {
            return RuleExt(Node, new_name, expecting_list, parse_fn);
        }

        fn Expecting(comptime items: anytype) type {
            return RuleExt(Node, name, items, parse_fn);
        }

        fn acceptOpt(p: Parser) RuleError!?Node {
            if (p.expectOneOpt(start_tags)) |_| {
                return try accept(p);
            }
            else {
                return null;
            }
        }

        fn accept(p: Parser) RuleError!Node {
            var parser = p;
            _ = try parser.expectOne(start_tags);
            return try parse_fn(parser);
        }

        fn acceptAlloc(p: Parser) RuleError!*Node {
            const allocator = p.allocator();
            const node = try allocator.create(Node);
            errdefer allocator.destroy(node);
            node.* = try accept(p);
            return node;
        }

        fn acceptListOpt(p: Parser, comptime end_tags: []const Tag) RuleError!?[]Node {
            return acceptDelimListOpt(p, &[_]Tag{}, end_tags);
        }

        fn acceptList(p: Parser, comptime end_tags: []const Tag) RuleError![]Node {
            return acceptDelimList(p, &[_]Tag{}, end_tags);
        }

        fn acceptDelimListOpt(p: Parser, comptime delim_tags: []const Tag, comptime end_tags: []const Tag) RuleError!?[]Node {
            if (p.expectOneOpt(start_tags) == null) {
                return null;
            }
            else {
                return try acceptDelimList(p, delim_tags, end_tags);
            }
        }

        fn acceptDelimList(p: Parser, comptime delim_tags: []const Tag, comptime end_tags: []const Tag) RuleError![]Node {
            const allocator = p.allocator();
            var list = std.ArrayListUnmanaged(Node){};
            errdefer if (list.items.len > 0) list.deinit(allocator);
            while (true)  {
                if (delim_tags.len > 0 and list.items.len > 0) {
                    _ = try p.expecting(delim_tags ++ end_tags).acceptOne(delim_tags);
                }
                if (atEndOfDelimList(p, delim_tags, end_tags)) {
                    break;
                }
                if (accept(p.expecting(comptime expecting_list.addTags(end_tags)))) |node| {
                    try list.append(allocator, node);
                }
                else |err| {
                    return err;
                    // p.skipToOne(start_tags ++ end_tags ++ delim_tags);
                }
                if (atEndOfDelimList(p, delim_tags, end_tags)) {
                    break;
                }
            }
            if (list.items.len == 0)  {
                if (name) |n| {
                    p.unit.diagnostics.logError(p.tokens.lookahead.text, "{s} list must have at least one item", .{n});
                }
                else {
                    p.unit.diagnostics.logError(p.tokens.lookahead.text, "list must have at least one item", .{});
                }
            }
            return list.toOwnedSlice(allocator);
        }

        fn atEndOfDelimList(p: Parser, comptime delim_tags: []const Tag, comptime end_tags: []const Tag) bool {
            if (end_tags.len > 0) {
                if (p.expectOneOpt(end_tags)) |_| {
                    return true;
                }
            }
            else if (p.expectOneOpt(start_tags ++ delim_tags) == null) {
                return true;
            }
            return false;
        }


    };
}

const RSyntax = Rule(Syntax, struct {
    fn f(p: Parser) RuleError!Syntax {
        const statement_tags = RStatement.start_tags;
        _ = try p.expectOne(statement_tags ++ &[_]Tag{.end_of_file});
        return Syntax {
            .statements = try RStatement.Expecting(.{.kw_where, statement_tags}).acceptListOpt(p, &.{.end_of_file}),
            .end_of_file = try p.accept(.end_of_file),
        };
    }
}.f);

fn RStatements(comptime end_tags: []const Tag) type {
    return Rule([]Statement, struct {
        fn f(p: Parser) RuleError![]Statement {
            return RStatement.acceptList(p, end_tags);
        }
    }.f);
}

const RStatement = Rule(Statement, struct {
    fn f(p: Parser) RuleError!Statement {
        return Statement {
            .kw_pub = p.expecting(.{.kw_pub, RStatementBody.start_tags}).acceptOpt(.kw_pub),
            .body = try RStatementBody.accept(p),
        };
    }
}.f);

const RStatementBody = Rule(Statement.Body, struct {
    fn f(p: Parser) RuleError!Statement.Body {
        return Statement.Body {
            .binding = try RStatementBinding.accept(p),
        };
    }
}.f);

const RStatementBinding = Rule(Statement.Binding, struct {
    fn f(p: Parser) RuleError!Statement.Binding {
        return Statement.Binding {
            .kw_let = try p.accept(.kw_let),
            .decl = try RDecl.accept(p),
            .where_clauses = try RWhereClause.acceptListOpt(p, &[_]Tag{}),
        };
    }
}.f);

const RWhereClause = Rule(WhereClause, struct {
    fn f(p: Parser) RuleError!WhereClause {
        return WhereClause {
            .kw_where = try p.expecting(.{.kw_where, RStatement.start_tags}).accept(.kw_where),
            .decl = try RDecl.accept(p),
        };
    }
}.f);

const RDecl = Rule(Decl, struct {
    fn f(p: Parser) RuleError!Decl {
        return Decl {
            .name = try p.accept(.name),
            .param_list = try RDeclParamList.acceptOpt(p),
            .type_expr = try RTypeExpr.acceptOpt(p),
            .equal = try p.accept(.equal),
            .expr = try RExpr.accept(p.expecting("expression")),
        };
    }
}.f);

const RDeclParamList = Rule(Decl.ParamList, struct {
    fn f(p: Parser) RuleError!Decl.ParamList {
        return Decl.ParamList {
            .lparen = try p.accept(.lparen),
            .params = try RDeclParam.acceptDelimList(p, &[_]Tag{.comma}, &[_]Tag{.rparen}),
            .rparen = try p.accept(.rparen),
        };
    }
}.f);

const RDeclParam = Rule(Decl.Param, struct {
    fn f(p: Parser) RuleError!Decl.Param {
        return Decl.Param {
            .name = try p.accept(.name),
            .type_expr = try RTypeExpr.accept(p),
        };
    }
}.f);

const RTypeExpr = Rule(TypeExpr, struct {
    fn f(p: Parser) RuleError!TypeExpr {
        return TypeExpr {
            .colon = try p.accept(.colon),
            .expr = try RExpr.accept(p.expecting("type expression")),
        };
    }
}.f);

const RExpr = Rule(Expr, struct {
    fn f(p: Parser) RuleError!Expr {
        return Expr {
            .expr = try RExprAdd.accept(p),
        };
    }
}.f);

fn RExprBinary(comptime Node: type, comptime ROperand: type) type {
    return struct {
        const RExprBin = Rule(Node, struct {
            fn f(p: Parser) RuleError!Node {
                return Node {
                    .operand = try ROperand.accept(p.expecting("value")),
                    .terms = try RTerm.acceptListOpt(p, &[_]Tag{}),
                };
            }
        }.f);
        const RTerm = Rule(Node.Term, struct {
            fn f(p: Parser) RuleError!Node.Term {
                return Node.Term {
                    .op = try p.expecting(null).acceptOne(Node.Term.op),
                    .operand = try ROperand.accept(p.expecting("value")),
                };
            }
        }.f);
    }.RExprBin;
}

fn RExprPrefix(comptime Node: type, comptime ROperand: type) type {
    return Rule(Node, struct {
        fn f(p: Parser) RuleError!Node {
            return Node {
                .op = p.acceptOneOpt(Node.op),
                .operand = try ROperand.accept(p.expecting("value")),
            };
        }
    }.f);
}

const RExprAdd = RExprBinary(Expr.Add, RExprMul);
const RExprMul = RExprBinary(Expr.Mul, RExprNeg);

const RExprNeg = RExprPrefix(Expr.Neg, RExprEval);

const RExprEval = Rule(Expr.Eval, struct {
    fn f(p: Parser) RuleError!Expr.Eval {
        return Expr.Eval {
            .function = try RExprAccess.accept(p.expecting("value")),
            .params = try RExprEvalParamList.acceptOpt(p.expecting(null))
        };
    }
}.f);

const RExprEvalParamList = Rule(Expr.Eval.ParamList, struct {
    fn f(p: Parser) RuleError!Expr.Eval.ParamList {
        return Expr.Eval.ParamList {
            .lparen = try p.accept(.lparen),
            .params = try RExpr.Named("parameter value").acceptDelimList(p, &[_]Tag{ .comma }, &[_]Tag{ .rparen }),
            .rparen = try p.accept(.rparen),
        };
    }
}.f);

const RExprAccess = Rule(Expr.Access, struct {
    fn f(p: Parser) RuleError!Expr.Access {
        return Expr.Access {
            .container = try RExprAtom.accept(p),
            .member = try RExprAccessMember.acceptOpt(p),
        };
    }
}.f);

const RExprAccessMember = Rule(Expr.Access.Member, struct {
    fn f(p: Parser) RuleError!Expr.Access.Member {
        return Expr.Access.Member {
            .dot = try p.accept(.dot),
            .name = try p.expecting("member name").accept(.name),
        };
    }
}.f);

const RExprAtom = Rule(Expr.Atom, struct {
    fn f(parser: Parser) RuleError!Expr.Atom {
        const p = parser.expecting(comptime nodeStartTags(Expr.Atom));
        if (p.acceptOpt(.number)) |number| {
            return Expr.Atom {
                .number = number,
            };
        }
        else if (p.acceptOpt(.name)) |name| {
            return Expr.Atom {
                .name = name,
            };
        }
        else if (try RExprModule.acceptOpt(p)) |atom| {
            return atom;
        }
        else if (try RExprImport.acceptOpt(p)) |import| {
            return Expr.Atom {
                .import = import,
            };
        }
        else {
            return RuleError.Unexpected;
        }

    }
}.f);

const RExprModule = Rule(Expr.Atom, struct {
    fn f(p: Parser) RuleError!Expr.Atom {
        const kw_module = try p.accept(.kw_module);
        if (p.acceptOpt(.lcurly)) |lcurly| {
            return Expr.Atom {
                .module = .{
                    .kw_module = kw_module,
                    .lcurly = lcurly,
                    .statements = try RStatements(&.{.rcurly}).acceptOpt(p),
                    .rcurly = try p.accept(.rcurly),
                },
            };
        }
        else {
            const name = Token.init(.name, kw_module.text);
            return Expr.Atom {
                .name = name,
            };
        }
    }
}.f);


const RExprImport = Rule(Expr.Atom.Import, struct {
    fn f(p: Parser) RuleError!Expr.Atom.Import {
        return Expr.Atom.Import {
            .kw_import = try p.accept(.kw_import),
            .import_path = try p.expecting(null).accept(.import_path),
        };
    }
}.f);

const RExprParens = Rule(Expr.Atom.Parens, struct {
    fn f(p: Parser) RuleError!Expr.Atom.Parens {
        return Expr.Atom.Parens {
            .lparen = try p.accept(.lparen),
            .expr = try RExpr.acceptAlloc(p.expecting("expression")),
            .rparen = try p.accept(.rparen),
        };
    }
}.f);

const FormatList = struct {
    items: []const []const u8 = &.{},

    const empty = FormatList{};

    fn init(comptime items: anytype) FormatList {
        const meta = std.meta;
        const trait = meta.trait;
        const Items = @TypeOf(items);
        if (Items == void or trait.is(.Null)(Items)) {
            return empty;
        }
        else if (trait.is(.Optional)(Items)) {
            if (items) |i| {
                return init(i);
            }
            else {
                return init({});
            }
        }
        else if (Items == type and @hasDecl(items, "name_list")) {
            return items.name_list;
        }
        else if (Items == FormatList) {
            return items;
        }
        else if (comptime trait.isZigString(Items)) {
            return empty.addString(items);
        }
        else if (trait.isIndexable(Items)) {
            var list = empty;
            for (items) |item| {
                list = list.concat(init(item));
            }
            return list;
        }
        else if (Items == Tag) {
            return empty.addTags(&.{items});
        }
        else if (trait.is(.EnumLiteral)(Items)) {
            return empty.addTags(&.{std.enums.nameCast(Tag, items)});
        }
        else {
            @compileError(@typeName(Items) ++ " cannot be converted to a FormatList");
        }
    }

    fn concat(comptime self: FormatList, comptime next: FormatList) FormatList {
        return self.addStrings(next.items);
    }

    fn addString(comptime self: FormatList, comptime text: []const u8) FormatList {
        return comptime self.addStrings(&[_][]const u8{ text });
    }

    fn addStrings(comptime self: FormatList, comptime text: []const []const u8) FormatList {
        return comptime .{ .items = self.items ++ text };
    }

    fn addTags(comptime self: FormatList, comptime tags: []const Tag) FormatList {
        comptime {
            var list = self;
            for (tags) |tag| {
                list = list.addString(std.fmt.comptimePrint("{}", .{tag}));
            }
            return list;
        }
    }

    fn toString(comptime self: FormatList, comptime last_sep: ?[]const u8) []const u8 {
        comptime {
            const sep: []const u8 = last_sep orelse "";
            return std.fmt.comptimePrint("{" ++ sep ++ "}", .{self});
        }
    }

    pub fn format(list: FormatList, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (list.items) |item, i| {
            if (i > 0) {
                if ((fmt.len == 0 or list.items.len > 2)) {
                    try writer.writeAll(", ");
                }
                else {
                    try writer.writeByte(' ');
                }
            }
            if (fmt.len > 0 and list.items.len > 1 and i == list.items.len - 1) {
                try writer.print("{s} ", .{fmt});
            }
            try writer.writeAll(item);
        }
    }

};

fn nodeStartTags(comptime Node: type) []const Tag {
    var tags: []const Tag = &.{};
    switch (@typeInfo(Node)) {
        .Struct => |Struct| {
            for (Struct.fields) |field| {
                tags = tags ++ nodeFieldStartTags(Node, field);
                if (!isOptional(field.field_type)) {
                    break;
                }
            }
            return tags;
        },
        .Union => |Union| {
            for (Union.fields) |field| {
                tags = tags ++ nodeFieldStartTags(Node, field);
            }
            return tags;
        },
        .Pointer, .Optional => {
            return nodeStartTags(std.meta.Child(Node));
        },
        else => @compileError(@typeName(Node) ++ " is not a syntax node"),
    }
}

const isOptional = std.meta.trait.is(.Optional);

fn nodeFieldStartTags(comptime Node: type, comptime field: anytype) []const Tag {
    const T = switch (@typeInfo(field.field_type)) {
        .Optional, .Pointer => std.meta.Child(field.field_type),
        else => field.field_type,
    };
    if (T == Token) {
        if (@hasField(Tag, field.name)) {
            return &[_]Tag { @field(Tag, field.name) };
        }
        else if (@hasDecl(Node, field.name)) {
            return @field(Node, field.name);
        }
        else {
            @compileError(field.name ++ " is not an accepted token tag and " ++ @typeName(Node) ++ " has no list of accepted tags");
        }
    }
    else {
        return nodeStartTags(T);
    }
}

const Parser = struct {

    unit: *CompilationUnit,
    tokens: *TokenStream,
    expecting_desc: ?[]const u8 = null,

    

    const Self = @This();

    fn init(unit: *CompilationUnit, tokens: *TokenStream) Self {
        return Self {
            .unit = unit,
            .tokens = tokens,
        };
    }

    fn allocator(self: Self) Allocator {
        return self.unit.arena.allocator();
    }

    fn Opt(comptime T: type) type {
        if (isOptional(T)) {
            return T;
        }
        else {
            return ?T;
        }
    }

    fn expecting(self: Self, comptime items: anytype) Parser {
        var parser = self;
        const list = comptime FormatList.init(items);
        if (list.items.len == 0) {
            parser.expecting_desc = null;
        }
        else {
            parser.expecting_desc = comptime list.toString("or");
        }
        return parser;
    }

    

    fn skipToOne(self: Self, comptime tags: []const Tag) void {
        while (self.expectOneOpt(tags) == null and self.tokens.lookahead.tag != .end_of_file) {
            _ = self.tokens.next();
        }
    } 

    fn expect(self: Self, comptime tag: Tag) ExpectError!Token {
        return self.expectOne(&[_]Tag{tag});
    }

    fn expectOne(self: Self, comptime tags: []const Tag) ExpectError!Token {
        if (self.expectOneOpt(tags)) |token| {
            return token;
        }
        else {
            const next = self.tokens.lookahead;
            const desc = (
                if (self.expecting_desc) |desc| desc
                else comptime FormatList.init(tags).toString("or")
            );
            self.unit.diagnostics.logError(next.text, "expected {s}, found {}", .{desc, next.tag});
            return ExpectError.Unexpected;
        }
    }

    fn expectOpt(self: Self, comptime tag: Tag) ?Token {
        return self.expectOneOpt(&[_]Tag{tag});
    }

    fn expectOneOpt(self: Self, comptime tags: []const Tag) ?Token {
        const next = self.tokens.lookahead;
        inline for (tags) |tag| {
            if (next.tag == tag) {
                return next;
            }
        }
        return null;
    }
    

    fn accept(self: Self, comptime tag: Tag) ExpectError!Token {
        return self.acceptOne(&[_]Tag{tag});
    }

    fn acceptOne(self: Self, comptime tags: []const Tag) ExpectError!Token {
        if (self.expectOne(tags)) |token| {
            _ = self.tokens.next();
            return token;
        }
        else |err| {
            return err;
        }
    }
    
    fn acceptOpt(self: Self, comptime tag: Tag) ?Token {
        return self.acceptOneOpt(&[_]Tag{tag});
    }

    fn acceptOneOpt(self: Self, comptime tags: []const Tag) ?Token {
        if (self.expectOneOpt(tags)) |token| {
            _ = self.tokens.next();
            return token;
        }
        else {
            return null;
        }
    }

};