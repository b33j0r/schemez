const std = @import("std");
const mecha = @import("mecha");

const Expr = union(enum) {
    Symbol: []const u8,
    Number: u64,
    List: []Expr,
};

const Value = union(enum) {
    Symbol: []const u8,
    Number: u64,
    List: []Value,
    Func: *const fn (env: *Env, args: []Value) Value,
};

const Builtins = struct {
    fn define(env: *Env, args: []Value) Value {
        if (args.len != 2) return Value{ .Symbol = "error" }; // TODO: error handling

        const name_val = args[0];
        const value_val = args[1];

        if (name_val != Value.Symbol) {
            return Value{ .Symbol = "error" };
        }

        env.set(name_val.Symbol, value_val) catch unreachable;

        return name_val;
    }

    fn add(_: *Env, args: []Value) Value {
        if (args.len != 2) return Value{ .Symbol = "error" }; // TODO: error handling

        const a = args[0].Number;
        const b = args[1].Number;

        return Value{ .Number = a + b };
    }

    fn sub(_: *Env, args: []Value) Value {
        if (args.len != 2) return Value{ .Symbol = "error" }; // TODO: error handling

        const a = args[0].Number;
        const b = args[1].Number;

        return Value{ .Number = a - b };
    }
};

const Env = struct {
    vars: std.StringHashMap(Value),

    const EvalError = error{
        UnknownSymbol,
        NotAFunction,
        EmptyApplication,
        OutOfMemory,
    };

    fn init() Env {
        return Env{
            .vars = std.StringHashMap(Value).init(std.heap.page_allocator),
        };
    }

    fn deinit(self: *Env) void {
        self.vars.deinit();
    }

    fn get(self: *Env, name: []const u8) ?Value {
        return self.vars.get(name);
    }

    fn set(self: *Env, name: []const u8, val: Value) !void {
        try self.vars.put(name, val);
    }

    fn eval(self: *Env, expr: Expr) EvalError!Value {
        return switch (expr) {
            Expr.Symbol => |sym| {
                const val = self.get(sym) orelse Value{ .Symbol = sym };
                return val;
            },
            Expr.Number => |num| Value{ .Number = num },
            Expr.List => |list| {
                const operator = try self.eval(list[0]);
                const func = operator.Func;
                const args = try self.evalList(list[1..]);
                return func(self, args);
            },
        };
    }

    fn evalList(self: *Env, exprs: []Expr) EvalError![]Value {
        var values = try std.ArrayList(Value).initCapacity(std.heap.page_allocator, exprs.len);
        defer values.deinit();

        for (exprs) |expr| {
            const val = try self.eval(expr);
            try values.append(val);
        }

        return values.toOwnedSlice();
    }
};

const expect = std.testing.expect;

pub fn strcmp(a: []const u8, b: []const u8) i32 {
    for (a, b) |a_char, b_char| {
        if (a_char != b_char) {
            return @intCast(a_char - b_char);
        }
    }
    return 0;
}

test "env stores vars" {
    var env = Env.init();
    const symbol = Value{ .Symbol = "bar" };
    env.set("foo", symbol) catch unreachable;
    const foo = env.get("foo") orelse unreachable;
    try expect(strcmp(foo.Symbol, "bar") == 0);
}

const ws = mecha.ascii.whitespace.many(.{ .collect = false });

// parseSymbol: many(alphanumeric).asStr + ws → strip ws → map to Expr.Symbol
const parseSymbol = mecha
    .combine(.{
        mecha.many(mecha.oneOf(.{ mecha.ascii.alphanumeric, mecha.ascii.char('-'), mecha.ascii.char('+'), mecha.ascii.char('*'), mecha.ascii.char('/'), mecha.ascii.char('%'), mecha.ascii.char('_'), mecha.ascii.char('='), mecha.ascii.char('!'), mecha.ascii.char('?') }), .{ .min = 1 }).asStr(),
        ws.discard(),
    })
    .map(struct {
    fn makeSymbol(name: []const u8) Expr {
        return Expr{ .Symbol = name };
    }
}.makeSymbol);

// parseNumber: int(u64) + ws → strip ws → map to Expr.Number
const parseNumber = mecha
    .combine(.{
        mecha.int(u64, .{}),
        ws.discard(),
    })
    .map(struct {
    fn makeNumber(val: u64) Expr {
        return Expr{ .Number = val };
    }
}.makeNumber);

const lparen = token(mecha.utf8.char('('));
const rparen = token(mecha.utf8.char(')'));
const parseExpr = mecha.ref(parseExprRef);

fn toList(elems: []Expr) Expr {
    return Expr{ .List = elems };
}

const parseList = mecha.combine(.{
    lparen,
    mecha.many(parseExpr, .{}),
    rparen,
})
    .map(toList);

fn parseExprRef() mecha.Parser(Expr) {
    return mecha.oneOf(.{ parseNumber, parseSymbol, parseList });
}

fn unwrap(e: Expr) Expr {
    return e;
}
const topExpr = mecha.combine(.{ ws.discard(), parseExpr, ws.discard() })
    .map(unwrap);

fn dump(e: Expr, indent: usize) void {
    for (0..indent) |_| std.debug.print(" ", .{});
    switch (e) {
        .Symbol => |s| std.debug.print("Symbol \"{s}\"\n", .{s}),
        .Number => |n| std.debug.print("Number {d}\n", .{n}),
        .List => |items| {
            std.debug.print("List\n", .{});
            for (items) |child| dump(child, indent + 2);
        },
    }
}

fn token(comptime p: anytype) mecha.Parser(void) {
    return mecha.combine(.{ p, ws.discard() }).discard();
}

pub fn main() !void {
    var env = Env.init();
    env.set("define", Value{ .Func = Builtins.define }) catch |err| {
        std.debug.print("Error setting 'define': {any}\n", .{err});
    };
    env.set("+", Value{ .Func = Builtins.add }) catch |err| {
        std.debug.print("Error setting '+': {any}\n", .{err});
    };
    env.set("-", Value{ .Func = Builtins.sub }) catch |err| {
        std.debug.print("Error setting '-': {any}\n", .{err});
    };

    // const input = "(define square (lambda (x) (* (+ x 1) x)))";
    const input = "(+ 1 2)";
    const res = try topExpr.parse(std.heap.page_allocator, input);
    switch (res.value) {
        .ok => |expr| {
            dump(expr, 0);

            const eval_result = env.eval(expr);
            std.debug.print("Eval result: {any}\n", .{eval_result});
        },
        .err => |_| {
            // parse didn’t match — report how far we got:
            std.debug.print("Parse failed at byte index {d}\n", .{res.index});
            return;
        },
    }
}
