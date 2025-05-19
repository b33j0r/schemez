const std = @import("std");
const mecha = @import("mecha");

const Expr = union(enum) {
    Symbol: []const u8,
    Number: u64,
    List: []Expr,
};

const Closure = struct {
    params: [][]const u8,
    body: Expr,
    env: *Env,

    pub fn call(self: *Closure, args: []Value) Value {
        var local_env = Env.init();
        defer local_env.deinit();

        // Copy the parent environment
        var it = self.env.vars.iterator();
        while (it.next()) |entry| {
            local_env.set(entry.key_ptr.*, entry.value_ptr.*) catch unreachable;
        }

        // Bind parameters to arguments
        if (args.len != self.params.len) {
            return Value{ .Symbol = "error: argument count mismatch" };
        }
        for (self.params, args) |param, arg| {
            local_env.set(param, arg) catch unreachable;
        }

        // Evaluate the body in the new environment
        return local_env.eval(self.body) catch Value{ .Symbol = "error: evaluation failed" };
    }
};

const Value = union(enum) {
    Symbol: []const u8,
    Number: u64,
    List: []Value,
    Func: *const fn (env: *Env, args: []Value) Value,
    Closure: *Closure,

    fn toExpr(self: Value) Expr {
        return switch (self) {
            Value.Symbol => |s| Expr{ .Symbol = s },
            Value.Number => |n| Expr{ .Number = n },
            Value.List => |list| {
                var exprs = std.ArrayList(Expr).init(std.heap.page_allocator);
                defer exprs.deinit();

                for (list) |item| {
                    exprs.append(item.toExpr()) catch unreachable;
                }

                return Expr{ .List = exprs.toOwnedSlice() catch unreachable };
            },
            else => Expr{ .Symbol = "error" },
        };
    }
};

const Builtins = struct {
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

    fn mul(_: *Env, args: []Value) Value {
        if (args.len != 2) return Value{ .Symbol = "error" }; // TODO: error handling

        const a = args[0].Number;
        const b = args[1].Number;

        return Value{ .Number = a * b };
    }

    fn div(_: *Env, args: []Value) Value {
        if (args.len != 2) return Value{ .Symbol = "error" }; // TODO: error handling

        const a = args[0].Number;
        const b = args[1].Number;

        if (b == 0) return Value{ .Symbol = "error: division by zero" }; // TODO: error handling

        return Value{ .Number = a / b };
    }

    fn mod(_: *Env, args: []Value) Value {
        if (args.len != 2) return Value{ .Symbol = "error" }; // TODO: error handling

        const a = args[0].Number;
        const b = args[1].Number;

        if (b == 0) return Value{ .Symbol = "error: division by zero" }; // TODO: error handling

        return Value{ .Number = a % b };
    }
};

/// A “special form” takes the raw AST of its arguments (Expr[])
/// and returns a Value without pre‐evaluating those args.
const SpecialForms = struct {
    // pub const FormFn = fn (env: *Env, rawArgs: []Expr) Env.EvalError!Value;
    pub const FormFn = *const fn (env: *Env, rawArgs: []Expr) Env.EvalError!Value;

    /// (define name expr) ⇒ evaluate expr, install into env under name
    pub fn define(env: *Env, rawArgs: []Expr) Env.EvalError!Value {
        if (rawArgs.len != 2) return Env.EvalError.EmptyApplication;
        // first arg must be a symbol literal:
        return switch (rawArgs[0]) {
            Expr.Symbol => |name_sym| {
                const val = try env.eval(rawArgs[1]);
                try env.set(name_sym, val);
                return val;
            },
            else => return Env.EvalError.NotAFunction,
        };
    }

    /// (lambda (p1 p2 …) body) ⇒ package a Closure capturing env
    pub fn lambda(env: *Env, rawArgs: []Expr) Env.EvalError!Value {
        if (rawArgs.len != 2) return Env.EvalError.EmptyApplication;
        // rawArgs[0] must be a list of symbols…
        return switch (rawArgs[0]) {
            Expr.List => |paramExprs| {
                var names = std.ArrayList([]const u8).init(std.heap.page_allocator);
                defer names.deinit();
                for (paramExprs) |p| {
                    switch (p) {
                        Expr.Symbol => |s| names.append(s) catch return Env.EvalError.OutOfMemory,
                        else => return Env.EvalError.NotAFunction,
                    }
                }
                const c = std.heap.page_allocator.create(Closure) catch return Env.EvalError.OutOfMemory;
                c.* = Closure{
                    .params = names.toOwnedSlice() catch unreachable,
                    .body = rawArgs[1],
                    .env = env,
                };
                return Value{ .Closure = c };
            },
            else => return Env.EvalError.NotAFunction,
        };
    }

    /// Lookup table for special forms
    pub fn get(name: []const u8) ?FormFn {
        if (std.mem.eql(u8, name, "define")) return SpecialForms.define;
        if (std.mem.eql(u8, name, "lambda")) return SpecialForms.lambda;
        return null;
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
                // variables or self-evaluating symbol
                return self.get(sym) orelse Value{ .Symbol = sym };
            },
            Expr.Number => |n| Value{ .Number = n },
            Expr.List => |list| {
                if (list.len == 0) return EvalError.EmptyApplication;

                // Check for special forms
                switch (list[0]) {
                    Expr.Symbol => |headSym| {
                        if (SpecialForms.get(headSym)) |formFn| {
                            return formFn(self, list[1..]);
                        }
                    },
                    else => {},
                }

                // Otherwise, we have a function call:
                const opVal = try self.eval(list[0]);
                const argVals = try self.evalList(list[1..]);
                return switch (opVal) {
                    Value.Func => |fnptr| fnptr(self, argVals),
                    Value.Closure => |c| c.call(argVals),
                    else => return EvalError.NotAFunction,
                };
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
const topExpr = mecha.combine(.{
    // eat whitespace
    ws.discard(),
    mecha.many(parseExpr, .{}),
    ws.discard(),
}).map(struct {
    fn getExprs(exprs: []Expr) []Expr {
        return exprs;
    }
}.getExprs);

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
    env.set("+", Value{ .Func = Builtins.add }) catch |err| {
        std.debug.print("Error setting '+': {any}\n", .{err});
    };
    env.set("-", Value{ .Func = Builtins.sub }) catch |err| {
        std.debug.print("Error setting '-': {any}\n", .{err});
    };
    env.set("*", Value{ .Func = Builtins.mul }) catch |err| {
        std.debug.print("Error setting '*': {any}\n", .{err});
    };
    env.set("/", Value{ .Func = Builtins.div }) catch |err| {
        std.debug.print("Error setting '/': {any}\n", .{err});
    };
    env.set("%", Value{ .Func = Builtins.mod }) catch |err| {
        std.debug.print("Error setting '%': {any}\n", .{err});
    };

    const input =
        \\(define square-plus-1 (lambda (x) (+ (* x x) 1)))
        \\(square-plus-1 5)
    ;
    // const input = "(+ 1 2)";
    const result = try topExpr.parse(std.heap.page_allocator, input);
    switch (result.value) {
        .ok => |exprs| {
            for (exprs) |expr| {
                dump(expr, 0);
                const val = env.eval(expr) catch |e| {
                    std.debug.print("Eval error: {}\n", .{e});
                    continue;
                };
                std.debug.print("Eval result: {any}\n", .{val});
            }
        },
        .err => |_| {
            std.debug.print("Parse failed at byte index {}\n", .{result.index});
        },
    }
}
