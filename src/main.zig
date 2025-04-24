const std = @import("std");
const mecha = @import("mecha");

const Expr = union(enum) {
    Symbol: []const u8,
    Number: u64,
    List: []Expr,
};

fn makeSymbol(name: []const u8) Expr {
    return Expr{ .Symbol = name };
}
fn makeNumber(val: u64) Expr {
    return Expr{ .Number = val };
}

const ws = mecha.ascii.whitespace.many(.{ .collect = false });

// parseSymbol: many(alphanumeric).asStr + ws → strip ws → map to Expr.Symbol
const parseSymbol = mecha
    .combine(.{
        mecha.many(mecha.oneOf(.{ mecha.ascii.alphanumeric, mecha.ascii.char('-'), mecha.ascii.char('+'), mecha.ascii.char('*'), mecha.ascii.char('/'), mecha.ascii.char('%'), mecha.ascii.char('_'), mecha.ascii.char('='), mecha.ascii.char('!'), mecha.ascii.char('?') }), .{ .min = 1 }).asStr(),
        ws.discard(),
    })
    .map(makeSymbol);

// parseNumber: int(u64) + ws → strip ws → map to Expr.Number
const parseNumber = mecha
    .combine(.{
        mecha.int(u64, .{}),
        ws.discard(),
    })
    .map(makeNumber);

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
    const input = "(define square (lambda (x) (* (+ x 1) x)))";
    const res = try topExpr.parse(std.heap.page_allocator, input);

    // res.value is union(enum) { ok: Expr, err: void }
    switch (res.value) {
        .ok => |expr| {
            dump(expr, 0);
        },
        .err => |_| {
            // parse didn’t match — report how far we got:
            std.debug.print("Parse failed at byte index {d}\n", .{res.index});
            return;
        },
    }
}
