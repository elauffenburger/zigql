const std = @import("std");

pub const QueryDef = struct {
    pub const Selector = union(Tag) {
        pub const Tag = enum {
            field,
        };

        pub const FieldNode = struct {
            name: []const u8,
            children: []const Selector,
        };

        field: FieldNode,
    };

    selector: ?Selector,
};

pub const QueryParser = struct {
    const Self = @This();

    pub const SelectorPool = struct {
        buf: []QueryDef.Selector,
        len: u32 = 0,

        pub fn init(storage: []QueryDef.Selector) SelectorPool {
            return .{ .buf = storage };
        }

        pub fn push(self: *SelectorPool, val: QueryDef.Selector) !void {
            if (self.len >= self.buf.len) {
                unreachable;
            }

            self.buf[self.len] = val;
            self.len += 1;
        }

        pub fn slice(self: SelectorPool) []const QueryDef.Selector {
            return self.buf[0..self.len];
        }
    };

    input: []const u8,
    cursor: u32 = 0,

    selectorPools: []SelectorPool,
    numSelectorPools: u32 = 0,

    pub fn init(query: []const u8, selectorPools: []SelectorPool) Self {
        return .{
            .input = query,
            .selectorPools = selectorPools,
        };
    }

    pub fn parse(self: *Self) !QueryDef {
        return .{
            .selector = blk: {
                const selectors = try self.parseSelectors();

                if (selectors.len == 0) {
                    break :blk null;
                }

                if (selectors.len == 1) {
                    break :blk selectors[0];
                }

                unreachable;
            },
        };
    }

    fn parseSelectors(self: *Self) anyerror![]const QueryDef.Selector {
        var selectors = self.selectorPools[self.numSelectorPools];
        self.numSelectorPools += 1;

        while (!self.isEOF()) {
            const next = self.peekNextToken();
            if (next == null) {
                unreachable;
            }

            if (next == null or next.?.len == 0 or std.mem.eql(u8, next.?, "}")) {
                _ = self.nextToken();
                break;
            }

            if (isIdentCh(next.?[0])) {
                try selectors.push(try self.parseSelector());
                continue;
            }

            std.debug.print("\nencountered '{s}'\n", .{next.?});
            unreachable;
        }

        return selectors.slice();
    }

    fn parseSelector(self: *Self) !QueryDef.Selector {
        const name = self.nextToken();
        if (name == null) {
            unreachable;
        }

        const next = self.peekNextToken();
        if (next == null or next.?.len == 0) {
            unreachable;
        }

        return .{
            .field = .{
                .name = name.?,
                .children = blk: {
                    if (std.mem.eql(u8, next.?, "{")) {
                        _ = self.nextToken();
                        break :blk try self.parseSelectors();
                    }

                    break :blk &[_]QueryDef.Selector{};
                },
            },
        };
    }

    fn assertNextTokenEq(self: *Self, expected: []const u8) void {
        const next = self.nextToken();

        if (next == null) {
            std.debug.print("\nexpected '{s}'; received null\n", .{expected});
            unreachable;
        }

        if (!std.mem.eql(u8, next.?, expected)) {
            std.debug.print("\nexpected '{s}'; received '{s}'\n", .{ expected, next.? });
            unreachable;
        }
    }

    fn nextToken(self: *Self) ?[]const u8 {
        self.skipWhitespace();

        // If this is the end of input, bail.
        if (self.isEOF()) {
            return null;
        }

        // If this is an ident, return the ident.
        if (isIdentCh(self.input[self.cursor])) {
            const start = self.cursor;
            while (isIdentCh(self.input[self.cursor])) {
                self.cursor += 1;
            }

            return self.input[start..self.cursor];
        }

        // Otherwise, return the single character as the token.
        self.cursor += 1;
        return self.input[self.cursor - 1 .. self.cursor];
    }

    fn peekNextToken(self: *Self) ?[]const u8 {
        const originalCursor = self.cursor;

        const token = self.nextToken();
        self.cursor = originalCursor;

        return token;
    }

    fn isEOF(self: Self) bool {
        return self.cursor >= self.input.len or self.input[self.cursor] == 0;
    }

    fn skipWhitespace(self: *Self) void {
        while (!self.isEOF() and isWhitespace(self.input[self.cursor])) {
            self.cursor += 1;
        }
    }

    fn isWhitespace(ch: u8) bool {
        return ch == ' ' or ch == '\n' or ch == '\t';
    }

    fn isIdentCh(ch: u8) bool {
        return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z');
    }
};

test "parses user query" {
    const assert = std.debug.assert;

    var pools: [10]QueryParser.SelectorPool = undefined;
    var poolBufs: [10 * 10]QueryDef.Selector = [_]QueryDef.Selector{
        .{ .field = .{ .name = undefined, .children = undefined } },
    } ** (100);
    for (pools) |_, i| {
        pools[i] = QueryParser.SelectorPool.init(poolBufs[i .. (i + 1) * 10]);
    }

    var parser = QueryParser.init(
        \\ user {
        \\   id
        \\   name
        \\ }
        ,
        &pools,
    );

    const query = try parser.parse();

    const user = query.selector.?.field;
    assert(std.mem.eql(u8, user.name, "user"));
    assert(user.children.len == 2);

    assert(std.mem.eql(u8, user.children[0].field.name, "id"));
    assert(user.children[0].field.children.len == 0);

    assert(std.mem.eql(u8, user.children[1].field.name, "name"));
    assert(user.children[0].field.children.len == 0);
}
