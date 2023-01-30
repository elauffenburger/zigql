const std = @import("std");
const TypeInfo = std.builtin.TypeInfo;

const MaxFields = 256;
const MaxTypes = 256;
const MaxIdentLen = 256;

// pub fn Schema(comptime schema: []const u8) !type {
//     const schemaInfo = try SchemaParser.init(schema).parse();

//     return struct {
//         pub fn query(comptime query: []const u8) !type {}
//     };
// }

const SchemaInfo = struct {
    const Type = struct {
        const DefTag = enum {
            @"struct",
        };

        const Def = union(DefTag) {
            @"struct": Struct,
        };

        const Struct = struct {
            const Field = struct {
                name: []const u8,
                typeName: []const u8,
            };

            fields: []Field,
        };

        name: []const u8,
        def: Def,
    };

    types: []Type,
};

const SchemaParser = struct {
    const Self = @This();

    input: []const u8,
    cursor: u32 = 0,

    types: [MaxTypes]SchemaInfo.Type,
    numTypes: u32 = 0,

    fields: [MaxFields]SchemaInfo.Type.Struct.Field,
    numFields: u32 = 0,

    fn init(schema: []const u8) Self {
        return .{
            .input = schema,
            .types = [_]SchemaInfo.Type{.{ .name = undefined, .def = undefined }} ** MaxTypes,
            .fields = std.mem.zeroes([MaxFields]SchemaInfo.Type.Struct.Field),
        };
    }

    fn parse(self: *Self) !SchemaInfo {
        while (!self.isEOF()) {
            const token = self.nextToken();
            if (token == null) {
                unreachable;
            }

            if (std.mem.eql(u8, token.?, "type")) {
                try self.parseType();
                continue;
            }

            unreachable;
        }

        return .{ .types = self.types[0..self.numTypes] };
    }

    fn parseType(self: *Self) !void {
        const typeName = self.nextToken();
        if (typeName == null) {
            return;
        }

        // Check for opening brace.
        self.assertNextTokenEq("{");

        // Read fields.
        var typ: SchemaInfo.Type = .{
            .name = typeName.?,
            .def = .{
                .@"struct" = .{
                    .fields = undefined,
                },
            },
        };

        var numFields: u32 = 0;
        while (!self.isEOF()) {
            // Get field name.
            const maybeFieldName = self.nextToken();
            if (maybeFieldName == null) {
                unreachable;
            }

            // Check if we've hit the end of the type.
            if (std.mem.eql(u8, maybeFieldName.?, "}")) {
                break;
            }

            // Check for `:`.
            self.assertNextTokenEq(":");

            // Get field type name.
            const fieldTypeName = self.nextToken();
            if (fieldTypeName == null) {
                unreachable;
            }

            // Add field to the parser field pool.
            self.fields[numFields] = .{
                .name = maybeFieldName.?,
                .typeName = fieldTypeName.?,
            };

            numFields += 1;
        }

        // Point to the block in the parser pool that we used for fields and update the pool.
        typ.def.@"struct".fields = self.fields[self.numFields .. self.numFields + numFields];
        self.numFields += numFields;

        // Add the type.
        self.types[self.numTypes] = typ;
        self.numTypes += 1;
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

test "parses user type" {
    const assert = std.debug.assert;

    var parser = SchemaParser.init(
        \\ type User {
        \\   id: Int
        \\   name: String
        \\ }
    );

    const schema = try parser.parse();
    assert(schema.types.len == 1);

    const typ = schema.types[0];
    assert(std.mem.eql(u8, typ.name, "User"));
    assert(typ.def.@"struct".fields.len == 2);

    assert(std.mem.eql(u8, typ.def.@"struct".fields[0].name, "id"));
    assert(std.mem.eql(u8, typ.def.@"struct".fields[0].typeName, "Int"));

    assert(std.mem.eql(u8, typ.def.@"struct".fields[1].name, "name"));
    assert(std.mem.eql(u8, typ.def.@"struct".fields[1].typeName, "String"));
}
