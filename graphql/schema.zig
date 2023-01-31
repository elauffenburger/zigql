const std = @import("std");
const TypeInfo = std.builtin.TypeInfo;

const MaxFields = 256;
const MaxTypes = 256;
const MaxIdentLen = 256;

pub const SchemaDef = struct {
    pub const Type = struct {
        pub const DefTag = enum {
            @"struct",
        };

        pub const Def = union(DefTag) {
            @"struct": Struct,
        };

        pub const Struct = struct {
            pub const Field = struct {
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

pub const SchemaParser = struct {
    const Self = @This();

    input: []const u8,
    cursor: u32 = 0,

    types: []SchemaDef.Type,
    numTypes: u32 = 0,

    fields: []SchemaDef.Type.Struct.Field,
    numFields: u32 = 0,

    pub fn init(schema: []const u8, typesBuf: []SchemaDef.Type, fieldsBuf: []SchemaDef.Type.Struct.Field) Self {
        return .{
            .input = schema,
            .types = typesBuf,
            .fields = fieldsBuf,
            // .types = [_]SchemaDef.Type{.{ .name = undefined, .def = undefined }} ** MaxTypes,
            // .fields = std.mem.zeroes([MaxFields]SchemaDef.Type.Struct.Field),
        };
    }

    pub fn parse(self: *Self) !SchemaDef {
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
        var typ: SchemaDef.Type = .{
            .name = typeName.?,
            .def = .{
                .@"struct" = .{
                    .fields = undefined,
                },
            },
        };

        const fieldsStart = self.numFields;
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
            self.fields[self.numFields] = .{
                .name = maybeFieldName.?,
                .typeName = fieldTypeName.?,
            };

            self.numFields += 1;
        }

        // Point to the block in the parser pool that we used for fields.
        typ.def.@"struct".fields = self.fields[fieldsStart..self.numFields];

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

    var typesBuf = [_]SchemaDef.Type{.{ .name = undefined, .def = undefined }} ** 10;
    var fieldsBuf = [_]SchemaDef.Type.Struct.Field{.{ .name = undefined, .typeName = undefined }} ** 10;

    var parser = SchemaParser.init(
        \\ type User {
        \\  id: Int
        \\  name: String
        \\ }
        \\
        \\ type Query {
        \\  user: User
        \\ }
        ,
        &typesBuf,
        &fieldsBuf,
    );

    const schema = try parser.parse();
    assert(schema.types.len == 2);

    const user = schema.types[0];
    assert(std.mem.eql(u8, user.name, "User"));
    assert(user.def.@"struct".fields.len == 2);

    assert(std.mem.eql(u8, user.def.@"struct".fields[0].name, "id"));
    assert(std.mem.eql(u8, user.def.@"struct".fields[0].typeName, "Int"));

    assert(std.mem.eql(u8, user.def.@"struct".fields[1].name, "name"));
    assert(std.mem.eql(u8, user.def.@"struct".fields[1].typeName, "String"));

    const query = schema.types[1];
    assert(std.mem.eql(u8, query.name, "Query"));
    assert(query.def.@"struct".fields.len == 1);

    assert(std.mem.eql(u8, query.def.@"struct".fields[0].name, "user"));
    assert(std.mem.eql(u8, query.def.@"struct".fields[0].typeName, "User"));
}
