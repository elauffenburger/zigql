const std = @import("std");

const schemaPkg = @import("schema.zig");
const SchemaParser = schemaPkg.SchemaParser;
const DefaultSchemaParser = schemaPkg.DefaultSchemaParser;
const SchemaDef = schemaPkg.SchemaDef;

const queryPkg = @import("query.zig");
const QueryParser = queryPkg.QueryParser;
const DefaultQueryParser = queryPkg.DefaultQueryParser;
const QueryDef = queryPkg.QueryDef;

pub fn Schema(comptime schema: []const u8) type {
    return struct {
        pub fn typeForQuery(comptime query: []const u8) !type {
            var schemaDef = blk: {
                var parser = DefaultSchemaParser(schema);
                break :blk try parser.parse();
            };

            const queryDef = blk: {
                var parser = DefaultQueryParser(query);
                break :blk try parser.parse();
            };

            var typeGenerator = TypeGenerator(schemaDef).init();
            const queryType = try typeGenerator.genTypeForSelector(queryDef.selector.?, try typeGenerator.getTypeDefFromSchema("Query"));

            return struct {
                const Query = queryType;

                query: Query,
            };
        }
    };
}

fn TypeGenerator(comptime schemaDef: SchemaDef) type {
    const TypeAndName = struct {
        t: type,
        name: []const u8,
    };

    const builtInTypes = [_]TypeAndName{
        .{ .name = "Int", .t = i32 },
        .{ .name = "String", .t = []const u8 },
    };

    return struct {
        const Self = @This();

        builtTypes: [schemaDef.types.len + builtInTypes.len]TypeAndName = builtInTypes ++ [_]TypeAndName{
            .{ .name = undefined, .t = undefined },
        } ** schemaDef.types.len,

        numBuiltTypes: u32 = builtInTypes.len,

        fn init() Self {
            return .{};
        }

        pub fn genTypeForSelector(comptime self: *Self, comptime selector: QueryDef.Selector, comptime parentType: SchemaDef.Type) !type {
            switch (selector) {
                .field => {
                    // Generate a field for each child selector in the selector.
                    var fields: [selector.field.children.len]std.builtin.Type.StructField = undefined;
                    for (selector.field.children) |child, i| {
                        switch (child) {
                            .field => {
                                // Find the type def of the selected child field.
                                const childTypeDef = childTypeDef: {
                                    switch (parentType.def) {
                                        .@"struct" => {
                                            // Find the type of the selected field in the parent type.
                                            for (parentType.def.@"struct".fields) |field| {
                                                if (!std.mem.eql(u8, field.name, child.field.name)) {
                                                    continue;
                                                }

                                                // Grab the type def from the schema.
                                                break :childTypeDef try self.getTypeDefFromSchema(field.typeName);
                                            }

                                            @compileError("unknown field " ++ child.field.name ++ " in type " ++ parentType.name);
                                        },
                                    }
                                };

                                // Generate the field.
                                fields[i] = .{
                                    .name = child.field.name,
                                    .type = try self.getOrGenType(childTypeDef),
                                    .default_value = null,
                                    .is_comptime = false,
                                    .alignment = 0,
                                };
                            },
                        }
                    }

                    // Generate the type for the selector.
                    return @Type(.{
                        .Struct = .{
                            .layout = .Auto,
                            .fields = fields[0..],
                            .decls = &[_]std.builtin.Type.Declaration{},
                            .is_tuple = false,
                        },
                    });
                },
            }
        }

        fn getOrGenType(comptime self: *Self, comptime typeDef: SchemaDef.Type) !type {
            // Check if we've already generated a type for the type def.
            for (self.builtTypes[0..self.numBuiltTypes]) |typ| {
                if (std.mem.eql(u8, typ.name, typeDef.name)) {
                    return typ.t;
                }
            }

            // Generate a new type.
            const typ = .{
                .name = typeDef.name,
                .t = @Type(typ: {
                    switch (typeDef.def) {
                        .@"struct" => {
                            const strct = typeDef.def.@"struct";

                            var fields: [strct.fields.len]std.builtin.Type.StructField = undefined;
                            for (strct.fields) |field, i| {
                                fields[i] = .{
                                    .name = field.name,
                                    .type = blk: {
                                        // Check if this is a built-in type or something we've already generated.
                                        for (self.builtTypes[0..self.numBuiltTypes]) |typ| {
                                            if (std.mem.eql(u8, typ.name, field.typeName)) {
                                                break :blk typ.t;
                                            }
                                        }

                                        break :blk try self.getOrGenType(try self.getTypeDefFromSchema(field.typeName));
                                    },
                                    .default_value = null,
                                    .is_comptime = false,
                                    .alignment = 0,
                                };
                            }

                            break :typ .{
                                .Struct = .{
                                    .layout = .Auto,
                                    .fields = fields[0..],
                                    .decls = &[_]std.builtin.Type.Declaration{},
                                    .is_tuple = false,
                                },
                            };
                        },
                    }
                }),
            };
            self.builtTypes[self.numBuiltTypes] = typ;

            return typ.t;
        }

        pub fn getTypeDefFromSchema(comptime self: Self, typeName: []const u8) !SchemaDef.Type {
            _ = self;

            for (schemaDef.types) |typ| {
                if (std.mem.eql(u8, typ.name, typeName)) {
                    return typ;
                }
            }

            @compileError("unknown type " ++ typeName ++ "\n");
        }
    };
}

test "can gen types for user query" {
    const schema = Schema(
        \\ type User {
        \\  id: Int
        \\  name: String
        \\ }
        \\
        \\ type Query {
        \\  user: User
        \\ }
    );

    const typ = try schema.typeForQuery(
        \\ query {
        \\  user {
        \\   id
        \\   name
        \\  }
        \\ }
    );

    var inst: typ = .{.query = .{.user = .{.id = 123, .name = "foo"}}};
    std.debug.print("\n{}\n", inst.query);
}
