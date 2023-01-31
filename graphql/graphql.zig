const std = @import("std");

const schemaPkg = @import("schema.zig");
const SchemaParser = schemaPkg.SchemaParser;
const SchemaDef = schemaPkg.SchemaDef;

const queryPkg = @import("query.zig");
const QueryParser = queryPkg.QueryParser;
const QueryDef = queryPkg.QueryDef;

pub fn Schema(comptime schema: []const u8) !type {
    return struct {
        pub fn typeForQuery(comptime query: []const u8) !type {
            var schemaDef = blk: {
                const MaxTypes = 10;
                const MaxFields = 10;

                var typesBuf = [_]SchemaDef.Type{.{ .name = undefined, .def = undefined }} ** MaxTypes;
                var fieldsBuf = [_]SchemaDef.Type.Struct.Field{.{ .name = undefined, .typeName = undefined }} ** MaxFields;
                var schemaParser = SchemaParser.init(schema, &typesBuf, &fieldsBuf);

                break :blk try schemaParser.parse();
            };

            var selectorPools = blk: {
                const NumPools = 10;
                const NumSelectorsPerPool = 10;

                var pools: [NumPools]QueryParser.SelectorPool = undefined;
                var poolBufs: [NumPools * NumSelectorsPerPool]QueryDef.Selector = [_]QueryDef.Selector{
                    .{ .field = .{ .name = undefined, .children = undefined } },
                } ** (NumPools * NumSelectorsPerPool);
                for (pools) |_, i| {
                    pools[i] = QueryParser.SelectorPool.init(poolBufs[i .. (i + 1) * NumSelectorsPerPool]);
                }

                break :blk pools;
            };

            var queryParser = QueryParser.init(query, &selectorPools);
            const queryDef = try queryParser.parse();

            var typeGenerator = TypeGenerator(schemaDef).init();
            const queryType = try typeGenerator.genTypeForSelector(queryDef.selector.?, try typeGenerator.getTypeDefFromSchema("Query"));

            _ = queryType;
            // _ = queryDef;
            // _ = queryParser;
            // _ = typeGenerator;

            return struct {
                // const Query = queryType;

                // query: Query,
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
                                    .name = selector.field.name,
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
                            .fields = &fields[0..],
                            .decls = &[_]std.builtin.TypeInfo.Declaration{},
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
            self.builtTypes[self.numBuiltTypes] = .{
                .name = typeDef.name,
                .t = @Type(typ: {
                    switch (typeDef.def) {
                        .@"struct" => {
                            const strct = typeDef.def.@"struct";

                            var fields: [strct.fields.len]std.builtin.Type.StructField = undefined;
                            for (strct.fields) |field, i| {
                                fields[i] = .{
                                    .name = field.name,
                                    .@"type" = try self.getOrGenType(try self.getTypeDefFromSchema(field.typeName)),
                                    .default_value = null,
                                    .is_comptime = false,
                                    .alignment = 0,
                                };
                            }

                            break :typ .{
                                .Struct = .{
                                    .layout = .Auto,
                                    .fields = fields[0..],
                                    .decls = &[_]std.builtin.TypeInfo.Declaration{},
                                    .is_tuple = false,
                                },
                            };
                        },
                    }
                }),
            };
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
    const schema = try Schema(
        \\ type User {
        \\  id: Int
        \\  name: String
        \\ }
        \\
        \\ type Query {
        \\  user: User
        \\ }
    );

    _ = try schema.typeForQuery(
        \\ query {
        \\  user {
        \\   id
        \\   name
        \\  }
        \\ }
    );
}
