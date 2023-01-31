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
            var schemaParser = SchemaParser.init(schema);
            const schemaDef = try schemaParser.parse();

            var typeGenerator = TypeGenerator(schemaDef).init();

            _ = typeGenerator;
            _ = query;

            // var queryParser = try QueryParser.init(query);
            // const queryDef = queryParser.parse();
            // const queryType = try typeGenerator.genTypeForSelector(schemaDef, queryDef.selector);

            return struct {
                // const Query = queryType;

                // query: Query,
            };
        }
    };
}

const FooDef = struct {
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

fn Foo(comptime schemaDef: FooDef) type {
    _ = schemaDef;

    return struct {
        const Self = @This();

        fn init() Self {
            return .{};
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

        fn genTypeForSelector(self: *Self, selector: QueryDef.Selector, parentType: SchemaDef.Type) !type {
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

                                            @compileError("genTypeForSelector");
                                        },
                                    }
                                };

                                // Generate the field.
                                fields[i] = @Type(.{
                                    .name = selector.field.name,
                                    .field_type = try self.getOrGenType(childTypeDef),
                                    .default_value = null,
                                    .is_comptime = false,
                                    .alignment = 0,
                                });
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

        fn getOrGenType(self: *Self, typeDef: SchemaDef.Type) !type {
            // Check if we've already generated a type for the type def.
            for (self.builtTypes) |typ| {
                if (std.mem.eql(u8, typ.name, typeDef.name)) {
                    return typ.t;
                }
            }

            // Generate a new type.
            self.builtTypes[self.numBuildTypes] = .{
                .name = typeDef.name,
                .t = @Type(typ: {
                    switch (typeDef.def) {
                        .@"struct" => {
                            const strct = typeDef.def.@"struct";

                            var fields: [strct.fields.len]std.builtin.Type.StructField = undefined;
                            for (strct.fields) |field, i| {
                                fields[i] = @Type(.{
                                    .name = field.name,
                                    .field_type = try self.getOrGenType(try self.getTypeDefFromSchema(field.typeName)),
                                    .default_value = null,
                                    .is_comptime = false,
                                    .alignment = 0,
                                });
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

        fn getTypeDefFromSchema(typeName: []const u8) !SchemaDef.Type {
            for (schemaDef.types) |typ| {
                if (std.mem.eql(u8, typ.name, typeName)) {
                    return typeName;
                }
            }

            @compileError("getTypeFromSchema");
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
        \\ user {
        \\   id
        \\   name
        \\ }
    );
}
