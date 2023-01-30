const std = @import("std");

const gql = @import("graphql.zig");

test "query gen" {
    const schema = try gql.Schema(@embedFile("schema.graphql"));
    // const query = try schema.query(@embedFile("query.graphql"));
}