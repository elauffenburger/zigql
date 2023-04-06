main() {
    files=(
        'graphql/graphql.zig'
        'graphql/query.zig'
        'graphql/schema.zig'
    )

    for file in ${files[@]}; do
        zig test $file
    done
}

main "$@"
