
# sandbox

This subdir was created for profiling and local development of this
package




### Work in sandbox

1. Run clickhouse server with `empty password` on `default` user and provide access to `8123 http port` and `9000 native port` to `clickhouse-server` 
2. Create example table via console

    ```bash
    clickhouse-client --query "CREATE TABLE test.example (channel_name String, clientId Int64, DateTime Int64, someField2 UUID
    ) ENGINE = MergeTree PARTITION BY channel_name ORDER BY channel_name SETTINGS index_granularity = 8192"
    ```

    or run query manualy
 
    ```sql
    CREATE TABLE test.example
        ( channel_name String
        , clientId Int64
        , someField DateTime
        , someField2 UUID
        )
    ENGINE = MergeTree
    PARTITION BY channel_name
    ORDER BY channel_name
    SETTINGS index_granularity = 8192
    ```
3. Run `cabal repl sandbox` or run an executable
