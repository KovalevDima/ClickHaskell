
# sandbox

This subdir was created for profiling and local development of this
package




### Work in sandbox

1. Run clickhouse server with `empty password` on `default` user
and provide access to `8123 http port` and `9000 native port` to `clickhouse-server`

   You can setup this from docker image:
   ```sh
   docker run -d -p 8123:8123 -p 9000:9000 --name clickhouse-server --ulimit nofile=262144:262144 clickhouse/clickhouse-server
   ```
2. Run `cabal run sandbox`
