# dev

This package was created for profiling and local development


### Work in dev

1. Run `clickhouse-server` with:
   1. `default` user with default password
   2. provided access to `8123 http port` 
   3. proviede access to `9000 native port`

   You can run it from docker image:
   ```sh
   docker run -d \
      -p 8123:8123 \
      -p 9000:9000 \
      --name clickhouse-server \
      --ulimit nofile=262144:262144 \
      clickhouse/clickhouse-server
   ```
2. Run `cabal run dev`
