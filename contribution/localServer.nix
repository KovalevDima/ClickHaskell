{app, agent, docDirPath, inputs, pkgs}:
{
  imports = [inputs.services-flake.processComposeModules.default];
  services.clickhouse."database" = {
    enable = true;
    initialDatabases = [ {name="default";} ];
    extraConfig = {
      http_port = 8123;
      listen-host = "localhost";
      tcp_port_secure = 9440;
      openSSL = {
        server = {
          certificateFile = ./certs/localhost.crt;
          privateKeyFile = ./certs/localhost.key;
        };
      };
    };
  };
  settings.processes = {
    "executable" = {
      command = ''
      CLICKHASKELL_STATIC_FILES_DIR=./documentation/ \
        EVENTLOG_SOCKET_PATH="./data/.eventlog.sock" \
        DEV= \
        cabal run server -- +RTS -l-agpfsuT --eventlog-flush-interval=1 -RTS
      '';
      depends_on."database".condition = "process_healthy";
    };
    "agent" = {
      command = ''
      sleep 3
      EVENTLOG_SOCKET_PATH="./data/.eventlog.sock" \
        cabal run eventlog-agent
      '';
      depends_on."executable".condition = "process_started";
    };
    "ui" = {
      command = ''
        npm run dev
      '';
      depends_on."executable".condition = "process_started";
    };
  };
  services.grafana."grafana" = {
    enable = true;
    http_port = 8080;
    datasources = [
      {
        name = "ClickHouse";
        type = "grafana-clickhouse-datasource";
        jsonData = {
          port = "9000";
          host = "localhost";
        };
      }
    ];
    declarativePlugins = [
      pkgs.grafanaPlugins.grafana-clickhouse-datasource
    ];
  };
}
