{app, agent, docDirPath, inputs, pkgs}:
{
  imports = [inputs.services-flake.processComposeModules.default];
  services.clickhouse."database" = {
    enable = true;
    extraConfig.http_port = 8123;
    initialDatabases = [ {name="default";} ];
  };
  settings.processes = {
    "executable" = {
      command = ''
      CLICKHASKELL_STATIC_FILES_DIR=. \
        EVENTLOG_SOCKET_PATH="./.eventlog.sock" \
        DEV= \
        ${app.program} +RTS -l-agpf --eventlog-flush-interval=1 -RTS
      '';
      depends_on."database".condition = "process_healthy";
    };
    "agent" = {
      command = ''
      sleep 3
      EVENTLOG_SOCKET_PATH="./.eventlog.sock" \
        ${agent.program}
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
          defaultDatabase = "default";
          port = "9000";
          host = "localhost";
          username = "default";
          tlsSkipVerify = "false";
        };
      }
    ];
    declarativePlugins = [
      pkgs.grafanaPlugins.grafana-clickhouse-datasource
    ];
  };
}
