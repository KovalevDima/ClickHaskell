{app, agent, docDirPath, inputs}:
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
        CLICKHASKELL_EVENTLOG_SOCKET_PATH="./.eventlog.sock" \
        DEV= \
        ${app.program}
      '';
      depends_on."database".condition = "process_healthy";
    };
    "agent" = {
      command = ''
      CLICKHASKELL_EVENTLOG_SOCKET_PATH="./.eventlog.sock" \
        ${agent.program}
      '';
    };
  };
}
