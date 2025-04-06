{app, docDirPath, schemas ? [], inputs}:
{
  imports = [inputs.services-flake.processComposeModules.default];
  services.clickhouse."database" = {
    enable = true;
    extraConfig.http_port = 8123;
    initialDatabases = [ {name="default"; schemas=schemas;} ];
  };
  settings.processes = {
    "executable" = {
      command = ''
      CLICKHASKELL_STATIC_FILES_DIR=. \
        DEV= \
        ${app.program}
      '';
      depends_on."database".condition = "process_healthy";
    };
  };
}
