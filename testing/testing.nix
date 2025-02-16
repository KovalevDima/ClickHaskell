{inputs, app, schemas}:
{
  imports = [inputs.services-flake.processComposeModules.default];
  settings.processes.integration-test = {
    command = "${app.program}";
    availability.exit_on_end = true;
    depends_on.testing-db.condition = "process_healthy";
  };
  services.clickhouse."testing-db" = {
    enable = true;
    extraConfig.http_port = 8123;
    initialDatabases = [ {name="default";schemas=schemas;} ];
  };
}
