{apps, inputs, pkgs}:

let
  programName = app: builtins.unsafeDiscardStringContext (builtins.baseNameOf app.program);
  appProcesses =
    builtins.listToAttrs (
      pkgs.lib.forEach apps
        (app:
          {
            name = programName app;
            value = {
              command = "${app.program}";
              depends_on = {
                "database".condition = "process_healthy";
              };
              availability.restart = "exit_on_failure";
            };
          })
      );
  shutdown = {
    "shutdown" = {
      command = "echo Success";
      depends_on = builtins.listToAttrs (
        pkgs.lib.forEach apps (app:
          {
            name = "${programName app}";
            value = {condition = "process_completed_successfully";};
          }
        )
      );
      availability.exit_on_end = true;
    };
  };
in
{
  imports = [inputs.services-flake.processComposeModules.default];
  services.clickhouse."database" = {
    enable = true;
    extraConfig = {
      http_port = 8123;
      listen-host = "localhost";
      tcp_port_secure = 9440;
      logger.console = 0;
      openSSL = {
        server = {
          certificateFile = ./certs/localhost.crt;
          privateKeyFile = ./certs/localhost.key;
        };
      };
    };
    initialDatabases = [ {name="default";} ];
  }; 
  settings = {
    processes = appProcesses // shutdown;
    ordered_shutdown = true;
  };
}
