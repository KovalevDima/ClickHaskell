{app, schemas ? [], inputs, pkgs}:
let
  programName = builtins.baseNameOf app.program;
in
{
  imports = [inputs.services-flake.processComposeModules.default];
  services.clickhouse."database" = {
    enable = true;
    extraConfig.http_port = 8123;
    initialDatabases = [ {name="default"; schemas=schemas;} ];
  }; 
  settings.processes = {
    "executable" = {
      command = "${app.program}";
      depends_on."database".condition = "process_healthy";
    };
    dump-artifacts = {
      command = "
        ${pkgs.lib.getExe' pkgs.haskellPackages.eventlog2html "eventlog2html"} ./${programName}.eventlog
        rm ./${programName}.eventlog
        rm ./${programName}.hp
      ";
      #availability.exit_on_end = true;
      depends_on."executable".condition = "process_completed_successfully";
    };
  };
}
