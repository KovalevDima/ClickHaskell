{app, inputs, pkgs}:
let
  programName = builtins.baseNameOf app.program;
  isPerformanceTest = pkgs.lib.hasPrefix "prof-" programName;
in
{
  imports = [inputs.services-flake.processComposeModules.default];
  services.clickhouse."database" = {
    enable = true;
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
    initialDatabases = [ {name="default";} ];
  }; 
  settings.processes = {
    "executable" = {
      command = "${app.program}";
      availability.exit_on_end = isPerformanceTest == false;
      depends_on."database".condition = "process_healthy";
    };
    dump-artifacts = pkgs.lib.mkIf isPerformanceTest {
      command = "
        ${pkgs.lib.getExe' pkgs.haskellPackages.eventlog2html "eventlog2html"} ./${programName}.eventlog
        rm ./${programName}.eventlog
        rm ./${programName}.hp
      ";
      availability.exit_on_end = true;
      depends_on."executable".condition = "process_completed_successfully";
    };
  };
}
