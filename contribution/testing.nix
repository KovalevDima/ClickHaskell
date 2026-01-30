{apps, inputs, pkgs}:

let
  prevs = [null] ++ (pkgs.lib.drop 1 apps);
  pairs = pkgs.lib.zipLists prevs apps;
  cntApps = builtins.length apps + 1;
  indexes = pkgs.lib.range 1 cntApps;
  iPairs = pkgs.lib.zipListsWith (i: {fst,snd}: {num=i; fst=fst; snd=snd;}) indexes pairs;
  appProcesses =
    builtins.listToAttrs (
      pkgs.lib.forEach
        iPairs
        (spec:
          {
            name = "t${toString spec.num}";
            value = {
              command = "${spec.snd.program}";
              availability.exit_on_end = spec.num == cntApps - 1;
              depends_on =
                if spec.num == 1
                then { "database".condition = "process_healthy"; }
                else { "t${toString (spec.num - 1)}".condition = "process_completed"; };
            };
          })
      );
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
  settings.processes = appProcesses;
}
