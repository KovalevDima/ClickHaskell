self: {
  config,
  lib,
  pkgs,
  ...
}:
let
  user = config.ClickHaskell.user;
  group = config.ClickHaskell.group;
  dataDir = config.ClickHaskell.path;
  domain = config.ClickHaskell.domain;
  pageDir = config.ClickHaskell.pagePackage;
  server = config.ClickHaskell.serverPackage;
in
{
  options.ClickHaskell = {
    user = lib.mkOption {
      type = lib.types.str;
      default = "ClickHaskell";
      description = "The user as which to run ClickHaskell server.";
    };
    group = lib.mkOption {
      type = lib.types.str;
      default = user;
      description = "The group as which to run ClickHaskell server.";
    };
    path = lib.mkOption {
      type = lib.types.passwdEntry lib.types.path;
      default = "/var/lib/ClickHaskell";
      description = "Filepath to state directory";
    };
    domain = lib.mkOption {
      type = lib.types.str;
      default = "clickhaskell.dev";
      description = "Domain name for ClickHaskell infrastructure";
    };
    serverPackage = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.system}."ghc984-server";
    };
    pagePackage = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.system}."documentation";
    };
  };
  config = {
    services = {
      hydra = {
        enable = false;
        hydraURL = "http://localhost:3000"; # externally visible URL
        notificationSender = "hydra@localhost"; # e-mail of hydra service
        buildMachinesFiles = [];
        useSubstitutes = true;
      };
      clickhouse = {
        enable = true;
      };
    };

    users = {
      groups.ClickHaskell = {};
      users.ClickHaskell = {
        group = group;
        homeMode = "755";
        home = "${dataDir}";
        isSystemUser = true;
      };
    };

    security.acme = {
      acceptTerms = true;
      certs = {
        "${domain}" = {
          group = "ClickHaskell";
          email = "letsencrypt@${domain}";
          extraDomainNames = [ "git.${domain}" ];
        };
      };
    };
    systemd.tmpfiles.rules = [
      "d '${dataDir}' 0770 ${user} ${group} - -"
    ];
    systemd.services = {
      ClickHaskell = {
        wantedBy = [ "multi-user.target" ];
        environment = {
          CLICKHASKELL_PAGE_SOCKET_PATH = "ClickHaskell.sock";
          CLICKHASKELL_STATIC_FILES_DIR = pageDir;
        };
        serviceConfig = {
          Restart = "always";
          User = user;
          Group = group;
          ReadWritePaths = [ dataDir ];
          ExecStart = ''
            ${server + /bin/server}
          '';
          UMask= "007";
          WorkingDirectory = dataDir;
        };
        startLimitIntervalSec = 30;
        startLimitBurst = 10;
      };
    };
  };
}
