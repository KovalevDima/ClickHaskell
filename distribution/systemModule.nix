self: {
  config,
  lib,
  pkgs,
  ...
}:
let
  path = config.ClickHaskell.path;
  domain = config.ClickHaskell.domain;
  pageDir = config.ClickHaskell.pagePackage;
  server = config.ClickHaskell.serverPackage;
in
{
  options.ClickHaskell = {
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
      type = lib.types.packages;
      default = self.packages.${pkgs.system}."server";
    };
    pagePackage = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.system}."documentation";
    };
  };
  config = {
    users = {
      groups.ClickHaskell = {};
      users.ClickHaskell = {
        group = "ClickHaskell";
        homeMode = "755";
        home = "${path}";
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

    systemd.services = {
      ClickHaskell = {
        wantedBy = [ "multi-user.target" ];
        environment = {
          CLICKHASKELL_PAGE_SOCKET_PATH = "./";
          CLICKHASKELL_STATIC_FILES_DIR = pageDir;
        };
        serviceConfig = {
          Restart = "always";
          User = "ClickHaskell";
          ReadWritePaths = [ path ];
          ExecStart = ''
            ${server};
          '';
          WorkingDirectory = path;
        };
        startLimitIntervalSec = 30;
        startLimitBurst = 10;
      };
    };
  };
}
