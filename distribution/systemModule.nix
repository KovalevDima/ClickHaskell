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
      default = "btcpayserver";
      description = "The user as which to run btcpayserver.";
    };
    group = lib.mkOption {
      type = lib.types.str;
      default = user;
      description = "The group as which to run btcpayserver.";
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
      default = self.packages.${pkgs.system}."ghc966-server";
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
          CLICKHASKELL_PAGE_SOCKET_PATH = "./";
          CLICKHASKELL_STATIC_FILES_DIR = pageDir;
        };
        serviceConfig = {
          Restart = "always";
          User = "ClickHaskell";
          ReadWritePaths = [ dataDir ];
          ExecStart = ''
            ${server};
          '';
          WorkingDirectory = dataDir;
        };
        startLimitIntervalSec = 30;
        startLimitBurst = 10;
      };
    };
  };
}
