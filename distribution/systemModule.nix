{ config
, lib
, ...
}:
let
  path = config.ClickHaskell.path;
  domain = config.ClickHaskell.domain;
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
  };
  config = {
    users = {
      groups.ClickHaskell = {};
      users.ClickHaskell = {
        homeMode = "755";
        isSystemUser = true;
      };
    };

    security.acme = {
      acceptTerms = true;
      certs = {
        "${domain}" = {
          group = "ClickHaskell";
          email = "letsencrypt@${domain}";
          directory = "${path}/certs";
        };
      };
    };
  };
}
