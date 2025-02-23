self: {
  config,
  lib,
  pkgs,
  ...
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
  };
}
