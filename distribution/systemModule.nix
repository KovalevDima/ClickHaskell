{ config
, lib
, pkgs
, ...
}:

{
  options.ClickHaskell = lib.mkOption {
    type = lib.types.passwdEntry lib.types.path;
    default = "/var/lib/ClickHaskell";
  };
  config = {
    users = {
      groups.ClickHaskell = {};
      users.ClickHaskell = {
        homeMode = "755";
        isSystemUser = true;
      };
    };
  };
}