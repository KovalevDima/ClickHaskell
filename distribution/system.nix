{ inputs
, self
, systemModules
, homeModules
, disko
, ...
}:
{
  system = "x86_64-linux";
  specialArgs = {inherit inputs;};
  modules = [
    disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    (
      {inputs, config, pkgs, modulesPath, ...}:{
        imports = systemModules ++ [
          (modulesPath + "/installer/scan/not-detected.nix")
        ];

        services.nginx = {
          enable = true;
          virtualHosts."clickhaskell.dev" = {
            enableACME = true;
            forceSSL = true;
            root = "${self.packages.x86_64-linux."documentation"}";
          };
        };
        
        users.users.nginx.extraGroups = [ "acme" ];
        security.acme = {
          acceptTerms = true;
          certs = {
            "clickhaskell.dev" = {group = "acme"; email = "letsencrypt@clickhaskell.dev";};
          };
        };

        networking = {
          hostName = "v279107";
          firewall.allowedTCPPorts = [ 80 443 ];
        };
        services.openssh = {
          enable = true;
          ports = [22];
          settings.AllowUsers = null;
        };

        users.users.root = { #
          initialHashedPassword = "$y$j9T$pK7pBEwosznnhVa9bENuL1$xksm19v2ut7hXCaSbAPTf0CxjtipPK4.AbYqY48r4O5";
          openssh.authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAMRId+WDlD6u83HZx62o0PrCS0aZSnSJT5kXbKI9CaV dmitry@desktop"
          ];
        };
        users.users.ClickHaskell = {
          isNormalUser = true;
          description = "ClickHaskell maintainers";
          extraGroups = [ "wheel" ];
        };
        environment.systemPackages = [];
        boot.loader.grub = {
          efiSupport = true;
          efiInstallAsRemovable = true;
        };

        i18n.defaultLocale = "en_US.UTF-8";
        time.timeZone = "Etc/UTC";
        system.stateVersion = "24.11"; # Did you read the comment?
      }
    )
    (
      {inputs, config, pkgs, ...} : {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = {inherit inputs;};
          users.ClickHaskell = {
            imports = homeModules;
            home = {
              homeDirectory = "/home/ClickHaskell";
              stateVersion = "24.11";
              packages = [];
            };
          };
        };
      }
    )
  ];
}
