{
  description = "Personal Darwin system flake";

  nixConfig = {
    experimental-features = [ "nix-command" "flakes" ];
    substituters = [
      # replace official cache with a mirror located in China
      "https://mirrors.ustc.edu.cn/nix-channels/store"
      "https://cache.nixos.org/"
    ];

    # nix community's cache server
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    # also use `nix register` to change user flake register in `~/.config/nix/registry.json`:
    # nix registry add nixpkgs github:nixos/nixpkgs/nixpkgs-23.05-darwin
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";

    # home-manager, used for managing user configuration
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      # The `follows` keyword in inputs is used for inheritance.
      # Here, `inputs.nixpkgs` of home-manager is kept consistent with the `inputs.nixpkgs` of the current flake,
      # to avoid problems caused by different versions of nixpkgs dependencies.
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, darwin, home-manager, ... }: {
    # Build darwin flake using:
    # $ nix run nix-darwin -- switch --flake ~/projects/personal/dotfiles/nix/
    # After the first installing:
    # $ darwin-rebuild switch --flake ~/projects/personal/dotfiles/nix/
    darwinConfigurations."Kevins-Mac-Studio" = darwin.lib.darwinSystem {
      modules = [
        ./darwin
        ./studio.nix

        # home manager
        home-manager.darwinModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = inputs;
            users.kevin = import ./home;
          };
        }
      ];
    };

    # $ hostname -s
    darwinConfigurations."Kevins-MacBook-Air" = darwin.lib.darwinSystem {
      modules = [
        ./darwin

        # home manager
        home-manager.darwinModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = inputs;
            users.kevin = import ./home;
          };
        }
      ];
    };

    # Expose the package set, including overlays, for convenience.
    # darwinPackages = self.darwinConfigurations."Kevins-Mac-Studio".pkgs;
  };
}
