{
  description = "Personal Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs }: {
    # Build darwin flake using:
    # nix build .#darwinConfiguration.Kevins-Mac-Studio.system
    # $ darwin-rebuild build --flake .#Kevins-Mac-Studio
    darwinConfigurations."Kevins-Mac-Studio" = nix-darwin.lib.darwinSystem {
      modules = [
        ./aarch64-darwin.nix
        ./studio.nix
      ];
    };

    darwinConfigurations."Kevins-Mac-Air" = nix-darwin.lib.darwinSystem {
      modules = [ ./aarch64-darwin.nix ];
    };

    # Expose the package set, including overlays, for convenience.
    # darwinPackages = self.darwinConfigurations."Kevins-Mac-Studio".pkgs;
  };
}
