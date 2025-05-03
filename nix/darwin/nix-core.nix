{ lib, ... }: {
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      # You should generally set this to the total number of logical cores in your system.
      # $ sysctl -n hw.ncpu
      max-jobs = lib.mkDefault 8;
      cores = lib.mkDefault 8;

      trusted-users = [ "kevin" ];
    };

    optimise.automatic = true;

    gc = {
      automatic = true;
      interval.Day = 7;
      options = "--delete-older-than 7d";
    };
  };

  nixpkgs = {
    hostPlatform = "aarch64-darwin";
    config = {
      allowBroken = true;
      allowUnfree = true;
      permittedInsecurePackages = [
        "nodejs-slim-16.20.2"
      ];
    };
  };

  programs.nix-index.enable = true;
  programs.direnv.enable = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  system.stateVersion = 5;
}
