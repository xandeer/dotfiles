{ lib, ... }: {
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      # You should generally set this to the total number of logical cores in your system.
      # $ sysctl -n hw.ncpu
      max-jobs = lib.mkDefault 8;
      cores = lib.mkDefault 8;

      # Manual optimise storage: nix-store --optimise
      # https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-auto-optimise-store
      auto-optimise-store = true;
      trusted-users = [ "kevin" ];
    };

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

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
}
