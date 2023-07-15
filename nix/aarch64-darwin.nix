{ config, pkgs, lib, ... }:

{
  environment.systemPackages = import ./pkgs.nix { inherit pkgs;  };

  nix = {
    settings = {
      experimental-features = "nix-command flakes";

      # https://mirrors.ustc.edu.cn/help/nix-channels.html
      substituters = [ "https://mirrors.ustc.edu.cn/nix-channels/store" ];

      # You should generally set this to the total number of logical cores in your system.
      # $ sysctl -n hw.ncpu
      max-jobs = lib.mkDefault 8;
      cores = lib.mkDefault 8;
    };

    gc = {
      automatic = true;
      interval.Day = 7;
      options = "--delete-older-than 7d";
    };
  };

  time.timeZone = "Asia/Shanghai";

  nixpkgs = {
    hostPlatform = "aarch64-darwin";
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  # programs.zsh.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
