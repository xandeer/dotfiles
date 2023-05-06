{ config, pkgs, ... }:

{
  imports =
    [
      ../../cfg/darwin.nix
    ];
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.bash.enable = true;
  programs.zsh.enable = true;
  # programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings.max-jobs = 4;
  nix.settings.cores = 4;

  # https://mirrors.ustc.edu.cn/help/nix-channels.html
  nix.settings.substituters = [ "https://mirrors.ustc.edu.cn/nix-channels/store" ];

  nixpkgs.config.allowBroken = true;
}
