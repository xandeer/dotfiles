{ config, lib, pkgs, ... }:

{
  imports = [
    ./fonts.nix
  ];

  nixpkgs = {
    config = import ./nixpkgs-config.nix;
    overlays = import ./nixpkgs-overlays.nix;
  };

  time.timeZone = "Asia/Shanghai";

  services.v2ray = {
    enable = true;
    configFile = "/etc/v2ray/client.json";
  };

  environment.systemPackages = with pkgs; [
    cmake
    gitAndTools.diff-so-fancy
    gcc
    gnumake
    mr
    stow

    unzip
    tmux
    tmuxPlugins.yank
    vim
    wget

    bat
    fzf
    htop
    lsof
    ripgrep
    tree

    font-awesome
    material-icons

    plantuml

    xandeer.dots
    xandeer.v2ray

    openjdk8
    python3
  #   (python3.withPackages(ps: with ps; [
  #     pip
  #     setuptools
  #   ]))
    shadowfox
  ];
}
