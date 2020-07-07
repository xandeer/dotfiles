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

  environment.systemPackages = with pkgs; [
    cmake
    gitAndTools.diff-so-fancy
    gnumake
    mr
    stow

    unzip
    tmux
    tmuxPlugins.continuum
    tmuxPlugins.resurrect
    tmuxPlugins.yank
    vim
    wget

    bat
    fzf
    htop
    lsof
    neofetch
    ripgrep
    tree

    pass

    font-awesome
    material-icons

    graphviz
    plantuml

    xandeer.dots
    # xandeer.v2ray

    openjdk8
    python3
  #   (python3.withPackages(ps: with ps; [
  #     pip
  #     setuptools
  #   ]))
    shadowfox
  ];
}
