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
    # cmake
    gitAndTools.diff-so-fancy
    git-chglog
    gnumake
    mr
    stow

    unzip
    # tmux
    # tmuxPlugins.continuum
    # tmuxPlugins.resurrect
    # tmuxPlugins.yank
    # vim
    wget

    # ag
    bat
    # broot
    exa
    duf
    fd
    fzf
    htop
    # httpie
    jq
    lsof
    ncdu
    neofetch
    ripgrep
    # skim
    # starship
    tldr
    tree

    pass
    hledger

    # font-awesome
    # material-icons

    # graphviz
    # plantuml

    # xandeer.dots
    # xandeer.v2ray

    deno
    nodejs
    yarn

    # android-tools
    gradle
    jre
    kotlin-language-server

    # https://cloud.r-project.org/bin/macosx/
    # R

    buku
    # bukubrow
    # hub

    # python3
  #   (python3.withPackages(ps: with ps; [
  #     pip
  #     setuptools
  #   ]))
    # shadowfox
  ];
}
