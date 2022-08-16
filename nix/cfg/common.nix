{ config, lib, pkgs, ... }:

{
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
    corkscrew

    unzip
    # tmux
    # tmuxPlugins.continuum
    # tmuxPlugins.resurrect
    # tmuxPlugins.yank
    # vim
    wget

    # iterm2

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

    ## fonts
    lmodern
    emacs-all-the-icons-fonts

    # graphviz
    # plantuml

    ## javascript & typescript
    # need for ts-ls: typescript lsp
    deno
    nodejs
    yarn

    elixir

    # android-tools
    gradle
    jre
    openjdk17
    kotlin-language-server

    # https://cloud.r-project.org/bin/macosx/
    # R

    python

    buku
  ];
}
