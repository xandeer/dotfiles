{ pkgs, ... }:

with pkgs; [
  # cmake
  gitAndTools.diff-so-fancy
  git-chglog
  gnumake
  mr
  stow
  corkscrew

  unrar
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
  enchant2

  (pass.withExtensions (exts: [ exts.pass-otp ]))
  hledger

  ## fonts
  lmodern
  # emacs-all-the-icons-fonts

  # graphviz
  # plantuml

  ## javascript & typescript
  # need for ts-ls: typescript lsp
  deno
  nodejs
  yarn

  # elixir

  # android-tools
  gradle
  jre
  # openjdk17
  # graalvm17-ce
  kotlin-language-server

  # clojure interpreter
  babashka

  leiningen

  # https://cloud.r-project.org/bin/macosx/
  # R

  # python39
  # openai

  buku
  ispell
  caddy
]
