{ pkgs, ... }:

{
  home.packages = with pkgs; [
    stow

    kitty-themes

    # archives
    zip
    xz
    unzip
    p7zip

    # project
    git-chglog
    mr

    # utils
    asdf-vm
    bat
    buku
    # curl
    duf
    fd
    lsof
    # ncdu
    neofetch
    # tldr
    ripgrep # recursively searches directories for a regex pattern
    jq # A lightweight and flexible command-line JSON processor
    # fzf # A command-line fuzzy finder

    # aria2 # A lightweight multi-protocol & multi-source command-line download utility

    # misc
    tree
    gnutar
    gawk
    ispell
    # caddy

    (pass.withExtensions (exts: [ exts.pass-otp ]))
    gnupg
    zbar

    hledger

    # fonts
    lmodern
    (nerdfonts.override {
        fonts = [
          "FiraCode"
          # "JetBrainsMono"
          # "Iosevka"
        ];
      })

    # productivity

    # javascript & typescript
    # need for ts-ls: typescript lsp
    # deno
    # nodejs-slim-16_x
    # yarn

    # android-tools
    # gradle
    # jre
    # openjdk17
    # graalvm17-ce
    # kotlin
    # kotlin-language-server

    # clojure interpreter
    babashka
    leiningen
  ];
}
