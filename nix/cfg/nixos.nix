{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./xserver.nix
    ./users.nix

    ../services/clash.nix
    ../services/v2ray.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;
  networking.proxy.default = "http://127.0.0.1:8010";
  services.openssh.enable = true;

  services.clash = {
    enable = true;
    configDir = "/etc/clash";
  };

  services.v2ray = {
    enable = false;
    configFile = "/etc/v2ray/client.json";
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };
  hardware.bluetooth = {
    enable = true;
    extraConfig = "
        [General]
        Enable=Source,Sink,Media,Socket
      ";
  };

  # Select internationalisation properties.
  i18n = {
    # consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8" ];
    inputMethod = {
      enabled = "fcitx";
      # fcitx.engines = with pkgs.fcitx-engines; [ libpinyin ];
    };
  };

  services.urxvtd.enable = true;

  programs.adb.enable = true;
  users.users.kevin.extraGroups = ["adbusers"];

  environment.systemPackages = with pkgs; [
    chromium
    compton
    dropbox
    emacs
    feh
    firefox
    fontmatrix
    gcc
    git
    gnupg
    pavucontrol
    prettyping
    ranger
    rofi
    vivaldi
    imagemagick
    polybarFull
    mpd
    upower
    xclip
    graphviz

    sqlite

    mu
    isync

    rustc
    cargo
    rustup
    rustracer

    nodejs
    yarn

    xandeer.clash
    xandeer.greenclip
    xandeer.i3exit
    xandeer.tdlib
    xandeer.xkeysnail
  ];

  system.stateVersion = "20.03";
}
