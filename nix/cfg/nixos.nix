{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix
    ./xserver.nix
    ./users.nix

    ../services/v2ray.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;
  networking.proxy.default = "http://127.0.0.1:8010";
  services.openssh.enable = true;

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

  environment.systemPackages = with pkgs; [
    dropbox
    emacs
    firefox
    git
    pavucontrol
    prettyping
    ranger
    rofi
    vivaldi
    imagemagick
    polybarFull
    mpd
    upower

    rustc
    cargo
    rustup
    rustracer

    xandeer.greenclip
    xandeer.i3exit
    xandeer.tdlib
    xandeer.xkeysnail
  ];

  system.stateVersion = "19.09";
}
