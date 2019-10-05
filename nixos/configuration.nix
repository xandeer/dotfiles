{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./pkgs/packages.nix
    ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
    proxy = {
      default = "http://127.0.0.1:8010";
      noProxy = "127.0.0.1,localhost";
    };
  };

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
  sound.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
    inputMethod = {
      enabled = "fcitx";
      fcitx.engines = with pkgs.fcitx-engines; [ libpinyin ];
    };
  };

  # Set your time zone.
  time.timeZone = "Asia/Shanghai";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget unzip vim git tree tmux htop ranger w3m
    binutils gnumake cmake gcc libtool
    fzf bat prettyping lsof
    pavucontrol
    rofi
    vivaldi
    dropbox
    rustc cargo
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    videoDrivers = ["intel"];
    xkbOptions="caps:ctrl_modifier";
    libinput = {
      enable = true;
      naturalScrolling = true;
      tapping = true;
    };

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };
    windowManager = {
      default = "i3";
      i3.enable = true;
    };
    displayManager.lightdm = {
      enable = true;
      autoLogin.user = "kevin";
      autoLogin.enable = true;
    };
  };

  users.users.kevin = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "root" "audio" ];
  };

  services.v2ray = {
    enable = true;
    configFile = "/home/kevin/.local/v2ray.json";
  };

  services.urxvtd.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
