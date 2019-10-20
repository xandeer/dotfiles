{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ../../cfg/nixos.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    timeout = 1;
  };

  networking.hostName = "nixos-14";
}
