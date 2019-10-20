{ config, lib, pkgs, ... }:

{
  users.users.kevin = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [
      "audio"
      "networkmanager"
      "root"
      "systemd-journal"
      "wheel"
      "video"
    ];
  };
}
