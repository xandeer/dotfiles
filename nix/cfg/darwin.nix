{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix

    ../services/v2ray-darwin.nix
  ];

  services.v2ray = {
    enable = false;
    configFile = "/etc/v2ray/client.json";
  };

  environment.systemPackages = with pkgs; [
    # emacs-plus
  ];
}
