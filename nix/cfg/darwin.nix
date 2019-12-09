{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix

    # ../services/v2ray-darwin.nix
  ];

  environment.systemPackages = with pkgs; [
    # emacs-plus
  ];
}
