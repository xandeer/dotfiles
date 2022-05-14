{ config, lib, pkgs, ... }:

{
  fonts = {
    fontDir.enable = true;
    fonts = with pkgs; [
      xandeer.consola
      xandeer.doom-icons
      xandeer.inconsolata
      xandeer.xingkai
    ];
  };
}
