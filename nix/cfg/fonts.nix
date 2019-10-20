{ config, lib, pkgs, ... }:

{
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      xandeer.consola
      xandeer.doom-icons
      xandeer.inconsolata
      xandeer.xingkai
    ];
  };
}
