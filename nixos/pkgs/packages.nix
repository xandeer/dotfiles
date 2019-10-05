{ config, pkgs, ... }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // pkgs.xlibs);
in
{
  imports = [
    ./v2ray/service.nix
  ];

  nixpkgs.config.packageOverrides = super: {
    v2ray = callPackage ./v2ray {};
    tdlib = callPackage ./tdlib {};
    greenclip = callPackage ./greenclip {};
    scripts = {
      dots = callPackage ./scripts/dots {};
      i3exit = callPackage ./scripts/i3exit {};
    };
  };

  nixpkgs.overlays = [(self: super: {
    emacs26 = super.emacs26.override {
      imagemagick = self.imagemagickBig;
    };
  })];

  environment.systemPackages = with pkgs; [
    scripts.dots
    scripts.i3exit
    v2ray
    greenclip
    emacs tdlib
  ];
}
