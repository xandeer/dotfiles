{ pkgs ? import <nixpkgs> {} }:

{
  xandeer = pkgs.recurseIntoAttrs {
    dots = pkgs.callPackage ./scripts/dots {};
    i3exit = pkgs.callPackage ./scripts/i3exit {};

    clash = pkgs.callPackage ./clash {};
    greenclip = pkgs.callPackage ./greenclip {};
    tdlib = pkgs.callPackage ./tdlib {};
    v2ray = pkgs.callPackage ./v2ray {};
    xkeysnail = pkgs.callPackage ./xkeysnail {
        inherit (pkgs.python36Packages)
            buildPythonPackage
            evdev
            fetchPypi
            inotify-simple
            xlib;
    };

    consola = pkgs.callPackage ./fonts/consola {};
    doom-icons = pkgs.callPackage ./fonts/doom-icons {};
    inconsolata = pkgs.callPackage ./fonts/inconsolata {};
    xingkai = pkgs.callPackage ./fonts/xingkai {};
  };
}
