{ pkgs ? import <nixpkgs> {} }:

{
  xandeer = pkgs.recurseIntoAttrs {
    i3exit = pkgs.callPackage ./scripts/i3exit {};

    greenclip = pkgs.callPackage ./greenclip {};
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
