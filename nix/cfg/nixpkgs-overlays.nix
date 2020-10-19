[(self: super:
let unstable = builtins.fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz; in {
  # emacs = (import unstable { }).emacs;
  emacs26 = super.emacs26.override {
    imagemagick = self.imagemagickBig;
  };
  emacs27 = (import unstable { }).emacs27.override {
    imagemagick = self.imagemagickBig;
  };
})]
