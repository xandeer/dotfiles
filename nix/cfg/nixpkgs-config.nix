{
  allowUnfree = true;

  packageOverrides = pkgs:
    import ../pkgs/default.nix { inherit pkgs;  };
}
