{ ... }: {
  imports = [
    ./nix-core.nix
    ./system.nix
    ./system-pkgs.nix
    ./homebrew.nix
  ];

  users.users.kevin.home = "/Users/kevin";
  environment.variables.EDITOR = "/opt/homebrew/bin/emacsclient";
}
