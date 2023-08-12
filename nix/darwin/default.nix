{ ... }:

{
  imports = [
    ./nix-core.nix
    ./system.nix
    ./apps.nix
  ];

  users.users.kevin.home = "/Users/kevin";
}
