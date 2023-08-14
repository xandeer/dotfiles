{ config, ... }: {
  imports = [
    ./nix-core.nix
    ./system.nix
    ./system-pkgs.nix
    ./homebrew.nix
  ];

  users.users.kevin.home = "/Users/kevin";

  environment = {
    variables.EDITOR = "emacsclient";

    systemPath = [
      "$HOME/bin"
      "$HOME/.yarn/bin"
      "$HOME/Library/Android/sdk/platform-tools"
      "/etc/profiles/per-user/kevin/bin"
    ];
  };
}
