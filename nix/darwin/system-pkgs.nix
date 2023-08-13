{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    git
    gnumake
    corkscrew
  ];
}
