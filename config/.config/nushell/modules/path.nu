$env.PATH = ([
  $"($env.HOME)/bin"
  $"($env.HOME)/.local/bin"
  $"($env.HOME)/.nix-profile/bin"
  $"($env.HOME)/.yarn/bin"
  $"($env.HOME)/Library/Android/sdk/platform-tools"
  "/etc/profiles/per-user/kevin/bin"
  "/run/current-system/sw/bin"
  "/nix/var/nix/profiles/default/bin"
  "/opt/homebrew/bin"
  "/usr/local/bin"

  ($env.PATH | split row (char esep))
] | flatten)
