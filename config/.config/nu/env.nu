# append below to ~/Library/Application Support/nushell/env.nu
# source ~/.config/nu/env.nu

let-env EDITOR = 'emacsclient'

# https://randomgeekery.org/config/shell/nushell/env.nu/
def-env ensure-path [new_path: string] {
  let full_path = ($new_path | path expand)
  let-env PATH = (
    if $full_path in $env.PATH { $env.PATH }
    else {
      $env.PATH | prepend $full_path
    }
  )
}

# let-env PATH = ($env.PATH | split row (char esep) | prepend '/opt/homebrew/bin')
ensure-path '/run/current-system/sw/bin'
ensure-path '/opt/homebrew/bin'
ensure-path $'($env.HOME)/bin'

# let-env NIX_PATH = $'darwin-config=/nix/var/nix/profiles/per-user/root/channels:($env.HOME)/.nix-defexpr/channels'
# let-env NIX_PATH = $'darwin-config=/nix/var/nix/profiles/per-user/root/channels'
