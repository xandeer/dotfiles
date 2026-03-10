source modules/theme.nu
source modules/path.nu
source modules/fnm.nu

$env.NIXPKGS_ALLOW_INSECURE = 1

def nuopen [arg, --raw (-r)] {
  if $raw { open -r $arg } else { open $arg }
}

source modules/prompt.nu
source ~/.cache/zoxide/init.nu
source ~/.cache/oh-my-posh/init.nu
source modules/completions.nu
source modules/hooks.nu

source ~/.config/env.nu

source modules/aliases.nu
