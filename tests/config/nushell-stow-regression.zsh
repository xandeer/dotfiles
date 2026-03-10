#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
tmp_home="$(mktemp -d /tmp/nushell-stow-home.XXXXXX)"

cleanup() {
  rm -rf "$tmp_home"
}
trap cleanup EXIT

stow -d "$repo_root" -t "$tmp_home" config >/dev/null

library_link="$tmp_home/Library"
config_link="$tmp_home/Library/Application Support/nushell/config.nu"
env_link="$tmp_home/Library/Application Support/nushell/env.nu"
xdg_root="$tmp_home/.config/nushell"
xdg_config_link="$xdg_root/config.nu"
xdg_env_link="$xdg_root/env.nu"
modules_root="$xdg_root/modules"
repo_config="$repo_root/config/Library/Application Support/nushell/config.nu"
repo_env="$repo_root/config/Library/Application Support/nushell/env.nu"
repo_xdg_config="$repo_root/config/.config/nushell/config.nu"
repo_xdg_env="$repo_root/config/.config/nushell/env.nu"

module_names=(
  theme
  path
  fnm
  prompt
  completions
  hooks
  aliases
)

[[ -L "$library_link" ]] || {
  print -u2 "expected stow to link the Library tree at $library_link"
  exit 1
}

[[ -f "$config_link" ]] || {
  print -u2 "expected stow to expose Nushell config at $config_link"
  exit 1
}

[[ -f "$env_link" ]] || {
  print -u2 "expected stow to expose Nushell env config at $env_link"
  exit 1
}

[[ -f "$xdg_config_link" ]] || {
  print -u2 "expected stow to expose XDG Nushell config at $xdg_config_link"
  exit 1
}

[[ -f "$xdg_env_link" ]] || {
  print -u2 "expected stow to expose XDG Nushell env config at $xdg_env_link"
  exit 1
}

[[ -d "$modules_root" ]] || {
  print -u2 "expected stow to expose Nushell modules at $modules_root"
  exit 1
}

for module_name in "${module_names[@]}"; do
  [[ -f "$modules_root/$module_name.nu" ]] || {
    print -u2 "expected Nushell module $module_name.nu at $modules_root/$module_name.nu"
    exit 1
  }
done

library_target="$(readlink "$library_link")"

[[ "$library_target" == *"config/Library" ]] || {
  print -u2 "expected Library symlink to point into the config stow package, got: $library_target"
  exit 1
}

cmp -s "$repo_xdg_config" "$xdg_config_link" || {
  print -u2 "expected installed XDG Nushell config to match the repository-managed file"
  exit 1
}

cmp -s "$repo_xdg_env" "$xdg_env_link" || {
  print -u2 "expected installed XDG Nushell env config to match the repository-managed file"
  exit 1
}

grep -Fxq 'source ~/.config/nushell/config.nu' "$config_link" || {
  print -u2 "expected Library/Application Support/nushell/config.nu to forward to ~/.config/nushell/config.nu"
  exit 1
}

grep -Fxq 'source ~/.config/nushell/env.nu' "$env_link" || {
  print -u2 "expected Library/Application Support/nushell/env.nu to forward to ~/.config/nushell/env.nu"
  exit 1
}

cmp -s "$repo_config" "$config_link" || {
  print -u2 "expected installed Nushell entry config to match the repository-managed file"
  exit 1
}

cmp -s "$repo_env" "$env_link" || {
  print -u2 "expected installed Nushell entry env config to match the repository-managed file"
  exit 1
}
