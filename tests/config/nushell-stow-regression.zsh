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
repo_config="$repo_root/config/Library/Application Support/nushell/config.nu"
repo_env="$repo_root/config/Library/Application Support/nushell/env.nu"

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

library_target="$(readlink "$library_link")"

[[ "$library_target" == *"config/Library" ]] || {
  print -u2 "expected Library symlink to point into the config stow package, got: $library_target"
  exit 1
}

cmp -s "$repo_config" "$config_link" || {
  print -u2 "expected installed Nushell config to match the repository-managed file"
  exit 1
}

cmp -s "$repo_env" "$env_link" || {
  print -u2 "expected installed Nushell env config to match the repository-managed file"
  exit 1
}
