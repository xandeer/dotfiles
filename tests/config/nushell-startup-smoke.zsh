#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
original_home="${HOME:A}"
tmp_home="$(mktemp -d /tmp/nushell-startup-home.XXXXXX)"

cleanup() {
  rm -rf "$tmp_home"
}
trap cleanup EXIT

mkdir -p "$tmp_home/.config" "$tmp_home/projects/others"

if [[ -f "$original_home/.config/env.nu" ]]; then
  ln -s "$original_home/.config/env.nu" "$tmp_home/.config/env.nu"
else
  print 'load-env {}' > "$tmp_home/.config/env.nu"
fi

[[ -d "$original_home/projects/others/nu_scripts" ]] || {
  print -u2 "expected nu_scripts at $original_home/projects/others/nu_scripts"
  exit 1
}

ln -s "$original_home/projects/others/nu_scripts" "$tmp_home/projects/others/nu_scripts"

HOME="$tmp_home" nu \
  --config "$repo_root/config/.config/nushell/config.nu" \
  --env-config "$repo_root/config/.config/nushell/env.nu" \
  -c 'version | get version' >/dev/null
