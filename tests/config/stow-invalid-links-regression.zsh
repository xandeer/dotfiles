#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"

output="$(
  stow -nvv -d "$repo_root" -t "$HOME" config 2>&1
)"

if print -- "$output" | rg -F "replacing invalid link: projects/personal/dotfiles/config/.config/github-copilot/hosts.json" >/dev/null; then
  print -u2 "expected config stow dry-run to avoid invalid github-copilot source links"
  exit 1
fi

if print -- "$output" | rg -F "replacing invalid link: projects/personal/dotfiles/config/.config/exercism/user.json" >/dev/null; then
  print -u2 "expected config stow dry-run to avoid invalid exercism source links"
  exit 1
fi
