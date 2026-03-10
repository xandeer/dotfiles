#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"

source "$repo_root/zsh/.config/zsh/core/helpers.zsh"

scd() { :; }

source "$repo_root/zsh/.config/zsh/interactive/aliases.zsh"

alias_output="$(alias s 2>/dev/null || true)"
if [[ "$alias_output" != *"scd"* ]]; then
  print -u2 "expected alias s to target scd when scd is a shell function"
  exit 1
fi
