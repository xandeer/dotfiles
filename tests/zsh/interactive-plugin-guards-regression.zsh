#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
interactive_dir="$repo_root/zsh/.config/zsh/interactive"

if rg -q 'zplug check "' "$interactive_dir"; then
  print -u2 "interactive config should not call zplug check after plugin load"
  exit 1
fi

if rg -q 'anyframe' "$interactive_dir"; then
  print -u2 "dead anyframe configuration should be removed from interactive config"
  exit 1
fi
