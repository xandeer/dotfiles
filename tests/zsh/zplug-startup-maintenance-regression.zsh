#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
plugin_loader="$repo_root/zsh/.config/zsh/plugins/zplug.zsh"

check_count="$(rg -c 'zplug check' "$plugin_loader")"
if [[ "$check_count" != "1" ]]; then
  print -u2 "expected exactly one explicit zplug check, inside the maintenance helper"
  exit 1
fi

if ! rg -q '^[[:space:]]*zplugsync\(\)' "$plugin_loader"; then
  print -u2 "expected an explicit zplugsync maintenance helper"
  exit 1
fi
