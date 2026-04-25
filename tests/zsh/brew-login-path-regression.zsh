#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
brew_login="$repo_root/zsh/.config/zsh/login/brew.zsh"

if ! rg -q '/opt/homebrew/bin/brew' "$brew_login"; then
  print -u2 "expected brew login init to support Apple Silicon Homebrew"
  exit 1
fi

if ! rg -q '/usr/local/bin/brew' "$brew_login"; then
  print -u2 "expected brew login init to support Intel Homebrew"
  exit 1
fi
