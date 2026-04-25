#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
tmp_home="$(mktemp -d /tmp/zsh-homebrew-path.XXXXXX)"
tmp_zdotdir="$(mktemp -d /tmp/zsh-zdotdir.XXXXXX)"

cleanup() {
  rm -rf "$tmp_home"
  rm -rf "$tmp_zdotdir"
}
trap cleanup EXIT

mkdir -p "$tmp_home/.config"
mkdir -p "$tmp_zdotdir/.config"
ln -s "$repo_root/profile/.config/profile.d" "$tmp_home/.config/profile.d"
ln -s "$repo_root/zsh/.config/zsh" "$tmp_zdotdir/.config/zsh"
ln -s "$repo_root/zsh/.zshenv" "$tmp_zdotdir/.zshenv"
: > "$tmp_zdotdir/.zshrc"

expected_brew=""
for candidate in /opt/homebrew/bin/brew /usr/local/bin/brew; do
  if [[ -x "$candidate" ]]; then
    expected_brew="$candidate"
    break
  fi
done

[[ -n "$expected_brew" ]] || exit 0

resolved_brew="$(
  env -i \
    HOME="$tmp_home" \
    USER="${USER:-$(id -un)}" \
    LOGNAME="${LOGNAME:-${USER:-$(id -un)}}" \
    SHELL=/bin/zsh \
    TERM=xterm-256color \
    PATH=/usr/bin:/bin:/usr/sbin:/sbin \
    ZDOTDIR="$tmp_zdotdir" \
    zsh -ic 'command -v brew || true'
)"

if [[ "$resolved_brew" != "$expected_brew" ]]; then
  print -u2 "expected non-login zsh to expose Homebrew at $expected_brew, got: ${resolved_brew:-<missing>}"
  exit 1
fi
