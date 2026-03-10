brew_bin=""

for candidate in /opt/homebrew/bin/brew /usr/local/bin/brew; do
  if [[ -x "$candidate" ]]; then
    brew_bin="$candidate"
    break
  fi
done

if [[ -z "$brew_bin" ]] && zsh_has_command brew; then
  brew_bin="$commands[brew]"
fi

if [[ -n "$brew_bin" ]]; then
  eval "$("$brew_bin" shellenv)"

  brew_wrap="$("$brew_bin" --prefix)/etc/brew-wrap"
  if [ -f "$brew_wrap" ]; then
    source "$brew_wrap"
  fi
  unset brew_wrap
fi

unset brew_bin
unset candidate
