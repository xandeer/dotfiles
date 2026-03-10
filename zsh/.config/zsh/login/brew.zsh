if [[ -x /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"

  brew_wrap="$(brew --prefix)/etc/brew-wrap"
  if [ -f "$brew_wrap" ]; then
    source "$brew_wrap"
  fi
  unset brew_wrap
fi
