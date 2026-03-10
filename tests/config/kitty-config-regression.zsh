#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
kitty_conf="$repo_root/config/.config/kitty/kitty.conf"

[[ -f "$kitty_conf" ]] || {
  print -u2 "expected repository-managed kitty config at $kitty_conf"
  exit 1
}

if rg -F 'shell_integration no-rc' "$kitty_conf" >/dev/null; then
  print -u2 "expected kitty shell integration to use the default mode"
  exit 1
fi

rg -Fx 'hide_window_decorations titlebar-only' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to hide only the macOS title bar"
  exit 1
}

rg -Fx 'window_margin_width 6' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to use a safe window margin for titlebar-only mode"
  exit 1
}

rg -Fx 'placement_strategy center' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to center extra cell space"
  exit 1
}

config_state="$(
  kitty +runpy 'from kitty.config import load_config; import sys; opts = load_config(sys.argv[1]); print(f"allow_remote_control={opts.allow_remote_control}"); print(f"shell={opts.shell}")' "$kitty_conf"
)"

[[ "$config_state" == *'allow_remote_control=no'* ]] || {
  print -u2 "expected kitty remote control to be disabled by default"
  exit 1
}

[[ "$config_state" == *'shell=/bin/zsh -l'* ]] || {
  print -u2 "expected kitty to launch zsh as a login shell"
  exit 1
}

rg -F 'map cmd+, launch --type=background /bin/zsh -lc' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty config edit shortcut to use a background zsh launcher"
  exit 1
}

rg -F '~/.config/kitty/kitty.conf' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty config edit shortcut to target the home-relative config path"
  exit 1
}

if rg -F '/opt/homebrew/bin/emacsclient' "$kitty_conf" >/dev/null; then
  print -u2 "expected kitty config to avoid Homebrew-specific editor paths"
  exit 1
fi

if rg -F '/Users/kevin/projects/personal/dotfiles' "$kitty_conf" >/dev/null; then
  print -u2 "expected kitty config to avoid repository-specific absolute paths"
  exit 1
fi
