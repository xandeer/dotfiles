#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
script="$repo_root/config/.config/kitty/kitty-clear-notification-center"

[[ -x "$script" ]] || {
  print -u2 "expected executable notification center cleanup script at $script"
  exit 1
}

rg -F "osascript <<'APPLESCRIPT'" "$script" >/dev/null || {
  print -u2 "expected cleanup script to embed AppleScript via osascript"
  exit 1
}

rg -F 'System Events' "$script" >/dev/null || {
  print -u2 "expected cleanup script to drive Notification Center through System Events"
  exit 1
}

rg -F 'repeat with passIndex from 1 to 8' "$script" >/dev/null || {
  print -u2 "expected cleanup script to use a bounded cleanup loop"
  exit 1
}

rg -F 'on toggleNotificationCenter()' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a Notification Center toggle helper"
  exit 1
}

rg -F 'on notificationCenterWindow()' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a Notification Center window lookup helper"
  exit 1
}

temp_root="$(mktemp -d)"
trap 'rm -rf "$temp_root"' EXIT

apple_script="$temp_root/kitty-clear-notification-center.applescript"
awk '
  /<<'\''APPLESCRIPT'\''/ {collect=1; next}
  /^APPLESCRIPT$/ {if (collect) exit}
  collect {print}
' "$script" >"$apple_script"

[[ -s "$apple_script" ]] || {
  print -u2 "expected cleanup script to contain an embedded AppleScript body"
  exit 1
}

osacompile -o "$temp_root/kitty-clear-notification-center.scpt" "$apple_script" >/dev/null || {
  print -u2 "expected embedded AppleScript to compile"
  exit 1
}
