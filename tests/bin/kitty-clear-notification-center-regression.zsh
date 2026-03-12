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

rg -F 'key code 160 using control down' "$script" >/dev/null || {
  print -u2 "expected cleanup script to toggle Notification Center with the globe+n shortcut"
  exit 1
}

rg -F 'set clearLabels to {"Clear", "Clear All", "清除", "全部清除"}' "$script" >/dev/null || {
  print -u2 "expected cleanup script to search localized clear labels"
  exit 1
}

rg -F 'set dismissLabels to {"Close", "Dismiss", "关闭", "忽略"}' "$script" >/dev/null || {
  print -u2 "expected cleanup script to search localized dismiss labels"
  exit 1
}

rg -F 'if not passMadeProgress then' "$script" >/dev/null || {
  print -u2 "expected cleanup script to stop when a cleanup pass makes no progress"
  exit 1
}

rg -F 'exit repeat' "$script" >/dev/null || {
  print -u2 "expected cleanup script to break out of the cleanup loop once progress stops"
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

rg -F 'on clickFirstMatchingButton(containerRef, labelList)' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a first-match button click helper"
  exit 1
}

rg -F 'on clickButtonsByLabel(containerRef, labelList)' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a labeled button click helper"
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
