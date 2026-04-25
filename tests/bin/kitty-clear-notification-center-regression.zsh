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

if rg -F 'key code 160 using control down' "$script" >/dev/null; then
  print -u2 "expected cleanup script to avoid the broken global control+160 key event"
  exit 1
fi

rg -F 'set toggleLabels to {"Clock", "Date & Time", "Date and Time", "时间", "日期", "时钟"}' "$script" >/dev/null || {
  print -u2 "expected cleanup script to search menu bar clock labels when opening Notification Center"
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

rg -F 'on notificationCenterToggleMenuBarItem()' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a menu bar lookup helper for Notification Center"
  exit 1
}

rg -F 'on openNotificationCenter()' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define an open helper for Notification Center"
  exit 1
}

rg -F 'on closeNotificationCenter()' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a close helper for Notification Center"
  exit 1
}

rg -F 'menu bar items of menu bar 1' "$script" >/dev/null || {
  print -u2 "expected cleanup script to use menu bar UI scripting instead of a synthetic global key event"
  exit 1
}

rg -F 'on notificationCenterWindow()' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a Notification Center window lookup helper"
  exit 1
}

rg -F 'on clickFirstMatchingButton(labelList)' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a first-match button click helper that resolves Notification Center lazily"
  exit 1
}

rg -F 'on clickButtonsByLabel(labelList)' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a labeled button click helper that resolves Notification Center lazily"
  exit 1
}

rg -F 'on clickTopmostNotificationCloseButton()' "$script" >/dev/null || {
  print -u2 "expected cleanup script to define a geometry-based fallback for unlabeled notification close buttons without passing stale window references"
  exit 1
}

rg -F 'on error' "$script" >/dev/null || {
  print -u2 "expected cleanup script to guard Notification Center UI lookups against stale accessibility references"
  exit 1
}

rg -F 'set notificationWindow to my notificationCenterWindow()' "$script" >/dev/null || {
  print -u2 "expected cleanup helpers to resolve the current Notification Center window on demand"
  exit 1
}

rg -F 'set widgetBoundaryY to windowTop + 600' "$script" >/dev/null || {
  print -u2 "expected cleanup script to limit fallback close-button clicks to the upper notification region"
  exit 1
}

rg -F 'if elementWidth < 18 or elementHeight < 18 then' "$script" >/dev/null || {
  print -u2 "expected cleanup script to ignore tiny widget buttons when searching for notification close controls"
  exit 1
}

rg -F 'if my clickTopmostNotificationCloseButton() then' "$script" >/dev/null || {
  print -u2 "expected cleanup script to fall back to unlabeled close buttons when text labels are unavailable"
  exit 1
}

rg -F 'set closeSettleDelaySeconds to 0.8' "$script" >/dev/null || {
  print -u2 "expected cleanup script to wait before closing Notification Center so a dismissed stacked notification does not reappear"
  exit 1
}

rg -F 'if didClearNotification then' "$script" >/dev/null || {
  print -u2 "expected cleanup script to only apply the close settle delay when it actually dismissed something"
  exit 1
}

geometry_line="$(rg -n -F 'if my clickTopmostNotificationCloseButton() then' "$script" | cut -d: -f1)"
label_line="$(rg -n -F 'else if my clickButtonsByLabel(clearLabels) then' "$script" | cut -d: -f1)"

[[ -n "$geometry_line" && -n "$label_line" && "$geometry_line" -lt "$label_line" ]] || {
  print -u2 "expected cleanup script to try the proven geometry-based close button before scanning for text labels"
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
