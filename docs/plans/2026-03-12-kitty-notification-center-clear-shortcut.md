# Kitty Notification Center Clear Shortcut Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `ctrl+q>c` `kitty` shortcut that launches a repository-managed macOS Notification Center cleanup script and attempts to clear all currently visible notifications with best-effort UI automation.

**Architecture:** Keep the shortcut wiring in `kitty.conf` and move the automation into a standalone shell wrapper at `config/.config/kitty/kitty-clear-notification-center`. The wrapper embeds an AppleScript program that uses `System Events` to open Notification Center, repeatedly click clear controls or per-notification dismiss controls, and then close Notification Center; regression tests verify both the key mapping and the script structure without depending on a live GUI session.

**Tech Stack:** kitty config, zsh, osascript, AppleScript UI scripting, shell regression tests, `osacompile`

---

### Task 1: Wire the shortcut into kitty with a failing config regression first

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`
- Modify: `config/.config/kitty/kitty.conf`
- Create: `config/.config/kitty/kitty-clear-notification-center`

**Step 1: Write the failing test**

Update `tests/config/kitty-config-regression.zsh` to require:

```zsh
kitty_clear_notifications="$repo_root/config/.config/kitty/kitty-clear-notification-center"

[[ -f "$kitty_clear_notifications" ]] || {
  print -u2 "expected repository-managed kitty notification center cleanup script at $kitty_clear_notifications"
  exit 1
}

[[ -x "$kitty_clear_notifications" ]] || {
  print -u2 "expected kitty notification center cleanup script to be executable"
  exit 1
}

rg -Fx "map ctrl+q>c launch --type=background /bin/zsh -lc 'exec ~/.config/kitty/kitty-clear-notification-center'" "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to map ctrl+q>c to the notification center cleanup shortcut"
  exit 1
}
```

**Step 2: Run test to verify it fails**

Run: `zsh tests/config/kitty-config-regression.zsh`
Expected: FAIL because the new script file and shortcut mapping do not exist yet.

**Step 3: Write minimal implementation**

- Add the `map ctrl+q>c ...` line to `config/.config/kitty/kitty.conf`
- Create `config/.config/kitty/kitty-clear-notification-center` as an executable shell script stub that exits successfully for now

Use this minimal stub first:

```zsh
#!/bin/zsh

set -eu

exit 0
```

**Step 4: Run test to verify it passes**

Run: `zsh tests/config/kitty-config-regression.zsh`
Expected: PASS

**Step 5: Commit**

```bash
git add tests/config/kitty-config-regression.zsh config/.config/kitty/kitty.conf config/.config/kitty/kitty-clear-notification-center
git commit -m "feat: add kitty notification clear shortcut wiring"
```

### Task 2: Add a failing regression for the cleanup script structure

**Files:**
- Create: `tests/bin/kitty-clear-notification-center-regression.zsh`
- Modify: `config/.config/kitty/kitty-clear-notification-center`

**Step 1: Write the failing test**

Create `tests/bin/kitty-clear-notification-center-regression.zsh` to assert that the script:

- exists and is executable
- shells out to `osascript`
- references `System Events`
- contains a bounded cleanup loop
- toggles Notification Center open and closed
- embeds AppleScript in a heredoc that can be extracted and compiled with `osacompile`

Use a test structure like:

```zsh
#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
script="$repo_root/config/.config/kitty/kitty-clear-notification-center"

[[ -x "$script" ]] || {
  print -u2 "expected executable notification center cleanup script at $script"
  exit 1
}

rg -F 'osascript <<' "$script" >/dev/null || {
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

temp_root="$(mktemp -d)"
trap 'rm -rf "$temp_root"' EXIT

apple_script="$temp_root/kitty-clear-notification-center.applescript"
python3 - "$script" "$apple_script" <<'PY'
from pathlib import Path
import sys

script = Path(sys.argv[1]).read_text(encoding="utf-8").splitlines()
out_path = Path(sys.argv[2])
collect = False
lines = []
for line in script:
    if line == "APPLESCRIPT":
        if collect:
            break
        continue
    if collect:
        lines.append(line)
    elif line.endswith("<<'APPLESCRIPT'"):
        collect = True
out_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
PY

osacompile -o "$temp_root/kitty-clear-notification-center.scpt" "$apple_script" >/dev/null || {
  print -u2 "expected embedded AppleScript to compile"
  exit 1
}
```

**Step 2: Run test to verify it fails**

Run: `zsh tests/bin/kitty-clear-notification-center-regression.zsh`
Expected: FAIL because the stub script does not contain any AppleScript yet.

**Step 3: Write minimal implementation**

Replace the stub with a shell wrapper that embeds AppleScript in a heredoc and executes:

```zsh
#!/bin/zsh

set -eu

osascript <<'APPLESCRIPT'
on run
  tell application "System Events"
    return UI elements enabled
  end tell
end run
APPLESCRIPT
```

Then extend it until the regression passes, but keep the shell wrapper responsible only for launching the AppleScript.

**Step 4: Run test to verify it passes**

Run: `zsh tests/bin/kitty-clear-notification-center-regression.zsh`
Expected: PASS

**Step 5: Commit**

```bash
git add tests/bin/kitty-clear-notification-center-regression.zsh config/.config/kitty/kitty-clear-notification-center
git commit -m "test: cover kitty notification center cleanup script"
```

### Task 3: Implement the best-effort cleanup behavior inside the AppleScript

**Files:**
- Modify: `config/.config/kitty/kitty-clear-notification-center`
- Modify: `tests/bin/kitty-clear-notification-center-regression.zsh`

**Step 1: Write the failing test**

Tighten `tests/bin/kitty-clear-notification-center-regression.zsh` so it now requires these concrete behaviors to be present in the script source:

```zsh
rg -F 'key code 160' "$script" >/dev/null || {
  print -u2 "expected cleanup script to toggle Notification Center with the globe+n shortcut"
  exit 1
}

rg -F 'repeat with passIndex from 1 to 8' "$script" >/dev/null || {
  print -u2 "expected cleanup script to retry notification cleanup across bounded passes"
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

rg -F 'if not passMadeProgress then exit repeat' "$script" >/dev/null || {
  print -u2 "expected cleanup script to stop once a pass makes no progress"
  exit 1
}
```

**Step 2: Run test to verify it fails**

Run: `zsh tests/bin/kitty-clear-notification-center-regression.zsh`
Expected: FAIL because the current wrapper does not yet contain the full cleanup loop and label sets.

**Step 3: Write minimal implementation**

Expand the embedded AppleScript to:

- verify `UI elements enabled`
- send the Notification Center toggle shortcut with `key code 160 using control down`
- wait for Notification Center to appear
- iterate up to eight cleanup passes
- first click bulk clear buttons matching `{"Clear", "Clear All", "清除", "全部清除"}`
- if no bulk clear happened, search buttons matching `{"Close", "Dismiss", "关闭", "忽略"}` inside the Notification Center hierarchy
- track whether the pass made progress and break early when nothing was cleared
- toggle Notification Center closed before returning

Implement helper handlers so the script stays readable:

```applescript
on clickFirstMatchingButton(containerRef, labelList)
on clickButtonsByLabel(containerRef, labelList)
on notificationCenterWindow()
on toggleNotificationCenter()
```

**Step 4: Run test to verify it passes**

Run: `zsh tests/bin/kitty-clear-notification-center-regression.zsh`
Expected: PASS

**Step 5: Commit**

```bash
git add tests/bin/kitty-clear-notification-center-regression.zsh config/.config/kitty/kitty-clear-notification-center
git commit -m "feat: add macOS notification center cleanup automation"
```

### Task 4: Verify the complete shortcut integration

**Files:**
- Verify: `config/.config/kitty/kitty.conf`
- Verify: `config/.config/kitty/kitty-clear-notification-center`
- Verify: `tests/config/kitty-config-regression.zsh`
- Verify: `tests/bin/kitty-clear-notification-center-regression.zsh`

**Step 1: Run focused verification**

Run:
- `zsh tests/config/kitty-config-regression.zsh`
- `zsh tests/bin/kitty-clear-notification-center-regression.zsh`
- `git diff --check`

Expected: all pass with no whitespace or syntax issues.

**Step 2: Manual smoke test on macOS**

Run inside a GUI `kitty` session after `stow` installs the config:
- press `ctrl+q` then `c`

Expected:
- Notification Center opens
- visible notifications are cleared in bulk when possible
- remaining notifications are dismissed individually when possible
- Notification Center closes afterward
- if macOS permissions are missing, the script exits quickly and macOS prompts for the required permission instead of hanging the terminal

**Step 3: Commit**

```bash
git add config/.config/kitty/kitty.conf config/.config/kitty/kitty-clear-notification-center tests/config/kitty-config-regression.zsh tests/bin/kitty-clear-notification-center-regression.zsh
git commit -m "test: verify kitty notification center cleanup shortcut"
```
