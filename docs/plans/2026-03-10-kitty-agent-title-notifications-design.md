# Kitty Agent Title Notifications Design

## Goal

Show Codex and Claude notification state in kitty tab titles, and clear both the title marker and the matching system notification when the window regains focus.

## Current State

- [config/.config/kitty/title_watcher.py](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/config/.config/kitty/title_watcher.py) currently rewrites titles to `repo | title`.
- [bin/bin/agent-notify-kitty](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/bin/bin/agent-notify-kitty) already emits kitty `OSC 99` desktop notifications for Codex and Claude results.
- The existing notification path intentionally avoids setting a notification identifier, so repeated results do not replace each other.

## Decision

Use kitty window user variables for title state and a stable per-window notification identifier for system-notification cleanup.

- Extend [bin/bin/agent-notify-kitty](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/bin/bin/agent-notify-kitty) to emit a stable notification id per `KITTY_WINDOW_ID` and agent source:
  - `agent-notify-<window-id>-codex`
  - `agent-notify-<window-id>-claude`
- Keep using `OSC 99` with `o=unfocused`, but add the stable `i=` identifier so the matching notification can be explicitly closed later.
- Emit a kitty `OSC 1337;SetUserVar=...` sequence from the same script so the current window records whether the unread result came from `codex` or `claude`.
- Extend [config/.config/kitty/title_watcher.py](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/config/.config/kitty/title_watcher.py) so the visible title becomes `repo | [codex] title` or `repo | [claude] title` when that user variable is present.
- Handle `on_set_user_var` to refresh the title immediately when a notification arrives.
- Handle `on_focus_change` so when the window regains focus it clears the user variable and closes the matching stable notification.

## Tradeoff

Using a stable id means repeated notifications from the same window and same agent replace each other instead of stacking in the system notification center.

This is acceptable because:

- The tab title only needs one current unread source marker.
- The focus-clear requirement needs an addressable notification identifier.
- The complexity stays much lower than tracking an open-ended per-window notification list.

## Verification

- Extend [tests/bin/agent-notify-kitty-regression.zsh](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/tests/bin/agent-notify-kitty-regression.zsh) to assert the script emits:
  - `OSC 99`
  - stable per-window agent notification identifiers
  - `OSC 1337;SetUserVar` updates for the source marker
- Extend [tests/config/kitty-config-regression.zsh](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/tests/config/kitty-config-regression.zsh) to assert:
  - source markers are injected into composed titles
  - focus regain clears the source marker
  - the watcher contains focus-change and user-var callbacks needed for the unread-state lifecycle
