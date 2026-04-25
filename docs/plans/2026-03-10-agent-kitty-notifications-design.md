# Agent Kitty Notifications Design

## Goal

Add a shared notification path for Codex CLI and Claude Code so their completed results can trigger kitty desktop notifications, but only when the kitty window is not focused.

## Current State

- [config/.claude/settings.json](/Users/kevin/projects/personal/dotfiles/config/.claude/settings.json) is already managed by the `config` stow package and does not define any hooks yet.
- `~/.claude/settings.json` already points at [config/.claude/settings.json](/Users/kevin/projects/personal/dotfiles/config/.claude/settings.json).
- `~/.codex/config.toml` exists as a regular file and already contains local model/provider settings for this machine, but the repository does not manage a stowed `.codex` tree yet.
- [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf) enables `allow_remote_control`, but kitty notifications do not need remote control if a process inside kitty emits `OSC 99`.

## Decision

Use one shared script in the `bin` stow package and wire both agents into it.

- Add `bin/bin/agent-notify-kitty` as the single notification adapter.
- For Codex, configure `notify` in a stowed `config/.codex/config.toml` so Codex invokes the script on agent turn completion.
- For Claude Code, add a `Stop` hook in [config/.claude/settings.json](/Users/kevin/projects/personal/dotfiles/config/.claude/settings.json) so the same script runs when Claude finishes a response.
- Emit kitty `OSC 99` notifications with `o=unfocused` so the notification is suppressed while the current kitty window is focused.
- Do not set the `i=` notification identifier, because reusing a fixed identifier causes later completions from the same kitty window to replace earlier notifications.

This keeps formatting and filtering logic in one place, avoids wrapper aliases around `codex` or `claude`, and matches the user's requirement more closely than plain OS notifications.

## Data Flow

1. Codex or Claude invokes the shared script after finishing a result.
2. The script reads agent-specific payloads:
   - Codex passes a JSON notification argument.
   - Claude hook commands receive JSON on stdin.
3. The script extracts a short title and body from the payload, clips the body to notification-friendly length, and exits quietly if not running in kitty.
4. The script prints `OSC 99` escape sequences with `o=unfocused`, letting kitty decide whether to surface the notification.

## Verification Strategy

- Add a shell regression test for the shared script that simulates both Codex and Claude payloads and asserts the output contains `OSC 99`, `o=unfocused`, and the expected agent labels.
- Assert the script output does not include a fixed `i=` notification identifier.
- Add a configuration regression test that checks the repository-managed Codex and Claude settings reference the shared script.
- Validate JSON and TOML syntax locally after editing.

## Non-Goals

- Do not reproduce cmux-specific unread rings, badges, or notification center UI.
- Do not change kitty installation, theme, or key bindings.
- Do not add wrapper commands around `codex` or `claude`.
- Do not notify for Claude subagent completions unless explicitly requested later.

## Expected Outcome

After `stow` installs the updated dotfiles, Codex and Claude Code should both use the same repository-managed notification path, and kitty should only display those result notifications when the current kitty window is not focused.
