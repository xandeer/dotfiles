# Kitty Agent Title Notifications Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Show Codex and Claude unread notification state in kitty titles and clear the matching system notification when the window regains focus.

**Architecture:** Extend the shared `agent-notify-kitty` script so each agent result both emits a stable per-window kitty desktop notification id and sets a kitty window user variable describing the unread source. Extend the existing kitty title watcher to render that source marker into the title, refresh on user-var changes, and clear both the marker and the matching notification on focus regain.

**Tech Stack:** kitty `OSC 99`, kitty `OSC 1337 SetUserVar`, kitty watcher callbacks, shell regression tests, Python watcher script

---

### Task 1: Lock in script output for unread-state notifications

**Files:**
- Modify: `tests/bin/agent-notify-kitty-regression.zsh`

**Step 1: Write the failing test**

- Simulate Codex and Claude notification payloads with `KITTY_WINDOW_ID=1`.
- Assert the script output contains `OSC 99`.
- Assert the output contains stable ids `agent-notify-1-codex` and `agent-notify-1-claude`.
- Assert the output contains `SetUserVar` metadata for the unread source marker.

**Step 2: Run the test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/tests/bin/agent-notify-kitty-regression.zsh
```

Expected: FAIL because the script does not yet emit stable notification ids or user-variable updates.

### Task 2: Lock in watcher behavior for source markers and focus clearing

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

- Assert [config/.config/kitty/title_watcher.py](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/config/.config/kitty/title_watcher.py) contains `on_set_user_var` and `on_focus_change`.
- Extend the existing `kitty +runpy` watcher check to assert composed titles can include `[codex]` and `[claude]`.
- Assert the watcher can clear notification state when focus returns.

**Step 2: Run the test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/tests/config/kitty-config-regression.zsh
```

Expected: FAIL because the watcher only handles repo-aware title rewriting today.

### Task 3: Implement unread-state emission in the notification script

**Files:**
- Modify: `bin/bin/agent-notify-kitty`

**Step 1: Write the minimal implementation**

- Compute a stable notification identifier from `KITTY_WINDOW_ID` and the agent name.
- Emit the identifier in the `OSC 99` metadata.
- Emit a matching `SetUserVar` sequence so the active kitty window records `codex` or `claude` as unread notification state.
- Preserve the existing payload parsing, clipping, and `o=unfocused` filtering.

**Step 2: Re-run the script regression**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/tests/bin/agent-notify-kitty-regression.zsh
```

Expected: PASS.

### Task 4: Implement watcher-driven title state and focus cleanup

**Files:**
- Modify: `config/.config/kitty/title_watcher.py`

**Step 1: Write the minimal implementation**

- Add helpers to compose titles with optional `[codex]` or `[claude]` markers.
- React to `on_set_user_var` so unread-state changes refresh the visible title.
- React to `on_focus_change` so focus regain clears the unread marker and closes the matching notification.
- Keep the existing repo-name and cwd fallback behavior intact.

**Step 2: Re-run the watcher regression**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/tests/config/kitty-config-regression.zsh
```

Expected: PASS.

### Task 5: Re-run both regressions as completion evidence

**Files:**
- Read: `bin/bin/agent-notify-kitty`
- Read: `config/.config/kitty/title_watcher.py`
- Read: `tests/bin/agent-notify-kitty-regression.zsh`
- Read: `tests/config/kitty-config-regression.zsh`

**Step 1: Run the full verification set**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/tests/bin/agent-notify-kitty-regression.zsh
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-agent-title-notifications/tests/config/kitty-config-regression.zsh
```

Expected: both commands PASS.
