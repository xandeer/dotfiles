# Kitty Tab Git Repo Titles Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Show the current git repository name in kitty titles while preserving the existing tab index and title content.

**Architecture:** Keep kitty's existing `tab_title_template`, but add a global watcher that intercepts child-driven title changes and rewrites the visible window title to `repo | title`. This fixes both single-tab macOS window titles and multi-tab kitty tab labels because both surfaces already render the current window title.

**Tech Stack:** kitty config, kitty watcher script, zsh regression test

---

### Task 1: Lock in the watcher contract

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

- Assert that `config/.config/kitty/kitty.conf` keeps `allow_remote_control no`.
- Assert that `config/.config/kitty/kitty.conf` contains `watcher title_watcher.py`.
- Assert that the existing `tab_title_template` line remains in place.
- Assert that `config/.config/kitty/title_watcher.py` exists.
- Assert that the script contains git-aware title helpers and an `on_title_change` callback that calls `window.set_window_title(...)`.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: FAIL because kitty still uses the ineffective custom tab-bar approach or does not yet define the watcher.

### Task 2: Implement repo-aware kitty title rewriting

**Files:**
- Modify: `config/.config/kitty/kitty.conf`
- Create: `config/.config/kitty/title_watcher.py`

**Step 1: Switch kitty to the watcher**

- Keep `allow_remote_control no`.
- Add `watcher title_watcher.py` without removing the existing `tab_title_template`.

**Step 2: Add the minimal watcher**

- Derive a repository label from the active working directory by walking parent directories until `.git` is found.
- Fall back to the active directory basename when no repository is found.
- On `on_title_change`, ignore watcher-originated title updates and rewrite only child-provided titles.
- Prefix the repository label ahead of the current title, deduplicating when both would be identical.

**Step 3: Run test to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: PASS.

### Task 3: Verify the kitty config stays parseable

**Files:**
- Read: `config/.config/kitty/kitty.conf`
- Read: `config/.config/kitty/title_watcher.py`

**Step 1: Run the parser-backed regression test**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: PASS, including the existing `kitty +runpy` config parsing check.
