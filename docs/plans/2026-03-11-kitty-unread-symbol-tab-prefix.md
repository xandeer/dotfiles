# Kitty Unread Symbol Tab Prefix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Show kitty unread state as a colored `◆` before the tab index instead of `[codex]` or `[claude]` inside the tab title text.

**Architecture:** Keep unread state and clear timing in the existing watcher, but switch the rendered marker to a single `◆`. Use kitty's `tab_title_template "{custom}"` path and a small `tab_bar.py` renderer to move that marker before the tab index while preserving the existing title body and numbering behavior.

**Tech Stack:** kitty config, kitty custom tab title renderer, Python, shell regression tests

---

### Task 1: Lock in the custom tab-title renderer behavior

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

- Change the expected `tab_title_template` to `{custom}`.
- Add assertions that `config/.config/kitty/tab_bar.py` exists and exposes `draw_title`.
- Add a `kitty +runpy` check that a title beginning with `◆ ` renders as a colored prefix before the tab index.
- Change existing watcher expectations from `[codex]` / `[claude]` to `◆`.

**Step 2: Run the regression to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: FAIL because the current config still uses the old template and no custom renderer exists.

### Task 2: Implement the unread marker rendering changes

**Files:**
- Modify: `config/.config/kitty/kitty.conf`
- Modify: `config/.config/kitty/title_watcher.py`
- Create: `config/.config/kitty/tab_bar.py`

**Step 1: Write the minimal implementation**

- Change the watcher's visible unread marker to `◆`.
- Change `tab_title_template` to `{custom}`.
- Add `tab_bar.py` with `draw_title(data)` that:
  - strips a leading `◆ `
  - emits an amber `◆ ` prefix before `{index}:`
  - preserves bell/activity symbols, progress, and the title body
- Keep unread clear timing unchanged.

**Step 2: Run the regression to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: PASS.

### Task 3: Re-run focused verification

**Files:**
- Read: `config/.config/kitty/kitty.conf`
- Read: `config/.config/kitty/title_watcher.py`
- Read: `config/.config/kitty/tab_bar.py`
- Read: `tests/config/kitty-config-regression.zsh`

**Step 1: Re-run the watcher regression**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: PASS.

**Step 2: Re-run the notification script regression**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/bin/agent-notify-kitty-regression.zsh
```

Expected: PASS, confirming the tab-title rendering change did not break notification output.
