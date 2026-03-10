# Kitty Tab Bar Top Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move the kitty tab bar to the top edge of the window.

**Architecture:** Update the stow-managed kitty config with kitty's native `tab_bar_edge top` setting and extend the existing kitty regression to prevent future drift. Keep all existing title, watcher, and notification behavior unchanged.

**Tech Stack:** kitty config, zsh regression test

---

### Task 1: Lock in top-edge tab bar placement

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

- Assert `config/.config/kitty/kitty.conf` contains `tab_bar_edge top`.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-tab-bar-top/tests/config/kitty-config-regression.zsh
```

Expected: FAIL because the config does not yet pin the tab bar to the top edge.

### Task 2: Set kitty to render the tab bar on top

**Files:**
- Modify: `config/.config/kitty/kitty.conf`

**Step 1: Write the minimal implementation**

- Add `tab_bar_edge top`.

**Step 2: Run test to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-tab-bar-top/tests/config/kitty-config-regression.zsh
```

Expected: PASS.
