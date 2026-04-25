# Kitty New Tab CWD Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make new kitty tabs inherit the working directory of the currently active tab.

**Architecture:** Update the stow-managed kitty config in place so the default new-tab shortcuts use kitty's built-in `new_tab_with_cwd` action. Lock that behavior in with the existing zsh regression test by rejecting bare `new_tab` mappings and bare `launch --type=tab` actions that would lose the active tab's working directory.

**Tech Stack:** kitty config, zsh regression test

---

### Task 1: Lock in cwd-aware new-tab behavior

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

- Assert that `config/.config/kitty/kitty.conf` contains `map cmd+t new_tab_with_cwd`.
- Assert that `config/.config/kitty/kitty.conf` contains `map ctrl+shift+t new_tab_with_cwd`.
- Fail if `config/.config/kitty/kitty.conf` contains any bare `map ... new_tab` mapping.
- Fail if `config/.config/kitty/kitty.conf` contains any `launch --type=tab` action that does not also include `--cwd=current`.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-new-tab-cwd/tests/config/kitty-config-regression.zsh
```

Expected: FAIL because the config does not yet override the default new-tab shortcuts.

### Task 2: Switch kitty shortcuts to cwd-aware tab creation

**Files:**
- Modify: `config/.config/kitty/kitty.conf`

**Step 1: Write minimal implementation**

- Add `map cmd+t new_tab_with_cwd`.
- Add `map ctrl+shift+t new_tab_with_cwd`.
- Keep existing non-tab mappings unchanged.

**Step 2: Run test to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-new-tab-cwd/tests/config/kitty-config-regression.zsh
```

Expected: PASS.

### Task 3: Re-run the full regression as completion evidence

**Files:**
- Read: `config/.config/kitty/kitty.conf`
- Read: `tests/config/kitty-config-regression.zsh`

**Step 1: Run the parser-backed regression test again**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-new-tab-cwd/tests/config/kitty-config-regression.zsh
```

Expected: PASS, including the existing kitty config parser checks.
