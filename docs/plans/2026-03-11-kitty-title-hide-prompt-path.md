# Kitty Title Hide Prompt Path Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Keep kitty's repo-aware titles while stripping duplicated shell prompt paths so titles show `repo | command` instead of `repo | path> command`.

**Architecture:** Extend the existing kitty title watcher with one normalization step for prompt-style child titles before the current `repo | title` composition runs. Lock the behavior in with the existing zsh regression test so prompt-style titles are shortened while ordinary titles, repo labels, and unread markers remain unchanged.

**Tech Stack:** kitty watcher script, Python, zsh regression test

---

### Task 1: Lock in prompt-path stripping with a failing regression

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

- Add a watcher-state print for a prompt-style title such as `~/projects/personal/remio> codex`.
- Assert that the watcher returns `prompt_title=repo | codex`.
- Keep the existing expectations for `repo | nvim`, `plain_title=nested | shell`, and unread marker behavior unchanged.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: FAIL because the watcher still preserves the prompt path prefix.

### Task 2: Normalize prompt-style child titles in the watcher

**Files:**
- Modify: `config/.config/kitty/title_watcher.py`

**Step 1: Write minimal implementation**

- Add a small helper that detects prompt-style titles shaped like `PATH> COMMAND`.
- Strip only the leading path segment and keep the command text.
- Run the normalized title through the existing marker stripping and `repo | title` composition path.
- Leave non-prompt titles unchanged.

**Step 2: Run test to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: PASS.

### Task 3: Re-run the full regression as completion evidence

**Files:**
- Read: `config/.config/kitty/title_watcher.py`
- Read: `tests/config/kitty-config-regression.zsh`

**Step 1: Run the parser-backed regression test again**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: PASS, including the existing kitty config parser checks.
