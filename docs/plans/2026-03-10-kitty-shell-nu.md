# Kitty Shell Nu Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Switch kitty's default shell from login zsh to login Nushell.

**Architecture:** Update the stow-managed kitty config so kitty launches `/etc/profiles/per-user/kevin/bin/nu -l` by default. Extend the existing parser-backed kitty regression to prevent drift back to zsh while leaving the rest of the kitty configuration untouched.

**Tech Stack:** kitty config, zsh regression test

---

### Task 1: Lock in the login Nushell shell setting

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

- Assert kitty's parsed `shell` setting equals `/etc/profiles/per-user/kevin/bin/nu -l`.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-shell-nu/tests/config/kitty-config-regression.zsh
```

Expected: FAIL because kitty still launches `/bin/zsh -l`.

### Task 2: Switch kitty to login Nushell

**Files:**
- Modify: `config/.config/kitty/kitty.conf`

**Step 1: Write the minimal implementation**

- Replace the existing `shell /bin/zsh -l` line with `shell /etc/profiles/per-user/kevin/bin/nu -l`.

**Step 2: Re-run the regression**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/.worktrees/kitty-shell-nu/tests/config/kitty-config-regression.zsh
```

Expected: PASS.
