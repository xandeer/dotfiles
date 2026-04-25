# Kitty Tab Ctrl Shortcuts Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Switch kitty tab shortcuts from `alt+1..4` to `ctrl+1..9`.

**Architecture:** Edit the existing kitty config in place. Remove the old four `alt` bindings and replace them with nine `ctrl` bindings that map directly to `goto_tab 1..9`.

**Tech Stack:** kitty config

---

### Task 1: Update kitty tab keybindings

**Files:**
- Modify: `config/.config/kitty/kitty.conf`

**Step 1: Edit the tab mappings**

- Remove `map alt+1 goto_tab 1` through `map alt+4 goto_tab 4`.
- Add `map ctrl+1 goto_tab 1` through `map ctrl+9 goto_tab 9`.

**Step 2: Verify the config**

Run:

```bash
rg -n '^map (ctrl|alt)\\+[1-9] goto_tab' /Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf
```

Expected: only `ctrl+1..9` mappings are present.
