# Kitty Tab Index Titles Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Prepend kitty tab numbers to visible tab titles.

**Architecture:** Update the existing kitty config in place with a `tab_title_template` that adds `{index}` before the tab title. Keep the existing bell/activity indicators so the title bar behavior stays close to kitty defaults.

**Tech Stack:** kitty config

---

### Task 1: Add numbered tab titles

**Files:**
- Modify: `config/.config/kitty/kitty.conf`

**Step 1: Add the title template**

- Add a `tab_title_template` entry that prepends `{index}: ` before `{title}`.
- Keep bell/activity indicators in the template.

**Step 2: Verify the config**

Run:

```bash
rg -n '^tab_title_template ' /Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf
```

Expected: the config contains one `tab_title_template` line with `{index}` before `{title}`.
