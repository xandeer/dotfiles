# Kitty Unread Clear On Active Tab Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Keep kitty unread title markers until the tab containing the unread result becomes the active tab.

**Architecture:** Preserve unread notification state per window in the existing watcher, keep focused-window clearing for the window that actually regains focus, and add a tab-aware path in `on_tab_bar_dirty()` for unread state living in background tabs. Use kitty's active-tab data plus active-tab transition tracking to decide when the unread tab has actually been opened, then clear the matching notification and refresh the title.

**Tech Stack:** kitty watcher callbacks, Python, shell regression tests

---

### Task 1: Lock in tab-aware unread clearing with a failing regression

**Files:**
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

- Assert `config/.config/kitty/title_watcher.py` contains `def on_tab_bar_dirty`.
- Extend the fake watcher exercise so focusing another tab's window does not clear unread state for the unread tab.
- Add a fake `tab_manager` and fake tab objects.
- Assert unread state is still present while another tab remains active.
- Assert unread state clears only after `on_tab_bar_dirty()` observes that the unread window belongs to the active tab.
- Assert same-tab `on_tab_bar_dirty()` calls do not clear unread state unless the active tab actually changed.

**Step 2: Run the regression to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: FAIL because the watcher currently clears unread state in `on_focus_change()`.

### Task 2: Implement active-tab clearing in the watcher

**Files:**
- Modify: `config/.config/kitty/title_watcher.py`

**Step 1: Write the minimal implementation**

- Add a helper that checks whether a window id is part of the current active tab.
- Add `on_tab_bar_dirty()` logic that:
  - returns immediately when no unread windows exist
  - resolves the active tab from `data["tab_manager"]`
  - tracks whether the active tab actually changed since the previous tab-bar event
  - finds unread windows that belong to the active tab
  - closes the matching notification for each such window
  - clears `agent_notify` and refreshes titles only for those windows
- Keep `on_focus_change()` clearing for the focused unread window itself.
- Keep existing title composition, prompt-prefix stripping, and notification id handling unchanged.

**Step 2: Run the regression to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: PASS.

### Task 3: Re-run focused verification

**Files:**
- Read: `config/.config/kitty/title_watcher.py`
- Read: `tests/config/kitty-config-regression.zsh`

**Step 1: Re-run the watcher regression**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh
```

Expected: PASS, including the repo-title and unread-marker assertions.

**Step 2: Re-run the notification script regression**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/bin/agent-notify-kitty-regression.zsh
```

Expected: PASS, confirming the title-marker behavior change did not break notification output.

### Task 4: Review final diff

**Files:**
- Read: `config/.config/kitty/title_watcher.py`
- Read: `tests/config/kitty-config-regression.zsh`
- Read: `docs/plans/2026-03-11-kitty-unread-clear-on-active-tab-design.md`

**Step 1: Inspect the final change set**

Run:

```bash
git -C /Users/kevin/projects/personal/dotfiles diff -- config/.config/kitty/title_watcher.py tests/config/kitty-config-regression.zsh docs/plans/2026-03-11-kitty-unread-clear-on-active-tab-design.md docs/plans/2026-03-11-kitty-unread-clear-on-active-tab.md
```

Expected: diff shows only the watcher behavior change, the matching regression updates, and the design/plan docs.
