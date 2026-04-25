# Org Capture Journal Hidden Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Keep org-capture journal entry insertion working while preventing `x/find-journal-location` from visibly switching the selected window to the journal file.

**Architecture:** Wrap the org-roam daily-note navigation in a window-preserving context, record the target journal buffer and point, then hand that hidden buffer back to `org-capture` with `set-buffer`. Lock the behavior in with a batch Emacs regression that checks visible-window state separately from current-buffer state.

**Tech Stack:** Emacs Lisp, org-capture, org-roam dailies, zsh regression script

---

### Task 1: Add a failing regression for hidden journal targeting

**Files:**
- Create: `tests/config/emacs-org-capture-journal-hidden-regression.zsh`

**Step 1: Write the failing test**

- Load `x-org-capture.el` in batch Emacs with minimal stubs for local helpers.
- Stub `org-roam-dailies-capture-today` so it switches to a fake journal buffer.
- Start with the selected window showing a fake capture buffer.
- Assert that after `x/find-journal-location`, the selected window still shows the capture buffer while the current buffer is the journal buffer.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-org-capture-journal-hidden-regression.zsh
```

Expected: FAIL because `x/find-journal-location` currently leaves the selected window showing the journal buffer.

### Task 2: Make journal targeting happen off-screen

**Files:**
- Modify: `emacs.d/.emacs.d/lisp/x-org-capture.el`

**Step 1: Write minimal implementation**

- Add a small helper that runs `org-roam-dailies-capture-today` inside a window-preserving context.
- Record the journal buffer and point after org-roam opens the daily note.
- Restore the visible window state, then use `set-buffer` and `goto-char` to continue in the journal buffer without displaying it.
- Keep the existing heading search and insertion logic intact.

**Step 2: Run test to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-org-capture-journal-hidden-regression.zsh
```

Expected: PASS.

### Task 3: Re-run targeted verification

**Files:**
- Read: `emacs.d/.emacs.d/lisp/x-org-capture.el`
- Read: `tests/config/emacs-org-capture-journal-hidden-regression.zsh`

**Step 1: Run the regression again**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-org-capture-journal-hidden-regression.zsh
```

Expected: PASS.

**Step 2: Confirm the module still loads in batch Emacs**

Run:

```bash
emacs --batch -Q --eval "(progn (require 'org-capture) (defun x/define-keys (&rest _args) nil) (defun x/insert-weather () nil) (load-file \"/Users/kevin/projects/personal/dotfiles/emacs.d/.emacs.d/lisp/x-org-capture.el\") (princ \"x-org-capture-load-ok\n\"))"
```

Expected: `x-org-capture-load-ok`.
