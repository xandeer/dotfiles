# Org Capture Journal CLI Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a direct journal CLI command that appends text to today’s journal without creating a capture UI.

**Architecture:** Reuse the existing hidden journal-target lookup, but bypass `org-capture` entirely. The new helper should insert one inline timestamped entry directly into the target journal buffer, save the file, and leave the current window/buffer untouched. Verification should assert both buffer stability and exact journal output shape.

**Tech Stack:** Emacs Lisp, org-mode, zsh regression script

---

### Task 1: Add a failing regression for direct journal CLI writes

**Files:**
- Modify: `tests/config/emacs-org-capture-journal-cli-regression.zsh`

**Step 1: Write the failing test**

- Stub `org-roam-dailies-capture-today` to point at a real temporary journal file.
- Start from a non-journal scratch buffer.
- Call `x/journal-capture-string "CLI text"`.
- Assert the selected/current buffer remains `*scratch*`.
- Assert the journal file contains a new inline timestamped entry containing `CLI text`.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-org-capture-journal-cli-regression.zsh
```

Expected: FAIL because `x/journal-capture-string` does not exist yet.

### Task 2: Implement the direct command and remove the pending template approach

**Files:**
- Modify: `emacs.d/.emacs.d/lisp/x-org-capture.el`

**Step 1: Write minimal implementation**

- Add `x/journal-capture-string`.
- Reuse the journal-target lookup and journal placement logic.
- Insert `*** <TIMESTAMP> TEXT` as one journal entry line, then ensure the entry ends with a newline.
- Save the journal buffer.
- Remove the uncommitted `J` template so the direct command is the single CLI path.

**Step 2: Run test to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-org-capture-journal-cli-regression.zsh
```

Expected: PASS.

### Task 3: Re-run verification

**Files:**
- Read: `emacs.d/.emacs.d/lisp/x-org-capture.el`
- Read: `tests/config/emacs-org-capture-journal-cli-regression.zsh`

**Step 1: Run the regression again**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-org-capture-journal-cli-regression.zsh
```

Expected: PASS.

**Step 2: Confirm the module still loads**

Run:

```bash
emacs --batch -Q --eval "(progn (require 'org-capture) (defun x/define-keys (&rest _args) nil) (defun x/insert-weather () nil) (load-file \"/Users/kevin/projects/personal/dotfiles/emacs.d/.emacs.d/lisp/x-org-capture.el\") (princ \"x-org-capture-load-ok\n\"))"
```

Expected: `x-org-capture-load-ok`.
