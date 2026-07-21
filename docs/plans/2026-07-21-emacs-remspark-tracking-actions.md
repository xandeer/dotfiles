# Emacs RemSpark Tracking Actions Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Route the existing `In last`, `Out`, and `Done current` Transient shortcuts to RemSpark's resume, pause, and complete URL actions without changing their keys or labels.

**Architecture:** Keep the change inside `x/transient-global-group` and reuse the existing autoloaded `x/open` helper. Each affected suffix becomes an interactive lambda with one fixed RemSpark URL, while a batch Emacs regression invokes the actual runtime suffix commands and captures their opened URLs.

**Tech Stack:** Emacs Lisp, Transient, macOS `open`, zsh regression script

---

### Task 1: Add a failing runtime regression for the three shortcuts

**Files:**
- Create: `tests/config/emacs-remspark-tracking-actions-regression.zsh`
- Read: `emacs.d/.emacs.d/lisp/x-transients.el:146`

**Step 1: Write the failing test**

Create an executable zsh script that:

- Loads `x-transients.el` in batch Emacs with minimal `consult` and key-definition stubs.
- Defines a capturing `x/open` stub.
- Uses `transient-get-suffix` to retrieve the runtime suffix command for `i`, `l`, and `k`.
- Invokes each actual suffix command interactively.
- Prints each key, unchanged description, and captured URL.
- Asserts these exact lines:

```text
i|In last|remspark://trackingAction?action=resume
l|Out|remspark://trackingAction?action=pause
k|Done current|remspark://trackingAction?action=complete
```

**Step 2: Make the test executable**

Run:

```bash
chmod +x /Users/kevin/projects/personal/dotfiles/tests/config/emacs-remspark-tracking-actions-regression.zsh
```

Expected: the script has executable permissions.

**Step 3: Run the test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-remspark-tracking-actions-regression.zsh
```

Expected: FAIL because the three suffixes still invoke the Org clock commands and therefore do not call the capturing `x/open` stub.

### Task 2: Replace the three Transient handlers

**Files:**
- Modify: `emacs.d/.emacs.d/lisp/x-transients.el:149`
- Test: `tests/config/emacs-remspark-tracking-actions-regression.zsh`

**Step 1: Write the minimal implementation**

Replace only the commands for `i`, `l`, and `k`:

```emacs-lisp
("i" "In last" (lambda ()
                   (interactive)
                   (x/open "remspark://trackingAction?action=resume")))
```

```emacs-lisp
("l" "Out" (lambda ()
              (interactive)
              (x/open "remspark://trackingAction?action=pause")))
```

```emacs-lisp
("k" "Done current" (lambda ()
                       (interactive)
                       (x/open "remspark://trackingAction?action=complete")))
```

Leave the keys, descriptions, and `g`, `w`, `r`, `n`, and `j` suffixes unchanged.

**Step 2: Run the regression to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-remspark-tracking-actions-regression.zsh
```

Expected: PASS with all three runtime suffix mappings matching their expected URLs and labels.

### Task 3: Verify the focused Emacs configuration change

**Files:**
- Read: `emacs.d/.emacs.d/lisp/x-transients.el`
- Read: `tests/config/emacs-remspark-tracking-actions-regression.zsh`

**Step 1: Run the targeted regression again**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/emacs-remspark-tracking-actions-regression.zsh
```

Expected: PASS.

**Step 2: Check the edited files for whitespace errors**

Run:

```bash
git diff --check
```

Expected: no output and exit status 0.

**Step 3: Review the scoped diff**

Run:

```bash
git diff -- emacs.d/.emacs.d/lisp/x-transients.el tests/config/emacs-remspark-tracking-actions-regression.zsh
```

Expected: one new focused regression script and only the three requested Transient handler replacements.

### Task 4: Commit the implementation

**Files:**
- Add: `emacs.d/.emacs.d/lisp/x-transients.el`
- Add: `tests/config/emacs-remspark-tracking-actions-regression.zsh`
- Add: `docs/plans/2026-07-21-emacs-remspark-tracking-actions.md`

**Step 1: Stage the scoped files**

Run:

```bash
git add emacs.d/.emacs.d/lisp/x-transients.el tests/config/emacs-remspark-tracking-actions-regression.zsh docs/plans/2026-07-21-emacs-remspark-tracking-actions.md
```

**Step 2: Commit**

Run:

```bash
git commit -m "feat: route Emacs tracking actions to RemSpark"
```

Expected: a commit containing only the implementation plan, regression test, and three shortcut-handler changes.
