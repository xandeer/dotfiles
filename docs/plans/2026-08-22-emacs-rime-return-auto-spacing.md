# Emacs Rime Return Auto-Spacing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Route Emacs Rime raw Return through librime so the existing bidirectional Han/ASCII-letter spacing logic runs unchanged.

**Architecture:** Add one Emacs command that normalizes Return and delegates to the existing `rime-send-keybinding`; retain the upstream preview branch. Extend the existing module regression with an Elisp routing check and a native librime sequence, without duplicating spacing logic in Emacs.

**Tech Stack:** Emacs Lisp, emacs-rime native module, librime-lua, zsh regression harness

---

### Task 1: Add failing Emacs Return routing regressions

**Files:**
- Modify: `tests/config/emacs-rime-module-regression.zsh:120-207`
- Modify: `tests/config/emacs-rime-module-regression.zsh:212-275`

**Step 1: Enable the shared Return processor in the native fixture**

Add `lua_processor@select_character` before the existing processors and `lua_filter@auto_space_filter` after `lua_filter@ai_candidate_filter` in the temporary `test.schema.yaml`.

After the existing AI candidate assertion, use the real module to:

1. Commit `测试` and assert the native commit is exactly `测试`.
2. Feed `harness`, process key `65293` with mask `0`, and assert the commit is exactly `" harness"`.
3. Feed `a`, assert the first candidate is `" 测试"`, commit it, and assert the commit is exactly `" 测试"`.

**Step 2: Add the Elisp routing contract**

In the second batch Emacs block, after loading `x-rime.el` twice:

```elisp
(unless (eq (lookup-key rime-active-mode-map (kbd "RET"))
            #'x/rime-return)
  (error "RET is not routed through x/rime-return"))
(unless (eq (lookup-key rime-active-mode-map (kbd "<return>"))
            #'x/rime-return)
  (error "<return> is not routed through x/rime-return"))

(let (sent-event previewed)
  (cl-letf (((symbol-function 'rime-send-keybinding)
             (lambda () (setq sent-event last-input-event)))
            ((symbol-function 'rime--commit-preview)
             (lambda () (setq previewed t))))
    (setq rime-return-insert-raw t
          last-input-event ?\r)
    (x/rime-return)
    (unless (eq sent-event 'return)
      (error "raw RET was not normalized: %S" sent-event))
    (when previewed
      (error "raw RET unexpectedly committed preview"))

    (setq rime-return-insert-raw nil
          sent-event nil
          previewed nil)
    (x/rime-return)
    (unless previewed
      (error "preview RET did not preserve upstream behavior"))
    (when sent-event
      (error "preview RET unexpectedly reached librime"))))
```

**Step 3: Run the test to verify RED**

Run:

```zsh
/bin/zsh tests/config/emacs-rime-module-regression.zsh
```

Expected: nonzero with `x/rime-return` missing or the RET binding still resolving to `rime--return`. Record the exact failure before implementation.

### Task 2: Route raw Emacs Return through librime

**Files:**
- Modify: `emacs.d/.emacs.d/lisp/x-rime.el:15-95`
- Test: `tests/config/emacs-rime-module-regression.zsh`

**Step 1: Add the minimal command and bindings**

Inside the existing `with-eval-after-load 'rime` block, add:

```elisp
(defun x/rime-return ()
  "Commit raw input through Rime so shared processors run."
  (interactive)
  (if rime-return-insert-raw
      (let ((last-input-event 'return))
        (rime-send-keybinding))
    (rime--commit-preview)))

(define-key rime-active-mode-map (kbd "RET") #'x/rime-return)
(define-key rime-active-mode-map (kbd "<return>") #'x/rime-return)
```

Do not alter Shift+Return, Ctrl+J/L, predicates, `rime.lua`, schema, or the dynamic module patch.

**Step 2: Run GREEN regressions**

Run:

```zsh
/bin/zsh tests/config/emacs-rime-module-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
/bin/zsh -n tests/config/emacs-rime-module-regression.zsh
git diff --check
```

Expected: `Emacs Rime module regression OK`, `Rime auto-space regression OK`, and `Rime AI regression OK`; syntax and diff checks exit 0.

**Step 3: Commit**

```zsh
git add emacs.d/.emacs.d/lisp/x-rime.el tests/config/emacs-rime-module-regression.zsh
git commit -m "fix(rime): route Emacs Return through librime"
```

### Task 3: Review, integrate, and reload Emacs

**Files:**
- Verify: `emacs.d/.emacs.d/lisp/x-rime.el`
- Verify: `tests/config/emacs-rime-module-regression.zsh`

**Step 1: Review the exact diff**

Confirm the raw branch delegates through normalized native Return, the preview branch remains unchanged, both unmodified Return bindings point to the new command, and native tests prove both spacing directions. Reject any host-buffer character inspection or duplicated spacing rules.

**Step 2: Fast-forward merge and rerun merged gates**

Fast-forward the reviewed branch into `master`, then rerun all commands from Task 2 Step 2 on merged `master`.

**Step 3: Reload the active Emacs configuration**

If an Emacs server is available, evaluate:

```elisp
(progn
  (load-file "/Users/kevin/projects/personal/dotfiles/emacs.d/.emacs.d/lisp/x-rime.el")
  (and (eq (lookup-key rime-active-mode-map (kbd "RET")) #'x/rime-return)
       (eq (lookup-key rime-active-mode-map (kbd "<return>")) #'x/rime-return)))
```

Require a true result. If no server is available, report that the stowed file is ready for the next Emacs restart; do not claim a live reload.

**Step 4: Clean the task worktree and branch**

Only after merged-tree verification and reload evidence, remove the task worktree and delete the merged feature branch. Do not push.
