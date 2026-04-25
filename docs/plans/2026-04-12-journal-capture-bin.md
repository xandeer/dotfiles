# Journal Capture Bin Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `journal-capture` shell command that sends argv or stdin text to the direct Emacs journal write function.

**Architecture:** Keep the shell wrapper intentionally thin. It should only collect input, encode it safely with base64, and call `emacsclient -n -e` with a stable `x/journal-capture-string` expression. Verification should stub `emacsclient` so the test checks the exact forwarded expression for both argv and stdin modes.

**Tech Stack:** bash, emacsclient, zsh regression test

---

### Task 1: Add a failing regression for the wrapper

**Files:**
- Create: `tests/bin/journal-capture-regression.zsh`

**Step 1: Write the failing test**

- Create a fake `emacsclient` on `PATH` that records its argv.
- Run the wrapper once with positional arguments and once with piped stdin.
- Assert each run forwards `-n`, `-e`, and an expression containing the expected base64 payload.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/bin/journal-capture-regression.zsh
```

Expected: FAIL because `bin/bin/journal-capture` does not exist yet.

### Task 2: Implement the thin wrapper

**Files:**
- Create: `bin/bin/journal-capture`

**Step 1: Write minimal implementation**

- Use `#!/usr/bin/env bash`.
- If `"$#"` is non-zero, use joined argv as the payload.
- Else, if stdin is piped, read stdin as the payload.
- Else print usage and exit non-zero.
- Base64 encode the payload and call:

```bash
emacsclient -n -e "(x/journal-capture-string (base64-decode-string \"...\"))"
```

- Preserve `emacsclient`'s exit code.

**Step 2: Run test to verify it passes**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/bin/journal-capture-regression.zsh
```

Expected: PASS.

### Task 3: Re-run verification

**Files:**
- Read: `bin/bin/journal-capture`
- Read: `tests/bin/journal-capture-regression.zsh`

**Step 1: Run the wrapper regression again**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/bin/journal-capture-regression.zsh
```

Expected: PASS.
