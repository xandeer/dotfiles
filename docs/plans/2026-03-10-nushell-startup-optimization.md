# Nushell Startup Optimization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reduce avoidable Nushell startup overhead by silencing `fnm`, avoiding unnecessary cache rewrites, and de-duplicating `PATH` entries without intentionally changing shell behavior.

**Architecture:** Extend the existing zsh-based smoke test to capture startup output, verify cache preservation, and assert PATH de-duplication. Then make the smallest possible changes in `fnm.nu`, `env.nu`, and `path.nu` to satisfy those tests while leaving the module graph and runtime behavior otherwise intact.

**Tech Stack:** Nushell, zsh test scripts, fnm, zoxide, oh-my-posh

---

### Task 1: Add regression coverage for quiet startup, lazy cache generation, and PATH de-duplication

**Files:**
- Modify: `tests/config/nushell-startup-smoke.zsh`
- Read: `config/.config/nushell/env.nu`
- Read: `config/.config/nushell/modules/fnm.nu`
- Read: `config/.config/nushell/modules/path.nu`

**Step 1: Write the failing test for cache preservation**

- Extend `tests/config/nushell-startup-smoke.zsh` to pre-create:
  - `"$HOME/.cache/zoxide/init.nu"`
  - `"$HOME/.cache/oh-my-posh/init.nu"`
- Write recognizable sentinel content into both files before invoking `nu`.
- After startup, assert that both files still contain the sentinel content.

**Step 2: Run the smoke test to verify it fails**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: FAIL because `env.nu` currently rewrites both cache files.

**Step 3: Write the failing test for quiet startup**

- Capture `nu` stdout into a temporary file.
- Assert that stdout does not contain `Adding FNM to path:` or `Adding FNM vars to shell env:`.

**Step 4: Run the smoke test to verify it still fails for the expected reason**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: FAIL because `fnm.nu` currently prints those messages.

**Step 5: Write the failing test for PATH de-duplication**

- Invoke `nu` with a deliberately duplicated `PATH` in the temporary `HOME`.
- Evaluate the resulting `PATH` and assert that a duplicated fixed entry such as `/opt/homebrew/bin` appears only once.

**Step 6: Run the smoke test to verify it fails**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: FAIL because `path.nu` currently preserves duplicate entries.

### Task 2: Make cache generation lazy in `env.nu`

**Files:**
- Modify: `config/.config/nushell/env.nu`
- Test: `tests/config/nushell-startup-smoke.zsh`

**Step 1: Write the minimal implementation**

- Change `env.nu` so each cache file is generated only if the target file does not already exist.
- Keep the cache directory creation behavior as-is.

**Step 2: Run the smoke test to verify the cache-preservation assertion passes**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: the cache-preservation portion passes, while the remaining assertions may still fail.

**Step 3: Commit the cache optimization**

Run:

```bash
git add config/.config/nushell/env.nu tests/config/nushell-startup-smoke.zsh
git commit -m "perf: avoid rewriting nushell init caches"
```

### Task 3: Silence `fnm` startup and handle missing PATH safely

**Files:**
- Modify: `config/.config/nushell/modules/fnm.nu`
- Test: `tests/config/nushell-startup-smoke.zsh`

**Step 1: Write the minimal implementation**

- Remove the two debug `print` statements.
- Guard the PATH append so it only runs when the parsed record contains `PATH`.
- Keep loading the remaining `FNM_*` variables.

**Step 2: Run the smoke test to verify the quiet-startup assertion passes**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: the quiet-startup assertion passes, while the PATH de-duplication assertion may still fail.

**Step 3: Commit the `fnm` cleanup**

Run:

```bash
git add config/.config/nushell/modules/fnm.nu tests/config/nushell-startup-smoke.zsh
git commit -m "perf: silence fnm nushell startup"
```

### Task 4: De-duplicate PATH entries without changing precedence

**Files:**
- Modify: `config/.config/nushell/modules/path.nu`
- Test: `tests/config/nushell-startup-smoke.zsh`

**Step 1: Write the minimal implementation**

- Keep the current ordered list construction.
- Add a final de-duplication step that preserves the first occurrence of each entry.

**Step 2: Run the smoke test to verify it passes**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: PASS.

**Step 3: Re-run the stow regression**

Run:

```bash
zsh tests/config/nushell-stow-regression.zsh
```

Expected: PASS.

**Step 4: Commit the PATH cleanup**

Run:

```bash
git add config/.config/nushell/modules/path.nu tests/config/nushell-startup-smoke.zsh
git commit -m "perf: deduplicate nushell path entries"
```

### Task 5: Run final verification and review the diff

**Files:**
- Read: `config/.config/nushell/env.nu`
- Read: `config/.config/nushell/modules/fnm.nu`
- Read: `config/.config/nushell/modules/path.nu`
- Read: `tests/config/nushell-startup-smoke.zsh`
- Read: `tests/config/nushell-stow-regression.zsh`

**Step 1: Run the startup smoke test**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: PASS.

**Step 2: Run the stow regression**

Run:

```bash
zsh tests/config/nushell-stow-regression.zsh
```

Expected: PASS.

**Step 3: Check diff hygiene**

Run:

```bash
git diff --check $(git merge-base HEAD master)..HEAD
```

Expected: no output.

**Step 4: Review the final diff**

Run:

```bash
git -C /Users/kevin/projects/personal/dotfiles/.worktrees/nushell-config-simplification diff -- \
  config/.config/nushell/env.nu \
  config/.config/nushell/modules/fnm.nu \
  config/.config/nushell/modules/path.nu \
  tests/config/nushell-startup-smoke.zsh \
  docs/plans
```

Expected: diff shows only the intended startup optimizations, test additions, and the new design/plan docs.
