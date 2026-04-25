# Nushell Config Simplification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move the real Nushell configuration into `config/.config/nushell/`, keep `config/Library/Application Support/nushell/` as thin entrypoints, and preserve existing shell behavior.

**Architecture:** Split the current monolithic `config.nu` into source-only modules under `config/.config/nushell/modules/`, keep the existing `env.nu` behavior under `config/.config/nushell/env.nu`, and make the macOS `Library/Application Support/nushell` files forward into the XDG-style config tree. Verification combines Stow layout checks with a direct Nushell startup smoke test against the repository-managed config.

**Tech Stack:** GNU Stow, Nushell, zoxide, oh-my-posh, fnm, zsh test scripts

---

### Task 1: Strengthen regression coverage for the new layout

**Files:**
- Modify: `tests/config/nushell-stow-regression.zsh`
- Read: `config/Library/Application Support/nushell/config.nu`
- Read: `config/Library/Application Support/nushell/env.nu`

**Step 1: Write the failing regression assertions**

- Extend the test to assert that Stow exposes `"$tmp_home/.config/nushell/config.nu"` and `"$tmp_home/.config/nushell/env.nu"`.
- Assert that `"$tmp_home/.config/nushell/modules/"` contains `theme.nu`, `path.nu`, `fnm.nu`, `prompt.nu`, `completions.nu`, `hooks.nu`, and `aliases.nu`.
- Replace the current file-equality-only checks for the `Library/Application Support/nushell` files with content checks that enforce the thin-entrypoint contract:
  - `config.nu` forwards to `~/.config/nushell/config.nu`
  - `env.nu` forwards to `~/.config/nushell/env.nu`

**Step 2: Run the regression test to verify it fails**

Run:

```bash
zsh tests/config/nushell-stow-regression.zsh
```

Expected: exit status `1` because `.config/nushell/` and thin entrypoint content do not exist yet.

**Step 3: Commit the failing-test change after implementation passes**

Run:

```bash
git add tests/config/nushell-stow-regression.zsh
git commit -m "test: cover nushell xdg entrypoints"
```

### Task 2: Move the real config into `config/.config/nushell`

**Files:**
- Create: `config/.config/nushell/config.nu`
- Create: `config/.config/nushell/env.nu`
- Create: `config/.config/nushell/modules/theme.nu`
- Create: `config/.config/nushell/modules/path.nu`
- Create: `config/.config/nushell/modules/fnm.nu`
- Create: `config/.config/nushell/modules/prompt.nu`
- Create: `config/.config/nushell/modules/completions.nu`
- Create: `config/.config/nushell/modules/hooks.nu`
- Create: `config/.config/nushell/modules/aliases.nu`
- Modify: `config/Library/Application Support/nushell/config.nu`
- Modify: `config/Library/Application Support/nushell/env.nu`
- Read: `config/.config/env.nu`

**Step 1: Create the real `env.nu` in the XDG tree**

- Copy the current cache-generation logic from `config/Library/Application Support/nushell/env.nu` into `config/.config/nushell/env.nu`.
- Keep its runtime behavior unchanged.

**Step 2: Split the current `config.nu` into modules**

- Move the theme table and `$env.config` setup into `modules/theme.nu`.
- Move PATH assembly into `modules/path.nu`.
- Move the existing `fnm` import logic into `modules/fnm.nu`.
- Move prompt setup into `modules/prompt.nu`.
- Move completion imports into `modules/completions.nu`.
- Move hook setup into `modules/hooks.nu`.
- Move alias definitions into `modules/aliases.nu`.

**Step 3: Create the new XDG `config.nu` assembler**

- In `config/.config/nushell/config.nu`, derive the current config directory from `$nu.config-path | path dirname`.
- Build module paths from that directory so the file works both when stowed into `~/.config/nushell` and when invoked directly with `nu --config <repo-path>`.
- Source the modules in this order:
  1. `theme.nu`
  2. `path.nu`
  3. `fnm.nu`
  4. `prompt.nu`
  5. `completions.nu`
  6. `hooks.nu`
  7. `source ~/.config/env.nu`
  8. `aliases.nu`

**Step 4: Reduce the macOS entrypoints to thin forwarders**

- Replace `config/Library/Application Support/nushell/config.nu` with a single source statement for `~/.config/nushell/config.nu`.
- Replace `config/Library/Application Support/nushell/env.nu` with a single source statement for `~/.config/nushell/env.nu`.

**Step 5: Run the regression test to verify it passes**

Run:

```bash
zsh tests/config/nushell-stow-regression.zsh
```

Expected: exit status `0`.

**Step 6: Commit the layout refactor**

Run:

```bash
git add config/.config/nushell 'config/Library/Application Support/nushell'
git commit -m "refactor: move nushell config into xdg tree"
```

### Task 3: Add a Nushell startup smoke test for the repository-managed config

**Files:**
- Create: `tests/config/nushell-startup-smoke.zsh`
- Read: `config/.config/nushell/config.nu`
- Read: `config/.config/nushell/env.nu`

**Step 1: Write the failing smoke test**

- Create a zsh test script that runs:

```bash
nu --config '/Users/kevin/projects/personal/dotfiles/config/.config/nushell/config.nu' \
  --env-config '/Users/kevin/projects/personal/dotfiles/config/.config/nushell/env.nu' \
  -c 'version | get version'
```

- Make the script fail if `nu` exits nonzero.

**Step 2: Run the smoke test to verify it fails or exposes missing path assumptions**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: the first run fails until module sourcing is wired through `$nu.config-path`.

**Step 3: Adjust module loading only as needed for direct invocation**

- Keep behavior unchanged.
- Restrict changes to path resolution for module sourcing, not to the module contents.

**Step 4: Run the smoke test to verify it passes**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: exit status `0` and a Nushell version string on stdout.

**Step 5: Commit the smoke test coverage**

Run:

```bash
git add tests/config/nushell-startup-smoke.zsh config/.config/nushell/config.nu
git commit -m "test: add nushell startup smoke test"
```

### Task 4: Run final verification and review the diff

**Files:**
- Read: `tests/config/nushell-stow-regression.zsh`
- Read: `tests/config/nushell-startup-smoke.zsh`
- Read: `config/.config/nushell/config.nu`
- Read: `config/.config/nushell/env.nu`

**Step 1: Re-run the stow regression**

Run:

```bash
zsh tests/config/nushell-stow-regression.zsh
```

Expected: exit status `0`.

**Step 2: Re-run the startup smoke test**

Run:

```bash
zsh tests/config/nushell-startup-smoke.zsh
```

Expected: exit status `0`.

**Step 3: Review the final diff**

Run:

```bash
git -C /Users/kevin/projects/personal/dotfiles diff -- \
  config/.config/nushell \
  'config/Library/Application Support/nushell' \
  tests/config \
  docs/plans
```

Expected: diff shows the XDG Nushell tree, the thin macOS entrypoints, the updated regression test, the smoke test, and the new design/plan docs.
