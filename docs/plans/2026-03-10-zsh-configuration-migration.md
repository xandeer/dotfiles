# Zsh Configuration Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor the repository's zsh configuration into clear shared-shell, login-shell, interactive-shell, host-specific, and local override layers without breaking the current user workflow.

**Architecture:** Introduce a new `profile/` stow package for POSIX-compatible shell setup, slim down the root zsh entry points, and split the current large zsh fragments into role-based files under `zsh/.config/zsh/`. Keep `zplug` for now, but isolate it behind a dedicated plugin layer so future replacement does not require another entry-point refactor.

**Tech Stack:** GNU Stow, zsh, POSIX shell, existing `zplug` plugin manager, repository `Makefile`

---

### Task 1: Add a shared shell package boundary

**Files:**
- Create: `profile/.profile`
- Create: `profile/.config/profile.d/env.sh`
- Create: `profile/.config/profile.d/path.sh`
- Create: `profile/.config/profile.d/darwin.sh`
- Create: `profile/.config/profile.d/linux.sh`
- Create: `profile/.config/profile.d/local.sh`
- Modify: `Makefile`

**Step 1: Write the failing verification command**

Run:

```bash
test -f profile/.profile
```

Expected: exit status `1` because the new shared shell package does not exist yet.

**Step 2: Create the shared shell package**

- Add `profile` to `CONFIGS` in `Makefile`.
- Create `profile/.profile` so it only sources `~/.config/profile.d/*.sh` using POSIX-compatible syntax.
- Create `env.sh`, `path.sh`, `darwin.sh`, `linux.sh`, and `local.sh` with comments that explain their responsibility.

**Step 3: Run syntax verification**

Run:

```bash
sh -n profile/.profile profile/.config/profile.d/env.sh profile/.config/profile.d/path.sh profile/.config/profile.d/darwin.sh profile/.config/profile.d/linux.sh profile/.config/profile.d/local.sh
```

Expected: exit status `0`.

**Step 4: Commit**

```bash
git add Makefile profile
git commit -m "refactor: add shared shell profile package"
```

### Task 2: Introduce minimal zsh entry points

**Files:**
- Modify: `zsh/.zshenv`
- Create: `zsh/.zprofile`
- Modify: `zsh/.zshrc`
- Create: `zsh/.config/zsh/core/helpers.zsh`
- Create: `zsh/.config/zsh/core/source.zsh`

**Step 1: Capture the current startup regression**

Run:

```bash
ZDOTDIR="$PWD/zsh" zsh -i -c exit
```

Expected: startup currently emits `bindkey: cannot bind to an empty key sequence` and performs heavy initialization from `.zshenv`.

**Step 2: Write minimal entry points**

- Reduce `zsh/.zshenv` to the smallest possible bootstrap.
- Add `zsh/.zprofile` to handle login-only zsh initialization.
- Convert `zsh/.zshrc` into an orchestrator that sources `core`, `host`, `plugins`, and `interactive` fragments in order.
- Add reusable helper/source functions under `zsh/.config/zsh/core/`.

**Step 3: Run syntax verification**

Run:

```bash
zsh -n zsh/.zshenv zsh/.zprofile zsh/.zshrc zsh/.config/zsh/core/helpers.zsh zsh/.config/zsh/core/source.zsh
```

Expected: exit status `0`.

**Step 4: Commit**

```bash
git add zsh/.zshenv zsh/.zprofile zsh/.zshrc zsh/.config/zsh/core
git commit -m "refactor: split zsh entry points by shell phase"
```

### Task 3: Move shared environment and path logic into the right layer

**Files:**
- Modify: `zsh/.config/zsh/exports.zsh`
- Modify: `zsh/.config/zsh/path.zsh`
- Modify: `profile/.config/profile.d/env.sh`
- Modify: `profile/.config/profile.d/path.sh`
- Modify: `profile/.config/profile.d/darwin.sh`
- Modify: `profile/.config/profile.d/linux.sh`
- Create: `zsh/.config/zsh/login/brew.zsh`
- Create: `zsh/.config/zsh/login/nix.zsh`
- Create: `zsh/.config/zsh/login/fnm.zsh`

**Step 1: Record the current layering problem**

Run:

```bash
nl -ba zsh/.zshenv | sed -n '1,40p'
```

Expected: output shows `brew` and `fnm` initialization still happening from `.zshenv`.

**Step 2: Rehome environment initialization**

- Move shared exports and generic path rules into `profile/.config/profile.d/`.
- Move zsh login-only setup into `zsh/.config/zsh/login/brew.zsh`, `nix.zsh`, and `fnm.zsh`.
- Keep host-specific paths and SDK setup out of generic files.

**Step 3: Run syntax and smoke verification**

Run:

```bash
sh -n profile/.config/profile.d/*.sh
zsh -n zsh/.zshenv zsh/.zprofile zsh/.config/zsh/login/*.zsh
```

Expected: both commands exit `0`.

**Step 4: Commit**

```bash
git add profile/.config/profile.d zsh/.zshenv zsh/.zprofile zsh/.config/zsh/login zsh/.config/zsh/exports.zsh zsh/.config/zsh/path.zsh
git commit -m "refactor: separate shared env from zsh login setup"
```

### Task 4: Split plugin management into a replaceable layer

**Files:**
- Create: `zsh/.config/zsh/plugins/zplug.zsh`
- Create: `zsh/.config/zsh/plugins/bundles.zsh`
- Modify: `zsh/.zshrc`

**Step 1: Capture the current plugin monolith**

Run:

```bash
nl -ba zsh/.zshrc | sed -n '1,220p'
```

Expected: output shows plugin bootstrap, plugin declarations, prompt configuration, and interactive behavior mixed together.

**Step 2: Split plugin bootstrap from plugin declarations**

- Move zplug environment setup and `check/install/load` logic into `plugins/zplug.zsh`.
- Move plugin declarations into `plugins/bundles.zsh`.
- Keep `.zshrc` limited to sourcing the plugin layer.

**Step 3: Run syntax verification**

Run:

```bash
zsh -n zsh/.zshrc zsh/.config/zsh/plugins/zplug.zsh zsh/.config/zsh/plugins/bundles.zsh
```

Expected: exit status `0`.

**Step 4: Commit**

```bash
git add zsh/.zshrc zsh/.config/zsh/plugins
git commit -m "refactor: isolate zplug behind plugin layer"
```

### Task 5: Split interactive behavior and fix runtime regressions

**Files:**
- Create: `zsh/.config/zsh/interactive/aliases.zsh`
- Create: `zsh/.config/zsh/interactive/key-bindings.zsh`
- Create: `zsh/.config/zsh/interactive/history.zsh`
- Create: `zsh/.config/zsh/interactive/prompt.zsh`
- Modify: `zsh/.config/zsh/alias.zsh`
- Modify: `zsh/.config/zsh/key-bindings.zsh`
- Modify: `zsh/.zshrc`

**Step 1: Reproduce the current runtime issues**

Run:

```bash
ZDOTDIR="$PWD/zsh" zsh -i -c exit
```

Expected: current config emits empty-key-sequence bind errors, and baseline commands can be shadowed by missing replacements.

**Step 2: Split and harden interactive configuration**

- Move aliases, key bindings, history widgets, and prompt settings into role-based interactive files.
- Guard `bindkey` calls so they only run when terminfo values are non-empty.
- Only define aliases like `ping`, `du`, `preview`, or `s` when their replacement command exists.
- Keep zsh-only features out of POSIX shell files.

**Step 3: Run syntax and startup verification**

Run:

```bash
zsh -n zsh/.config/zsh/interactive/*.zsh zsh/.zshrc
ZDOTDIR="$PWD/zsh" zsh -i -c exit
```

Expected: syntax check exits `0`, and interactive startup exits without the empty key sequence warning.

**Step 4: Commit**

```bash
git add zsh/.zshrc zsh/.config/zsh/interactive zsh/.config/zsh/alias.zsh zsh/.config/zsh/key-bindings.zsh
git commit -m "fix: harden interactive zsh configuration"
```

### Task 6: Isolate platform and machine-specific overrides

**Files:**
- Create: `zsh/.config/zsh/host/darwin.zsh`
- Create: `zsh/.config/zsh/host/linux.zsh`
- Create: `zsh/.config/zsh/host/<hostname>.zsh`
- Create: `zsh/.config/zsh/local/overrides.zsh.example`
- Modify: `zsh/.config/zsh/proxy.zsh`
- Modify: `zsh/.zshrc`

**Step 1: Record the current host leakage**

Run:

```bash
nl -ba zsh/.config/zsh/path.zsh zsh/.config/zsh/proxy.zsh | sed -n '1,120p'
```

Expected: output shows platform-specific SDK paths and proxy settings mixed into generic files.

**Step 2: Move host and local differences**

- Put OS-only config into `host/darwin.zsh` and `host/linux.zsh`.
- Put machine-only config into `host/<hostname>.zsh`.
- Replace checked-in local override content with an example file and conditional sourcing.

**Step 3: Run syntax verification**

Run:

```bash
zsh -n zsh/.config/zsh/host/*.zsh zsh/.zshrc
```

Expected: exit status `0`.

**Step 4: Commit**

```bash
git add zsh/.config/zsh/host zsh/.config/zsh/local zsh/.config/zsh/proxy.zsh zsh/.zshrc
git commit -m "refactor: isolate host-specific zsh overrides"
```

### Task 7: Final verification and cleanup

**Files:**
- Modify: `Makefile`
- Modify: `zsh/.profile`
- Modify: `zsh/.config/zsh/exports.zsh`
- Modify: `zsh/.config/zsh/path.zsh`
- Modify: `zsh/.config/zsh/alias.zsh`
- Modify: `zsh/.config/zsh/key-bindings.zsh`

**Step 1: Verify the final startup chain**

Run:

```bash
sh -n profile/.profile profile/.config/profile.d/*.sh
zsh -n zsh/.zshenv zsh/.zprofile zsh/.zshrc zsh/.config/zsh/**/*.zsh
ZDOTDIR="$PWD/zsh" zsh -i -c exit
```

Expected: all syntax checks exit `0`, and interactive startup exits cleanly.

**Step 2: Verify installation path**

Run:

```bash
make install
```

Expected: stow completes without new conflicts for `profile` and `zsh`.

**Step 3: Review remaining obsolete files**

- Remove or shrink legacy files once their logic has moved.
- Keep compatibility wrappers only where they reduce migration risk.

**Step 4: Commit**

```bash
git add Makefile profile zsh
git commit -m "refactor: finalize zsh configuration layout"
```
