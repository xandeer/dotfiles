# Nushell Stow Management Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move Nushell configuration management from Home Manager to the existing GNU Stow-managed `config` package without changing current shell behavior.

**Architecture:** Add repository-managed Nushell config files under `config/Library/Application Support/nushell/`, migrate the current generated config payload into those files, and replace the Home Manager Nushell module with plain Nix package installation. Verification uses a stow dry run plus direct `nu` startup against the checked-in config files.

**Tech Stack:** GNU Stow, Nushell, Nix Home Manager, zoxide, oh-my-posh

---

### Task 1: Add a regression test for the stow-managed Nushell path

**Files:**
- Create: `tests/config/nushell-stow-regression.zsh`
- Read: `Makefile`

**Step 1: Write the failing regression test**

- Add a test that stows the `config` package into a temporary `$HOME`.
- Assert that `"$HOME/Library/Application Support/nushell/config.nu"` is a symlink.
- Assert that `"$HOME/Library/Application Support/nushell/env.nu"` is a symlink.
- Assert that the symlink targets point back into the repository `config` package.

**Step 2: Run the test to verify it fails**

Run:

```bash
zsh tests/config/nushell-stow-regression.zsh
```

Expected: exit status `1` because the Nushell files do not exist in the `config` stow package yet.

### Task 2: Add stow-managed Nushell config files

**Files:**
- Create: `config/Library/Application Support/nushell/config.nu`
- Create: `config/Library/Application Support/nushell/env.nu`
- Read: `config/.config/env.nu`

**Step 1: Create repository-managed `config.nu`**

- Copy the current theme, PATH setup, `fnm` import, prompt logic, completions, hooks, aliases, and `source ~/.config/env.nu` into the new file.

**Step 2: Create repository-managed `env.nu`**

- Generate the cached `zoxide` and `oh-my-posh` init scripts into `~/.cache/...`.
- Source those cached init files from the checked-in `env.nu`.

**Step 3: Run syntax/startup verification**

Run:

```bash
zsh tests/config/nushell-stow-regression.zsh
nu --config '/Users/kevin/projects/personal/dotfiles/config/Library/Application Support/nushell/config.nu' --env-config '/Users/kevin/projects/personal/dotfiles/config/Library/Application Support/nushell/env.nu' -c 'version | get version'
```

Expected: the regression test passes and Nushell exits `0`.

### Task 3: Remove Home Manager ownership and keep Nix installation

**Files:**
- Modify: `nix/home/default.nix`
- Modify: `nix/home/home-pkgs.nix`
- Delete: `nix/home/nushell.nix`

**Step 1: Stop importing the Nushell Home Manager module**

- Remove `./nushell.nix` from `nix/home/default.nix`.

**Step 2: Keep package installation via Nix**

- Add `nushell`, `zoxide`, and `oh-my-posh` to `nix/home/home-pkgs.nix`.

**Step 3: Delete the obsolete Home Manager module**

- Remove `nix/home/nushell.nix`.

**Step 4: Verify no Home Manager Nushell ownership remains**

Run:

```bash
rg -n "programs\\.(nushell|zoxide|oh-my-posh)|nushell\\.nix" /Users/kevin/projects/personal/dotfiles/nix/home
```

Expected: no matches.

### Task 4: Run final migration verification

**Files:**
- Read: `config/Library/Application Support/nushell/config.nu`
- Read: `config/Library/Application Support/nushell/env.nu`
- Read: `tests/config/nushell-stow-regression.zsh`

**Step 1: Verify the stow install preview**

Run:

```bash
tmp_home="$(mktemp -d)"
HOME="$tmp_home" stow -nv -d /Users/kevin/projects/personal/dotfiles -t "$tmp_home" config
```

Expected: output shows the Nushell files would link into `"$tmp_home/Library/Application Support/nushell/"`.

**Step 2: Verify the checked-in Nushell config can start**

Run:

```bash
HOME="$(mktemp -d)" nu --config '/Users/kevin/projects/personal/dotfiles/config/Library/Application Support/nushell/config.nu' --env-config '/Users/kevin/projects/personal/dotfiles/config/Library/Application Support/nushell/env.nu' -c 'version | get version'
```

Expected: exit status `0`.

**Step 3: Review the final diff**

Run:

```bash
git -C /Users/kevin/projects/personal/dotfiles diff -- config/Library\Application\ Support/nushell nix/home tests/config docs/plans
```

Expected: diff shows the new stow-managed Nushell files, the Nix package-only changes, the regression test, and the design/plan docs.
