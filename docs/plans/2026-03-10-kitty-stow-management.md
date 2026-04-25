# Kitty Stow Management Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move kitty configuration management from Home Manager to the existing GNU Stow-managed `config` package without changing current kitty behavior.

**Architecture:** Add a checked-in kitty config tree under `config/.config/kitty/`, split the theme into a local include file, and remove the Home Manager import that generates kitty's config. Verification relies on kitty's config parser plus a `stow` dry run against `$HOME`.

**Tech Stack:** GNU Stow, kitty, Nix Home Manager, repository `config` stow package

---

### Task 1: Capture the currently effective kitty configuration

**Files:**
- Read: `~/.config/kitty/kitty.conf`
- Read: `/nix/store/.../AtomOneLight.conf`

**Step 1: Record the generated config**

Run:

```bash
sed -n '1,220p' ~/.config/kitty/kitty.conf
```

Expected: output shows the current settings and an `include` of the Nix store `AtomOneLight.conf`.

**Step 2: Record the theme payload**

Run:

```bash
sed -n '1,220p' /nix/store/.../share/kitty-themes/themes/AtomOneLight.conf
```

Expected: output shows the color values that need to move into the repository.

### Task 2: Add stow-managed kitty config files

**Files:**
- Create: `config/.config/kitty/kitty.conf`
- Create: `config/.config/kitty/themes/AtomOneLight.conf`

**Step 1: Write the repository-managed config**

- Copy the active kitty settings into `config/.config/kitty/kitty.conf`.
- Preserve `shell_integration no-rc`, shell/editor settings, and key mappings.
- Replace the Nix store include with a repository-local theme include.

**Step 2: Add the local theme file**

- Copy the current `AtomOneLight` theme color definitions into `config/.config/kitty/themes/AtomOneLight.conf`.

**Step 3: Verify kitty can parse the new config**

Run:

```bash
kitty --config /Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf --debug-config
```

Expected: kitty loads the config successfully and resolves the local include without errors.

### Task 3: Remove Home Manager ownership of kitty config

**Files:**
- Modify: `nix/home/default.nix`
- Delete: `nix/home/kitty.nix`

**Step 1: Remove the import**

- Delete `./kitty.nix` from `nix/home/default.nix`.

**Step 2: Remove the obsolete module**

- Delete `nix/home/kitty.nix` because kitty config is no longer managed there.

**Step 3: Verify no Home Manager references remain**

Run:

```bash
rg -n "kitty\\.nix|programs\\.kitty" /Users/kevin/projects/personal/dotfiles/nix
```

Expected: no matches for `kitty.nix` import or `programs.kitty`.

### Task 4: Verify the stow installation path

**Files:**
- Read: `Makefile`
- Read: `config/.config/kitty/kitty.conf`

**Step 1: Dry-run the `config` stow package**

Run:

```bash
stow -nv -d /Users/kevin/projects/personal/dotfiles -t "$HOME" config
```

Expected: output shows that `config/.config/kitty/kitty.conf` would be linked into `~/.config/kitty/kitty.conf`, or reports the existing Home Manager symlink as the only conflict to resolve during install.

**Step 2: Review the final diff**

Run:

```bash
git -C /Users/kevin/projects/personal/dotfiles diff -- config/.config/kitty nix/home/default.nix docs/plans
```

Expected: diff contains the new stow-managed kitty files, the design/plan docs, and the removal of the Home Manager kitty import/module.
