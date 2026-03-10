# Tmux Configuration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a Stow-managed tmux package with modular configuration, manual session helpers, and a small stable plugin set for the current macOS workflow.

**Architecture:** Create a dedicated `tmux/` Stow package that exposes `~/.tmux.conf` as a minimal bootstrap into `~/.config/tmux/`. Split the real configuration into ordered `conf.d` fragments, keep TPM loading optional until bootstrapped, and verify the whole package by Stow-installing it into an isolated temporary home before touching the real home directory.

**Tech Stack:** GNU Stow, tmux 3.6a, POSIX shell, macOS clipboard integration via `pbcopy`

---

### Task 1: Add the tmux package scaffold

**Files:**
- Modify: `.gitignore`
- Modify: `Makefile`
- Create: `tmux/.tmux.conf`
- Create: `tmux/.config/tmux/tmux.conf`
- Create: `tmux/.config/tmux/conf.d/00-options.conf`
- Create: `tmux/.config/tmux/conf.d/10-keybindings.conf`
- Create: `tmux/.config/tmux/conf.d/20-windows.conf`
- Create: `tmux/.config/tmux/conf.d/30-copy-mode.conf`
- Create: `tmux/.config/tmux/conf.d/40-status.conf`
- Create: `tmux/.config/tmux/conf.d/50-plugins.conf`
- Create: `tmux/.config/tmux/conf.d/90-local.conf.example`
- Create: `tmux/.config/tmux/scripts/sessionizer`

**Step 1: Confirm the package does not exist yet**

Run:

```bash
test -d /Users/kevin/projects/personal/dotfiles/tmux
```

Expected: exit status `1` because the repository does not have a `tmux/` package yet.

**Step 2: Create the scaffold**

- Add `tmux` to `CONFIGS` in `Makefile`.
- Add `.gitignore` coverage for `tmux/.config/tmux/conf.d/90-local.conf`.
- Create `tmux/.tmux.conf` so it only sources `~/.config/tmux/tmux.conf`.
- Create `tmux/.config/tmux/tmux.conf` so it sources the ordered `conf.d/*.conf` files and conditionally loads `90-local.conf`.
- Create the `conf.d/` files as comment-backed placeholders, plus a stub `sessionizer` shell script.

**Step 3: Verify the Stow package boundary**

Run:

```bash
stow -nv -d /Users/kevin/projects/personal/dotfiles -t "$HOME" tmux
```

Expected: output shows links for `~/.tmux.conf` and `~/.config/tmux/...` without unexpected path conflicts.

**Step 4: Verify the local override file is ignored**

Run:

```bash
git check-ignore -v /Users/kevin/projects/personal/dotfiles/tmux/.config/tmux/conf.d/90-local.conf
```

Expected: output points at the new `.gitignore` rule for `90-local.conf`.

**Step 5: Commit**

```bash
git add .gitignore Makefile tmux
git commit -m "feat: add tmux stow package scaffold"
```

### Task 2: Implement core tmux behavior

**Files:**
- Modify: `tmux/.config/tmux/tmux.conf`
- Modify: `tmux/.config/tmux/conf.d/00-options.conf`
- Modify: `tmux/.config/tmux/conf.d/10-keybindings.conf`
- Modify: `tmux/.config/tmux/conf.d/20-windows.conf`
- Modify: `tmux/.config/tmux/conf.d/30-copy-mode.conf`

**Step 1: Capture the placeholder state**

Run:

```bash
sed -n '1,220p' /Users/kevin/projects/personal/dotfiles/tmux/.config/tmux/conf.d/00-options.conf
```

Expected: output shows only placeholder comments or minimal scaffolding.

**Step 2: Add the core configuration**

- In `00-options.conf`, set the base behavior: prefix key, `escape-time`, history limit, mouse support, indexing, renumbering, default shell, and terminal feature flags appropriate for macOS terminals.
- In `10-keybindings.conf`, add bindings for reload, splitting panes, resizing, switching windows, and toggling synchronized panes.
- In `20-windows.conf`, set predictable window naming, pane-border styling, and starting-directory behavior.
- In `30-copy-mode.conf`, switch to vi-style copy mode and wire copied text to `pbcopy`.
- Keep `tmux.conf` as an ordered loader only; do not bury configuration in the loader.

**Step 3: Verify the config in an isolated home**

Run:

```bash
tmp_home="$(mktemp -d)"
stow -d /Users/kevin/projects/personal/dotfiles -t "$tmp_home" tmux
HOME="$tmp_home" tmux -L dotfiles-tmux-core -f "$tmp_home/.tmux.conf" new-session -d -s smoke
HOME="$tmp_home" tmux -L dotfiles-tmux-core show -g mouse
HOME="$tmp_home" tmux -L dotfiles-tmux-core kill-server
```

Expected: tmux starts cleanly from the Stow-installed files, `show -g mouse` reports `mouse on`, and the isolated server shuts down without config errors.

**Step 4: Commit**

```bash
git add tmux/.config/tmux/tmux.conf tmux/.config/tmux/conf.d/00-options.conf tmux/.config/tmux/conf.d/10-keybindings.conf tmux/.config/tmux/conf.d/20-windows.conf tmux/.config/tmux/conf.d/30-copy-mode.conf
git commit -m "feat: add core tmux workflow settings"
```

### Task 3: Add the workflow layer

**Files:**
- Modify: `tmux/.config/tmux/conf.d/40-status.conf`
- Modify: `tmux/.config/tmux/conf.d/50-plugins.conf`
- Modify: `tmux/.config/tmux/conf.d/90-local.conf.example`
- Modify: `tmux/.config/tmux/scripts/sessionizer`

**Step 1: Capture the empty workflow layer**

Run:

```bash
sed -n '1,220p' /Users/kevin/projects/personal/dotfiles/tmux/.config/tmux/conf.d/50-plugins.conf
```

Expected: output shows only placeholder content and no working plugin declarations yet.

**Step 2: Implement status, plugins, and session helper**

- In `40-status.conf`, add a minimal status line that shows session/window state on the left and time, host, and prefix state on the right.
- In `50-plugins.conf`, declare `tpm`, `tmux-sensible`, `vim-tmux-navigator`, and `tmux-resurrect`.
- Guard the TPM `run-shell` so tmux still starts when `~/.tmux/plugins/tpm` is absent.
- In `90-local.conf.example`, document the intended pattern for machine-only overrides.
- In `scripts/sessionizer`, accept an optional directory argument, derive a deterministic session name from the target directory, and either create/switch/attach without adding any automatic directory hooks.

**Step 3: Verify the workflow layer**

Run:

```bash
sh -n /Users/kevin/projects/personal/dotfiles/tmux/.config/tmux/scripts/sessionizer
tmp_home="$(mktemp -d)"
stow -d /Users/kevin/projects/personal/dotfiles -t "$tmp_home" tmux
HOME="$tmp_home" tmux -L dotfiles-tmux-workflow -f "$tmp_home/.tmux.conf" new-session -d -s smoke
HOME="$tmp_home" tmux -L dotfiles-tmux-workflow show -g status-right
HOME="$tmp_home" tmux -L dotfiles-tmux-workflow kill-server
```

Expected: the `sessionizer` script passes shell syntax validation, tmux starts without TPM installed, and `show -g status-right` returns the configured right-hand status template instead of the default.

**Step 4: Commit**

```bash
git add tmux/.config/tmux/conf.d/40-status.conf tmux/.config/tmux/conf.d/50-plugins.conf tmux/.config/tmux/conf.d/90-local.conf.example tmux/.config/tmux/scripts/sessionizer
git commit -m "feat: add tmux workflow and plugin layer"
```

### Task 4: Perform final verification and bootstrap notes

**Files:**
- Read: `Makefile`
- Read: `tmux/.tmux.conf`
- Read: `tmux/.config/tmux/tmux.conf`
- Read: `tmux/.config/tmux/conf.d/*.conf`
- Read: `tmux/.config/tmux/scripts/sessionizer`
- Read: `docs/plans/2026-03-10-tmux-configuration-design.md`

**Step 1: Dry-run the real home install**

Run:

```bash
stow -nv -d /Users/kevin/projects/personal/dotfiles -t "$HOME" tmux
```

Expected: output shows the repository will link the new tmux files into the real home directory cleanly.

**Step 2: Bootstrap TPM if it is not installed yet**

Run:

```bash
test -d ~/.tmux/plugins/tpm
```

Expected: exit status `0` if TPM already exists, or exit status `1` on first-time setup.

If the directory is missing, run:

```bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

Expected: the TPM checkout appears under `~/.tmux/plugins/tpm`.

**Step 3: Manual smoke-test the intended workflow**

Run:

```bash
tmux source-file ~/.tmux.conf
```

Expected: the active tmux server reloads without syntax errors.

Then verify manually inside tmux:

- create a session with `tmux new -s demo -c ~/projects` or the new `sessionizer`
- split panes, resize panes, and reload the config with the configured key bindings
- enter copy mode, copy text, and confirm it reaches the macOS clipboard
- save and restore a session through `tmux-resurrect`

**Step 4: Review the final diff**

Run:

```bash
git -C /Users/kevin/projects/personal/dotfiles diff -- Makefile .gitignore tmux docs/plans/2026-03-10-tmux-configuration-design.md docs/plans/2026-03-10-tmux-configuration.md
```

Expected: the diff contains the new `tmux` Stow package, the `Makefile` and `.gitignore` adjustments, and the matching design/plan docs only.
