# Tmux Configuration Design

## Goal

Add a systematic, Stow-managed tmux configuration for the current macOS workflow, with modular configuration files, manual session management, and a small stable plugin set.

## Current State

- [nix/darwin/homebrew.nix](/Users/kevin/projects/personal/dotfiles/nix/darwin/homebrew.nix) lists `tmux` as an installed tool, so runtime installation is already handled outside the dotfiles config tree.
- [Makefile](/Users/kevin/projects/personal/dotfiles/Makefile) does not currently include an active `tmux` package in `CONFIGS`, although an older commented example mentions one.
- The repository has no checked-in `tmux/` Stow package today.
- The current machine has no existing `~/.tmux.conf` or `~/.config/tmux/` tree to migrate, so this is a fresh structure rather than a lift-and-shift.
- This repository already uses dedicated Stow packages for top-level shell entry points and uses package-local `.config/*` trees when a tool needs a larger internal layout.

## Decision

Store tmux in a new dedicated `tmux/` Stow package, while splitting the real configuration under `~/.config/tmux/`.

Use this layout:

```text
tmux/
  .tmux.conf
  .config/tmux/
    tmux.conf
    conf.d/
      00-options.conf
      10-keybindings.conf
      20-windows.conf
      30-copy-mode.conf
      40-status.conf
      50-plugins.conf
      90-local.conf.example
    scripts/
      sessionizer
```

This keeps tmux's default entry point at `~/.tmux.conf`, while giving the repository a modular structure that can grow without turning the top-level file into a monolith.

## Configuration Boundaries

- `tmux/.tmux.conf` stays minimal and only sources `~/.config/tmux/tmux.conf`.
- `tmux/.config/tmux/tmux.conf` is the orchestrator that loads `conf.d/*.conf` in a fixed order.
- `00-options.conf` holds global tmux behavior such as prefix, history size, shell, mouse support, indexing, and default terminal capabilities.
- `10-keybindings.conf` holds pane, window, resize, reload, and sync-input bindings.
- `20-windows.conf` holds window naming, starting-directory, pane-border, and layout-related behavior.
- `30-copy-mode.conf` standardizes vi-style copy mode and macOS clipboard integration through `pbcopy`.
- `40-status.conf` owns the status line only.
- `50-plugins.conf` owns plugin declarations and guarded TPM loading only.
- `90-local.conf` is optional and untracked; the repository only provides `90-local.conf.example`.
- `scripts/sessionizer` is a manual helper for creating or attaching project sessions without adding directory-triggered automation.

## Plugin Set

Use a deliberately small plugin set:

- `tmux-plugins/tpm` for plugin management
- `tmux-plugins/tmux-sensible` for sane defaults
- `christoomey/vim-tmux-navigator` for pane navigation across tmux and Vim/Neovim
- `tmux-plugins/tmux-resurrect` for explicit session save and restore

Do not enable automatic session creation or automatic session restoration in the first version. The user asked for manual session control, so `tmux-continuum` stays out of scope unless that requirement changes later.

## Workflow

- New sessions remain manual. The normal entry point is still `tmux new -s <name> -c <dir>`, with `sessionizer` as an optional convenience wrapper.
- Session persistence is explicit. Saving and restoring uses `tmux-resurrect` commands rather than hidden background automation.
- TPM itself is not checked into this repository. The config should guard TPM loading so tmux still starts cleanly before the one-time TPM bootstrap step.
- Local machine-only tweaks belong in `90-local.conf`, which should be conditionally sourced only if the file exists.
- Stow installation should create both `~/.tmux.conf` and the `~/.config/tmux/` tree from the single `tmux/` package.

## Non-Goals

- Do not support Linux in the first version.
- Do not add automatic project-directory hooks from zsh, zoxide, or other shell tooling.
- Do not turn the first version into a large plugin stack with theme frameworks or popup toolchains.
- Do not move tmux installation into a new package manager path; only configuration ownership changes here.

## Expected Outcome

After `stow` installs the new `tmux` package, tmux should load through a minimal `~/.tmux.conf` entry point into a modular `~/.config/tmux/` tree, support a manual session-based macOS workflow, and remain easy to extend without restructuring the package again.
