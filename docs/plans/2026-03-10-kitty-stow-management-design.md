# Kitty Stow Management Design

## Goal

Move kitty configuration management out of Home Manager and into the repository's existing GNU Stow workflow, while preserving the currently effective kitty behavior and theme.

## Current State

- [nix/home/default.nix](/Users/kevin/projects/personal/dotfiles/nix/home/default.nix) imports [nix/home/kitty.nix](/Users/kevin/projects/personal/dotfiles/nix/home/kitty.nix).
- Home Manager currently generates `~/.config/kitty/kitty.conf`.
- The generated config pulls the `AtomOneLight` theme from a Nix store path and adds one extra generated line: `shell_integration no-rc`.
- The repository already uses the `config/` stow package for other `~/.config/*` paths, so kitty belongs there.

## Decision

Store kitty in the existing `config` stow package at `config/.config/kitty/`.

Use two files:

- `config/.config/kitty/kitty.conf` for the main kitty settings and key mappings
- `config/.config/kitty/themes/AtomOneLight.conf` for the theme colors currently pulled from the Nix store

This keeps the repository layout consistent with the rest of the dotfiles, avoids introducing a new stow package, and removes runtime dependence on a Home Manager generated config file.

## Migration Steps

1. Copy the currently effective kitty settings into `config/.config/kitty/kitty.conf`.
2. Copy the current `AtomOneLight` theme colors into a local theme file inside the repository.
3. Remove the kitty import from `nix/home/default.nix`.
4. Remove the now-unused `nix/home/kitty.nix`.
5. Verify the new config with kitty's config parser and with a `stow` dry run.

## Non-Goals

- Do not change how kitty itself is installed.
- Do not remove `kitty-themes` or other non-config Nix packages.
- Do not redesign the current kitty key bindings or shell/editor settings.

## Expected Outcome

After `stow` installs the `config` package, `~/.config/kitty/kitty.conf` should point into this repository instead of a Home Manager generated Nix store path, and kitty should continue using the same theme and settings as before.
