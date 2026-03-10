# Kitty Tab Ctrl Shortcuts Design

## Goal

Change kitty's direct tab shortcuts from `alt+1..4` to `ctrl+1..9`.

## Decision

Use direct `goto_tab` mappings in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf) and remove the previous `alt+1..4` bindings.

- Add `ctrl+1` through `ctrl+9`.
- Do not keep the old `alt` bindings.
- Do not introduce a multi-key leader sequence.

## Verification

- Read the updated `map` lines in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf).
- Confirm the file contains `ctrl+1..9` and no remaining `alt+1..4` tab mappings.
