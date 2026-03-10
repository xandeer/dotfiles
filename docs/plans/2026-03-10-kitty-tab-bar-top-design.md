# Kitty Tab Bar Top Design

## Goal

Move the kitty tab bar from the bottom edge of the window to the top edge.

## Decision

Use kitty's built-in `tab_bar_edge` setting and keep all other tab-bar behavior unchanged.

- Add `tab_bar_edge top` to [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-tab-bar-top/config/.config/kitty/kitty.conf).
- Keep the existing tab title template, watcher behavior, margins, and styling as-is.
- Extend the existing kitty regression test so this placement stays locked in.

## Verification

- Read the `tab_bar_edge` line in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-tab-bar-top/config/.config/kitty/kitty.conf).
- Run [tests/config/kitty-config-regression.zsh](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-tab-bar-top/tests/config/kitty-config-regression.zsh) and confirm it passes.
