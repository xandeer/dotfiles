# Kitty Tab Index Titles Design

## Goal

Show each kitty tab number at the front of its tab title.

## Decision

Use kitty's `tab_title_template` in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf) and prepend `{index}` to the visible tab title.

- Keep the existing `ctrl+1..9` bindings aligned with visible tab numbers.
- Keep kitty's bell and activity indicators in the title template.
- Do not add scripting or dynamic modifier-based title changes.

## Verification

- Read the new `tab_title_template` line in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf).
- Confirm it contains `{index}` ahead of `{title}`.
