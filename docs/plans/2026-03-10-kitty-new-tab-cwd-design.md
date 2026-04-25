# Kitty New Tab CWD Design

## Goal

Make new kitty tabs start in the working directory of the currently active tab.

## Decision

Use kitty's built-in cwd-aware tab actions instead of shell wrappers or menu-only additions.

- Override the default new-tab shortcuts in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-new-tab-cwd/config/.config/kitty/kitty.conf) so `cmd+t` and `ctrl+shift+t` run `new_tab_with_cwd`.
- Treat any explicit tab-creation action in the repository as valid only if it uses `new_tab_with_cwd` or `launch --type=tab --cwd=current`.
- Keep the existing login-shell and title-watcher behavior unchanged.
- Do not try to rewrite the built-in macOS `Shell > New Tab` menu item, because kitty 0.39.1 documents `menu_map` as a way to add global menubar entries, not override existing ones.

## Verification

- Read the new-tab mappings in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-new-tab-cwd/config/.config/kitty/kitty.conf).
- Read the regression guard in [tests/config/kitty-config-regression.zsh](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-new-tab-cwd/tests/config/kitty-config-regression.zsh).
- Confirm the test enforces `cmd+t` and `ctrl+shift+t` mapping to `new_tab_with_cwd`.
- Confirm the test rejects any repository-managed `launch --type=tab` action that does not also set `--cwd=current`.
