# Kitty Shell Nu Design

## Goal

Change kitty's default interactive shell from login zsh to login Nushell.

## Decision

Update kitty's `shell` setting only, and preserve the rest of the existing kitty behavior.

- Replace `shell /bin/zsh -l` in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-shell-nu/config/.config/kitty/kitty.conf) with `shell /etc/profiles/per-user/kevin/bin/nu -l`.
- Keep the existing tab bar placement, title watcher, agent notifications, new-tab cwd behavior, and config-edit shortcut unchanged.
- Lock the shell path and login flag into the existing kitty regression test.

## Verification

- Read the `shell` line in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-shell-nu/config/.config/kitty/kitty.conf).
- Run [tests/config/kitty-config-regression.zsh](/Users/kevin/projects/personal/dotfiles/.worktrees/kitty-shell-nu/tests/config/kitty-config-regression.zsh) and confirm it passes.
