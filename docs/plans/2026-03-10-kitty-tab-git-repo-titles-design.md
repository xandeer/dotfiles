# Kitty Tab Git Repo Titles Design

## Goal

Show the active git repository name in kitty titles while keeping the existing tab index and command-aware title behavior.

## Decision

Use kitty's window-title watcher path instead of customizing the tab bar renderer.

- Keep the current tab index prefix from [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf).
- Keep the existing `tab_title_template` in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf) so tabs continue rendering whatever kitty sees as the current window title.
- Add `watcher title_watcher.py` in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf).
- Add a repository-aware watcher in `config/.config/kitty/title_watcher.py`.
- Resolve the display name from the active window working directory:
  - Show the git repository root directory name when the active directory is inside a git worktree.
  - Fall back to the active directory basename when no git repository is detected.
- Update the visible window title when child processes change it, so the same `repo | command` text shows in both the macOS window title and kitty tab titles.

## Verification

- Read the `watcher` and `tab_title_template` lines in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf).
- Read the watcher in [config/.config/kitty/title_watcher.py](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/title_watcher.py).
- Confirm the watcher derives a repository label from the active working directory and prefixes it onto child-provided titles before calling `window.set_window_title(...)`.
