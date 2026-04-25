# Kitty Title Hide Prompt Path Design

## Goal

Keep the existing git-repository-aware kitty title behavior, but stop repeating the shell working directory path when the child title already includes it.

## Current State

- [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf) keeps the tab format as `{index}: {title}` and delegates title rewriting to [config/.config/kitty/title_watcher.py](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/title_watcher.py).
- The watcher currently rewrites child titles into `repo | title`.
- When Nushell sets a prompt-style title like `~/projects/personal/remio> codex`, the watcher preserves that entire string, which produces `remio | ~/projects/personal/remio> codex`.
- This repeats path information that is already implied by the repository label.

## Decision

Normalize shell prompt-style child titles before composing the final kitty title.

If the child title clearly matches a prompt-style shape such as `PATH> COMMAND` or `/absolute/path> COMMAND`, the watcher should keep only the command segment and drop the leading path. The existing repo label, unread notification marker, tab index, and fallback behavior should remain unchanged.

## Scope

- Modify only [config/.config/kitty/title_watcher.py](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/title_watcher.py) and [tests/config/kitty-config-regression.zsh](/Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh).
- Preserve the current `repo | title` composition for non-prompt titles.
- Preserve the existing `[codex]` and `[claude]` unread markers.

## Non-Goals

- Do not change the tab title template in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf).
- Do not change how repository names are discovered.
- Do not add shell-specific behavior beyond stripping the duplicated prompt path prefix.

## Testing

Extend [tests/config/kitty-config-regression.zsh](/Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh) with a failing expectation that `compose_window_title(repo_path, "~/projects/personal/remio> codex")` yields `repo | codex`, then rerun the existing kitty regression test to verify the full watcher contract still passes.
