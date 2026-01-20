# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture
- **Type**: Modular Emacs configuration.
- **Entry Point**: `init.el` loads modules and sets up the environment. `early-init.el` handles startup optimizations (GC, UI).
- **Modules**: Located in `lisp/`, prefixed with `x-` (e.g., `x-git.el`, `x-org.el`). Each module handles a specific domain.
- **Package Management**: Uses `straight.el`. Packages are defined in `lisp/x-packages.el` and installed via the `x/package-use` wrapper.
- **State Management**: Uses `no-littering` to keep `~/.emacs.d` clean. Persistent data goes to `var/`, configuration to `etc/`.
- **Local Config**: `local.el` and `private.el` (symlink) are used for machine-specific or private settings.

## Development & Commands
- **Reload Config**: Run `M-x x/load-init-session` or restart Emacs.
- **Install Package**: Add to `x/packages` list in `lisp/x-packages.el` and reload/restart.
- **Evaluate Code**: Use `C-x C-e` (eval-last-sexp) or `M-x eval-buffer` to test changes immediately.
- **Linting**: `flycheck` is enabled for on-the-fly linting.
- **Testing**: No formal test suite. Test changes by evaluating code in the running Emacs session.

## Code Style & Conventions
- **Prefix**: Use `x-` or `x/` prefix for all custom functions, variables, and files to avoid namespace collisions.
- **Lexical Binding**: Ensure `;;; -*- lexical-binding: t -*-` is at the top of every file.
- **Lazy Loading**: Use `with-eval-after-load` to configure packages to minimize startup time.
- **Package Declaration**: Use `x/package-use` in `lisp/x-packages.el` instead of `straight-use-package` directly.
- **Keybindings**: Use `x/define-keys` or `transient` for custom key definitions.
