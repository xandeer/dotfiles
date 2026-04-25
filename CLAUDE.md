# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architecture
- **Type**: Dotfiles repository managed by GNU Stow.
- **Management**: Uses `make` and `stow` to symlink configurations to the home directory.
- **Encryption**: Sensitive files are encrypted using GPG and managed via the `bin/bin/dots` script.
- **Structure**:
  - Top-level directories (e.g., `zsh`, `emacs.d`, `git`) correspond to stow packages.
  - `emacs.d/.emacs.d/`: Contains the Emacs configuration (has its own `CLAUDE.md`).
  - `bin/`: Contains utility scripts (stowed to `~/bin`).
  - `Makefile`: Entry point for installation and management.

## Commands
- **Install**: `make install` - Stows all configurations to `~`.
- **Update**: `make update` - Pulls latest changes and runs install.
- **Encrypt**: `make endot` - Encrypts sensitive files (requires `bin/dots`).
- **Decrypt**: `make dedot` - Decrypts sensitive files.
- **Help**: `make help` - Shows available make targets.

## Development
- **New Config**: To add a new configuration:
  1. Create a directory (e.g., `mytool`).
  2. Place config files inside, mimicking the home directory structure (e.g., `mytool/.config/mytool/config`).
  3. Add the directory name to `CONFIGS` in `Makefile`.
  4. Run `make install`.
- **Emacs**: Refer to `emacs.d/.emacs.d/CLAUDE.md` for Emacs-specific development.
