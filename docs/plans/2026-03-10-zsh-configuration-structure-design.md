# Zsh Configuration Structure Design

## Goal

Reorganize the repository's zsh configuration so that shell startup responsibilities are explicit, host-specific behavior is isolated, and interactive-only features do not leak into shared shell entry points.

## Current Problems

- `zsh/.profile` currently sources zsh-only fragments, so any `sh`-style consumer can fail on `alias -g`, `zle`, `bindkey`, and `[[ ... ]]`.
- `zsh/.zshenv` performs heavy initialization (`brew`, `fnm`) that should not run in every zsh process.
- `zsh/.zshrc` mixes plugin bootstrapping, prompt setup, key bindings, aliases, and host-specific behavior in a single file.
- Platform and machine-specific settings are mixed into generic files, which makes the configuration harder to port and debug.
- Several interactive aliases assume replacement commands exist, which can break baseline commands such as `ping` and `du`.

## Design Principles

- Keep POSIX-shell-compatible configuration separate from zsh-specific configuration.
- Keep login-shell initialization separate from interactive-shell behavior.
- Keep host-specific and local overrides out of shared, generic files.
- Treat plugin management as a replaceable layer rather than the center of the startup graph.
- Default to minimal startup in `.zshenv`; load heavier integrations later.

## Recommended Repository Structure

Split shared shell environment and zsh-specific behavior into two stow packages.

```text
profile/
  .profile
  .config/profile.d/
    env.sh
    path.sh
    darwin.sh
    linux.sh
    local.sh

zsh/
  .zshenv
  .zprofile
  .zshrc
  .config/zsh/
    core/
      helpers.zsh
      source.zsh
    login/
      brew.zsh
      nix.zsh
      fnm.zsh
    plugins/
      zplug.zsh
      bundles.zsh
    interactive/
      aliases.zsh
      key-bindings.zsh
      completion.zsh
      history.zsh
      prompt.zsh
    host/
      darwin.zsh
      linux.zsh
      <hostname>.zsh
    local/
      overrides.zsh.example
```

## Responsibility Boundaries

### `profile/`

- Only POSIX-compatible shell code.
- Shared environment variables and path logic that should work for `sh`, `bash`, login managers, and scripts.
- No zsh-specific syntax or interactive shell constructs.

### `zsh/.zshenv`

- Minimal bootstrap for every zsh process.
- Safe variables or helper sourcing only.
- No plugin manager, no prompt, no `brew shellenv`, no `fnm env`.

### `zsh/.zprofile`

- Login-shell initialization for zsh.
- Reads shared shell environment and then loads zsh-specific login setup.
- Appropriate home for `nix`, `brew`, and `fnm`.

### `zsh/.zshrc`

- Interactive zsh orchestration only.
- Sources interactive behavior, plugin layer, prompt, aliases, completion, and key bindings.
- Should stay small and declarative.

### `zsh/.config/zsh/host/`

- Platform-specific and machine-specific overrides.
- `darwin.zsh` and `linux.zsh` hold OS differences.
- `<hostname>.zsh` holds one-machine exceptions such as proxies or local SDK paths.

### `zsh/.config/zsh/local/`

- Optional local overrides or templates.
- Use checked-in examples and source real local files conditionally.

## Mapping From Current Files

- `zsh/.profile`
  - Shrink to POSIX-compatible loading of `~/.config/profile.d/*.sh`.
- `zsh/.zshenv`
  - Reduce to minimal zsh bootstrap.
- `zsh/.zshrc`
  - Convert into a small orchestrator that sources structured fragments.
- `zsh/.config/zsh/exports.zsh`
  - Split between `profile.d/env.sh` and optional local auth loading with file existence checks.
- `zsh/.config/zsh/path.zsh`
  - Split between shared path setup and host-specific files.
- `zsh/.config/zsh/alias.zsh`
  - Move to `interactive/aliases.zsh`.
- `zsh/.config/zsh/key-bindings.zsh`
  - Move to `interactive/key-bindings.zsh` with terminfo guards.
- `zsh/.config/zsh/proxy.zsh`
  - Move to `host/<hostname>.zsh` or `local/`.
- `zsh/.zshrc` zplug block
  - Split into `plugins/zplug.zsh` and `plugins/bundles.zsh`.

## Placement Rules For Future Changes

- Shared shell environment goes in `profile/.config/profile.d/*.sh`.
- Zsh login-only initialization goes in `zsh/.zprofile` or `zsh/.config/zsh/login/*.zsh`.
- Interactive-only behavior goes in `zsh/.config/zsh/interactive/*.zsh`.
- OS differences go in `zsh/.config/zsh/host/darwin.zsh` and `linux.zsh`.
- One-machine behavior goes in `zsh/.config/zsh/host/<hostname>.zsh`.
- Secrets and local-only state go in `local/` or encrypted files and must be sourced conditionally.
- Plugin declaration and plugin bootstrap must stay under `plugins/`.

## Non-Negotiable Guardrails

- `.profile` must stay POSIX-compatible.
- `.zshenv` must remain minimal and side-effect-light.
- `.zshrc` must not carry secrets or machine-specific hard-coded paths.
- Generic files must not contain hostname-specific logic.
- Aliases that replace standard commands must check that the replacement exists.

## Expected Outcome

After the refactor, shell startup behavior should be predictable:

1. `profile/.profile` loads shared shell environment.
2. `zsh/.zprofile` performs zsh login initialization.
3. `zsh/.zshrc` loads interactive behavior.
4. Host and local layers apply only the differences they own.

This structure keeps the current dotfiles framework, remains compatible with GNU Stow, and leaves the plugin layer replaceable in a future cleanup.
