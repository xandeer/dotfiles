# Nushell Config Simplification Design

## Goal

Simplify the Nushell configuration layout so that the real configuration lives under `~/.config/nushell`, while macOS's `~/Library/Application Support/nushell` remains only a thin compatibility entrypoint.

## Current State

- The repository currently stores the checked-in Nushell runtime files at `config/Library/Application Support/nushell/config.nu` and `config/Library/Application Support/nushell/env.nu`.
- `config.nu` mixes theme setup, PATH assembly, `fnm` import, prompt setup, completions, hooks, aliases, and sourcing of `~/.config/env.nu` in one file.
- `env.nu` generates cached init scripts for `zoxide` and `oh-my-posh`.
- The existing regression test only verifies that Stow exposes the `Library/Application Support/nushell` files.

## Constraints

- Keep behavior unchanged. This is a structure-only simplification.
- Keep `~/Library/Application Support/nushell/config.nu` and `env.nu` as the Nushell entrypoint on macOS.
- Move the main checked-in configuration to `config/.config/nushell/`.
- Do not set global `XDG_CONFIG_HOME`.
- Do not rewrite the config into a different abstraction style such as `use`/`export` modules.
- Preserve the current load order for PATH, prompt, completions, hooks, aliases, and `source ~/.config/env.nu`.

## Chosen Approach

Use a two-layer layout:

```text
config/
  .config/
    nushell/
      config.nu
      env.nu
      modules/
        theme.nu
        path.nu
        fnm.nu
        prompt.nu
        completions.nu
        hooks.nu
        aliases.nu
  Library/
    Application Support/
      nushell/
        config.nu
        env.nu
```

The `Library/Application Support/nushell` files become thin forwarders:

- `config.nu` only sources `~/.config/nushell/config.nu`
- `env.nu` only sources `~/.config/nushell/env.nu`

All real logic moves into `config/.config/nushell/`.

## Runtime Model

### Compatibility entrypoints

The macOS-native Nushell entrypoints stay in place so Nushell can continue starting from its default directory. They do not contain any runtime logic beyond forwarding to the XDG-style config tree.

### Main config

`config/.config/nushell/config.nu` becomes a thin assembler that:

1. derives its directory from `$nu.config-path`
2. sources each file in `modules/` in a fixed order
3. keeps `source ~/.config/env.nu` near the end of the chain

Using `$nu.config-path | path dirname` keeps the module loading stable even when the file is invoked with `nu --config <repo-path>`.

### Main env config

`config/.config/nushell/env.nu` keeps the current cache-generation behavior for `zoxide` and `oh-my-posh`. It remains separate from the interactive shell configuration.

### Modules

- `theme.nu`: theme table and `$env.config` color configuration
- `path.nu`: PATH assembly
- `fnm.nu`: `fnm env --shell bash` import
- `prompt.nu`: `oh-my-posh` prompt variables and prompt closures
- `completions.nu`: completion `use` statements
- `hooks.nu`: `direnv` and `nuenv` hooks
- `aliases.nu`: alias definitions

The modules continue using `source` semantics so they can preserve the current top-level side effects without being redesigned.

## Rejected Alternatives

### Keep everything in `Library/Application Support/nushell`

Rejected because it keeps the main runtime logic in a macOS-specific path and does not simplify the long-term structure.

### Move the main config to `~/.config/nushell` and remove `Library/Application Support/nushell`

Rejected because the user explicitly wants the macOS directory to remain as the Nushell entrypoint.

### Use a new export-based module system

Rejected because it changes the implementation model rather than only simplifying the structure.

## Verification

The change is correct when all of the following are true:

1. Stowing `config` exposes both `~/.config/nushell/*` and `~/Library/Application Support/nushell/*`.
2. The `Library/Application Support/nushell` files are thin forwarders to `~/.config/nushell`.
3. Running `nu --config <repo>/config/.config/nushell/config.nu --env-config <repo>/config/.config/nushell/env.nu -c 'version | get version'` exits successfully on the current machine.
4. The real runtime logic only exists under `config/.config/nushell/`.

## Risks

- The main config must load modules in a way that works both after Stow installation and when invoked directly from the repository.
- Any accidental content change while splitting the file would violate the structure-only constraint.
- Startup verification will still depend on the existing machine-local paths referenced by the current config, such as `~/projects/others/...` and `~/.config/env.nu`.
