# Nushell Startup Optimization Design

## Goal

Reduce avoidable Nushell startup overhead without intentionally changing interactive behavior.

## Current State

- `config/.config/nushell/modules/fnm.nu` runs `fnm env --shell bash`, parses the result, and prints debug output on every shell startup.
- `config/.config/nushell/env.nu` rewrites the cached `zoxide` and `oh-my-posh` init scripts on every shell startup.
- `config/.config/nushell/modules/path.nu` prepends fixed path entries to the inherited `PATH`, which allows duplicates to accumulate across nested shells.
- The current tests verify the stow layout and basic startup, but do not assert cache preservation, silent startup, or PATH de-duplication.

## Constraints

- Keep behavior as close as possible to the current configuration.
- Do not change the module load order in `config/.config/nushell/config.nu`.
- Do not modify prompt, hook, completion, or alias behavior.
- Do not introduce a more complex cache invalidation policy; only avoid unnecessary rewrites.
- Do not intentionally change `fnm` behavior beyond removing debug prints and handling missing `PATH` data safely.

## Chosen Approach

Make three narrow changes:

1. `fnm.nu`
   - Remove startup debug `print` calls.
   - Keep the existing `fnm env --shell bash` parsing pipeline.
   - If the parsed result does not contain `PATH`, skip only the PATH append and continue loading the remaining `FNM_*` variables.

2. `path.nu`
   - Keep the current prefix order.
   - Append the inherited `PATH` as today.
   - De-duplicate entries while preserving first occurrence order.

3. `env.nu`
   - Keep the same cache file locations.
   - Generate each cache file only when it is missing.

## Rejected Alternatives

### Aggressive lazy loading

Rejected because deferring `fnm`, completions, or hooks would change when features become available and risks behavior drift.

### Hash- or mtime-based cache refresh

Rejected because it adds more moving parts than this optimization needs. The goal is only to stop rewriting known cache files on every startup.

### Reworking `fnm` parsing entirely

Rejected because the current behavior already works. This change only needs to stop noisy output and avoid a hard failure when `PATH` is missing from the parsed data.

## Verification

The optimization is correct when all of the following are true:

1. `tests/config/nushell-stow-regression.zsh` still passes.
2. `tests/config/nushell-startup-smoke.zsh` still passes.
3. A smoke test with pre-existing cache files confirms Nushell startup does not rewrite them.
4. Startup stdout no longer contains the `Adding FNM...` debug messages.
5. The evaluated `PATH` contains only one copy of repeated entries while preserving order.

## Risks

- `fnm` parsing is still string-based, so the fail-soft behavior must stay narrowly scoped to the missing `PATH` case.
- PATH de-duplication must keep the first occurrence to avoid changing precedence.
- Cache generation must remain compatible with the existing `source ~/.cache/.../init.nu` statements in `config.nu`.
