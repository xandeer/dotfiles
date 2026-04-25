# Zplug Startup Optimization Design

## Goal

Reduce interactive zsh startup latency without replacing `zplug`, while preserving the current prompt, completion, history search, autosuggestion, and `scd` workflows.

## Current Baseline

- Interactive startup measured from `ZDOTDIR="$PWD/zsh" zsh -i -c exit` is currently about `1.29s` to `1.41s` in this environment.
- `zplug` is still on the hot path for every shell startup through `zsh/.config/zsh/plugins/zplug.zsh`.
- The current plugin bundle still includes many optional or duplicated plugins.
- Interactive fragments still call `zplug check "<plugin>"` after `zplug load`, which adds unnecessary plugin-manager work during every startup.

## Chosen Approach

Keep `zplug`, but narrow its responsibility and remove startup-time maintenance logic.
This first pass focuses on the highest-value hot-path work without rewriting the remaining plugin set all at once.

### 1. Remove startup-time install checks

- `zplug check` should not run on every interactive shell startup.
- Startup should assume the plugin set is already installed.
- A separate manual maintenance entry point should handle install/update checks when needed.

### 2. Trim the plugin bundle to obvious low-value and duplicated entries

Keep the plugin manager for the capabilities the current zsh config still actively uses, and avoid pruning compatibility-sensitive plugins in the same pass:

- `mafredri/zsh-async`
- `denysdovhan/spaceship-prompt`
- `hchbaw/zce.zsh`
- `plugins/scd`
- `zsh-users/zsh-completions`
- `zsh-users/zsh-autosuggestions`
- `zsh-users/zsh-syntax-highlighting`
- `zsh-users/zsh-history-substring-search`
- `Aloxaf/fzf-tab`

Remove or stop declaring plugins that are redundant, low-value, or clearly duplicated by local config:

- `plugins/nvm` because `fnm` now owns Node version management
- `plugins/common-aliases`
- `plugins/git-extras`
- `plugins/gitignore`
- `plugins/colorize`
- `plugins/yarn`
- dead `anyframe` handling
- defer broader oh-my-zsh pruning until after this measured first pass

### 3. Remove unnecessary runtime `zplug check "<plugin>"` calls

After `zplug load`, interactive configuration should guard on the actual loaded shell state instead of asking `zplug` again:

- history search key bindings should guard on the widget/function existence they need
- autosuggestion key bindings should guard on autosuggestion widgets
- prompt configuration should set prompt variables directly
- syntax-highlighting configuration should set style variables directly
- dead `anyframe` branches should be deleted

### 4. Keep behavior-compatible escape hatches

- Preserve `scd` support and the `s` alias
- Keep `spaceship` prompt behavior intact
- Keep `fzf-tab` conditional on `fzf`
- Add a manual sync helper for plugin maintenance instead of doing install checks during startup

## Non-Goals

- Replacing `zplug`
- Rewriting prompt or key-binding behavior
- Changing the `profile/` or `login/` shell layers
- Introducing a new plugin manager

## Expected Result

Startup should become meaningfully faster by:

1. removing `zplug check` from the hot path
2. reducing the number of plugin declarations `zplug` has to resolve
3. stopping post-load interactive files from querying `zplug` repeatedly

The resulting structure should also make a future plugin-manager replacement easier, because the plugin layer becomes smaller and the interactive layer stops depending on `zplug` for routine presence checks.

## First-Pass Results

Measured with writable `zplug` cache access:

- `master` warm interactive startup: about `0.87s` to `0.89s`
- optimized branch warm interactive startup: about `0.51s` to `0.52s`

That is roughly a `40%` reduction for warm interactive shell startup in this environment.
