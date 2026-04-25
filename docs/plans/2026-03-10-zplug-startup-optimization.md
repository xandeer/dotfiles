# Zplug Startup Optimization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reduce zsh startup time by removing `zplug` startup maintenance work, trimming the plugin bundle, and replacing runtime plugin-manager checks with direct loaded-state guards.

**Architecture:** Keep `zplug` as the plugin loader, but move install/update checks out of the interactive startup path. Shrink `bundles.zsh` to the plugins the current config still relies on, and make interactive files depend on loaded widgets/functions instead of repeated `zplug check "<plugin>"` calls.

**Tech Stack:** zsh, zplug, GNU Stow, local shell regression scripts

---

### Task 1: Remove startup-time plugin maintenance

**Files:**
- Modify: `zsh/.config/zsh/plugins/zplug.zsh`
- Modify: `zsh/.config/zsh/interactive/aliases.zsh`
- Test: `tests/zsh/zplug-startup-maintenance-regression.zsh`

**Step 1: Write the failing test**

Create a regression script that sources the plugin loader and fails if interactive startup still runs `zplug check` unconditionally.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh tests/zsh/zplug-startup-maintenance-regression.zsh
```

Expected: failure showing the startup path still invokes install/check logic.

**Step 3: Write minimal implementation**

- Remove the unconditional `zplug check` block from `zsh/.config/zsh/plugins/zplug.zsh`
- Add a manual maintenance alias or helper for explicit plugin sync/install

**Step 4: Run verification**

Run:

```bash
zsh tests/zsh/zplug-startup-maintenance-regression.zsh
zsh -n zsh/.config/zsh/plugins/zplug.zsh zsh/.config/zsh/interactive/aliases.zsh
```

Expected: both commands exit `0`.

**Step 5: Commit**

```bash
git add zsh/.config/zsh/plugins/zplug.zsh zsh/.config/zsh/interactive/aliases.zsh tests/zsh/zplug-startup-maintenance-regression.zsh
git commit -m "perf: remove zplug startup maintenance checks"
```

### Task 2: Trim the plugin bundle to active dependencies

**Files:**
- Modify: `zsh/.config/zsh/plugins/bundles.zsh`

**Step 1: Capture the current bundle**

Run:

```bash
nl -ba zsh/.config/zsh/plugins/bundles.zsh | sed -n '1,220p'
```

Expected: output shows the current large bundle list including optional oh-my-zsh plugins and `plugins/nvm`.

**Step 2: Write minimal implementation**

- Keep only plugins that the current local config still depends on directly
- Remove duplicated or low-value plugin declarations, especially `plugins/nvm`

**Step 3: Run verification**

Run:

```bash
zsh -n zsh/.config/zsh/plugins/bundles.zsh
```

Expected: exit status `0`.

**Step 4: Commit**

```bash
git add zsh/.config/zsh/plugins/bundles.zsh
git commit -m "perf: trim zplug bundle set"
```

### Task 3: Replace runtime plugin checks in interactive files

**Files:**
- Modify: `zsh/.config/zsh/interactive/history.zsh`
- Modify: `zsh/.config/zsh/interactive/highlighting.zsh`
- Modify: `zsh/.config/zsh/interactive/key-bindings.zsh`
- Modify: `zsh/.config/zsh/interactive/prompt.zsh`
- Test: `tests/zsh/interactive-plugin-guards-regression.zsh`

**Step 1: Write the failing test**

Create a regression script that simulates loaded plugin widgets/functions and fails if the interactive files still require `zplug check "<plugin>"` for common startup behavior.

**Step 2: Run test to verify it fails**

Run:

```bash
zsh tests/zsh/interactive-plugin-guards-regression.zsh
```

Expected: failure indicating the interactive layer still depends on repeated `zplug check` calls.

**Step 3: Write minimal implementation**

- Replace `zplug check "zsh-users/zsh-history-substring-search"` with function/widget guards
- Replace autosuggestion checks with widget guards
- Drop dead `anyframe` code
- Set `spaceship` and syntax-highlighting configuration directly after plugin load

**Step 4: Run verification**

Run:

```bash
zsh tests/zsh/interactive-plugin-guards-regression.zsh
zsh -n zsh/.config/zsh/interactive/*.zsh
```

Expected: both commands exit `0`.

**Step 5: Commit**

```bash
git add zsh/.config/zsh/interactive tests/zsh/interactive-plugin-guards-regression.zsh
git commit -m "perf: remove repeated zplug checks from interactive config"
```

### Task 4: Measure startup and verify behavior

**Files:**
- Modify: `docs/plans/2026-03-10-zplug-startup-optimization-design.md`
- Modify: `docs/plans/2026-03-10-zplug-startup-optimization.md`

**Step 1: Re-measure startup**

Run:

```bash
TIMEFMT='%E real'; for i in 1 2 3; do time ZDOTDIR="$PWD/zsh" zsh -i -c exit >/tmp/zplug-opt.$i.out 2>/tmp/zplug-opt.$i.err; done
```

Expected: startup time improves relative to the original `1.29s` to `1.41s` baseline in this environment.

**Step 2: Verify key user-facing behaviors**

Run:

```bash
ZDOTDIR="$PWD/zsh" zsh -i -c 'alias s 2>/dev/null; whence -v scd' 2>&1
ZDOTDIR="$PWD/zsh" zsh -i -c 'typeset -p SPACESHIP_PROMPT_ORDER' 2>&1
```

Expected:
- `s=scd`
- `scd` still resolves
- prompt configuration is still present

**Step 3: Commit**

```bash
git add docs/plans/2026-03-10-zplug-startup-optimization-design.md docs/plans/2026-03-10-zplug-startup-optimization.md
git commit -m "docs: record zplug startup optimization results"
```
