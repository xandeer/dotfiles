# Agent Kitty Notifications Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add shared kitty result notifications for Codex CLI and Claude Code, shown only when the current kitty window is unfocused and without reusing a fixed kitty notification ID.

**Architecture:** Create one shell script in the `bin` stow package that converts Codex and Claude completion payloads into kitty `OSC 99` notifications with `o=unfocused`. Move the existing local Codex config into the repository's `config` stow package, then wire Codex `notify` and Claude `Stop` hooks to the shared script. Avoid sending a fixed `i=` identifier so repeated completions from the same kitty window do not overwrite each other.

**Tech Stack:** GNU Stow, kitty `OSC 99`, Codex CLI `notify`, Claude Code hooks, zsh shell tests, JSON/TOML config files

---

### Task 1: Add regression tests for the shared notification behavior

**Files:**
- Create: `tests/bin/agent-notify-kitty-regression.zsh`

**Step 1: Write the failing test**

- Simulate a Codex notification payload and a Claude `Stop` payload.
- Assert the script output contains `OSC 99`, `o=unfocused`, and agent-specific titles.
- Assert the output does not contain a fixed `i=` notification identifier.
- Expect the test to fail against the current script because it reuses a fixed notification ID.

**Step 2: Run the test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/bin/agent-notify-kitty-regression.zsh
```

Expected: FAIL because the script output still contains `i=agent-notify-...`.

### Task 2: Implement the shared kitty notification script

**Files:**
- Create: `bin/bin/agent-notify-kitty`

**Step 1: Write the minimal implementation**

- Accept the agent kind as the first argument (`codex` or `claude`).
- Read Codex payload JSON from the next argument and Claude payload JSON from stdin.
- If `KITTY_WINDOW_ID` is absent, exit successfully without output.
- Extract a short title and body from the payload, clip the body length, base64-encode title/body, and print kitty `OSC 99` notifications with `o=unfocused`.
- Do not send a fixed `i=` identifier in the OSC payload.

**Step 2: Re-run the regression test**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/bin/agent-notify-kitty-regression.zsh
```

Expected: PASS.

### Task 3: Add regression tests for config wiring

**Files:**
- Create: `tests/config/agent-notify-config-regression.zsh`

**Step 1: Write the failing test**

- Assert `config/.codex/config.toml` exists and contains the shared `notify` command.
- Assert [config/.claude/settings.json](/Users/kevin/projects/personal/dotfiles/config/.claude/settings.json) contains a `Stop` hook pointing at the shared script.

**Step 2: Run the test to verify it fails**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/agent-notify-config-regression.zsh
```

Expected: FAIL because the Codex config file is not in the repository yet and Claude has no hook entry.

### Task 4: Move Codex config into stow and wire both agents

**Files:**
- Create: `config/.codex/config.toml`
- Modify: `config/.claude/settings.json`

**Step 1: Add the repository-managed Codex config**

- Copy the current local `~/.codex/config.toml` content into `config/.codex/config.toml`.
- Preserve existing model/provider/project settings.
- Add `notify = ["/Users/kevin/bin/agent-notify-kitty", "codex"]`.

**Step 2: Add the Claude `Stop` hook**

- Keep existing permissions, plugins, and language settings intact.
- Add a `hooks.Stop` entry that runs `/Users/kevin/bin/agent-notify-kitty claude`.

**Step 3: Re-run the config regression test**

Run:

```bash
zsh /Users/kevin/projects/personal/dotfiles/tests/config/agent-notify-config-regression.zsh
```

Expected: PASS.

### Task 5: Validate syntax and installation shape

**Files:**
- Read: `bin/bin/agent-notify-kitty`
- Read: `config/.codex/config.toml`
- Read: `config/.claude/settings.json`
- Read: `Makefile`

**Step 1: Validate script and config syntax**

Run:

```bash
sh -n /Users/kevin/projects/personal/dotfiles/bin/bin/agent-notify-kitty
python3 -c 'import json, pathlib, tomllib; json.load(open("/Users/kevin/projects/personal/dotfiles/config/.claude/settings.json")); tomllib.loads(pathlib.Path("/Users/kevin/projects/personal/dotfiles/config/.codex/config.toml").read_text())'
```

Expected: both commands exit successfully.

**Step 2: Dry-run stow for the managed files**

Run:

```bash
stow -nv -d /Users/kevin/projects/personal/dotfiles -t "$HOME" bin config
```

Expected: output shows the new `bin` script and `config/.codex/config.toml` would be linked into place, with the existing plain `~/.codex/config.toml` reported as the conflict to resolve during install.

**Step 3: Review the final diff**

Run:

```bash
git -C /Users/kevin/projects/personal/dotfiles diff -- bin config tests docs/plans
```

Expected: diff shows only the shared notification script, Codex/Claude config wiring, regression tests, and the matching plan/design docs.
