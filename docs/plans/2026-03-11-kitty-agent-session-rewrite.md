# Kitty Agent Session Rewrite Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Rewrite saved kitty session entries for agent windows so Codex resumes with `codex resume --last` and Claude restarts with `claude -c`, while leaving non-agent windows unchanged.

**Architecture:** Keep `save_as_session --use-foreground-process` in kitty so the current OS window layout and cwd are captured. After saving, run `kitty-session-postprocess` to scan each `cd` + `launch 'kitty-unserialize-data=...'` pair, detect Codex or Claude commands from the serialized startup payload, and replace only those `launch` lines with explicit `/bin/zsh -lc 'cd ... && exec ...'` commands.

**Tech Stack:** kitty sessions, zsh, perl-compatible quoting rules, shell regression tests

---

### Task 1: Update regression coverage for agent launch rewrites

**Files:**
- Modify: `tests/bin/kitty-session-postprocess-regression.zsh`
- Modify: `tests/config/kitty-config-regression.zsh`

**Step 1: Write the failing test**

Extend the postprocess regression to include:
- a Codex launch entry that should become `launch /bin/zsh -lc 'cd ... && exec codex resume --last'`
- a Claude launch entry that should become `launch /bin/zsh -lc 'cd ... && exec claude -c'`
- a plain shell launch entry that should stay unchanged

Update the config regression to stop expecting the wrapper script and continue expecting the postprocess save hook.

**Step 2: Run test to verify it fails**

Run: `zsh tests/bin/kitty-session-postprocess-regression.zsh`
Expected: FAIL because the current postprocess script only rewrites binary paths.

**Step 3: Write minimal implementation**

Teach `kitty-session-postprocess` to parse the saved session file line-by-line, remember the most recent `cd`, inspect `cmd_at_shell_startup`, and rewrite only Codex and Claude launch lines.

**Step 4: Run test to verify it passes**

Run: `zsh tests/bin/kitty-session-postprocess-regression.zsh`
Expected: PASS

### Task 2: Remove obsolete wrapper dependencies

**Files:**
- Modify: `config/.config/kitty/kitty.conf`
- Delete: `config/.config/kitty/codex-session-wrapper`
- Modify: `tests/config/kitty-config-regression.zsh`
- Delete: `tests/bin/codex-session-wrapper-regression.zsh`

**Step 1: Write the failing test**

Update config assertions so they only require the postprocess script path and reject stale wrapper references.

**Step 2: Run test to verify it fails**

Run: `zsh tests/config/kitty-config-regression.zsh`
Expected: FAIL until wrapper references are removed from config expectations.

**Step 3: Write minimal implementation**

Remove the unused wrapper file and any config/test references to it.

**Step 4: Run test to verify it passes**

Run: `zsh tests/config/kitty-config-regression.zsh`
Expected: PASS

### Task 3: Verify complete behavior

**Files:**
- Verify: `config/.config/kitty/kitty-session-postprocess`
- Verify: `config/.config/kitty/kitty.conf`
- Verify: `config/.config/kitty/sessions/startup.kitty-session`

**Step 1: Run focused verification**

Run:
- `zsh tests/config/kitty-config-regression.zsh`
- `zsh tests/bin/kitty-session-postprocess-regression.zsh`
- `git diff --check`

Expected: all pass.

**Step 2: Smoke-check rewritten session output**

Run: `~/.config/kitty/kitty-session-postprocess config/.config/kitty/sessions/startup.kitty-session`
Expected: Codex and Claude windows become explicit `launch /bin/zsh -lc 'cd ... && exec ...'` entries, while non-agent windows remain in their saved kitty form.
