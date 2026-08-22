# Rime AI Punctuation Guard Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Prevent live and learned AI candidates from replacing native punctuation candidates, so punctuation keeps librime's direct-commit behavior.

**Architecture:** Reuse one local `ai_segment_is_punct` predicate at the existing live-filter, learned-translator, and selection-learning boundaries. Keep the schema and punctuation mapping unchanged; remove existing runtime pollution only after the guarded code is deployed.

**Tech Stack:** Rime Lua, librime-lua segment tags, zsh regression harness, native librime 1.16.0.

---

### Task 1: Guard every AI punctuation boundary

**Files:**
- Modify: `tests/config/rime-ai-regression.lua`
- Modify: `rime/rime.lua`

**Step 1: Write the failing regression**

Add a test segment whose `has_tag("punct")` returns true, then assert:

```lua
local punct_segment = {start = 0, _end = 1, status = "selected"}
function punct_segment:has_tag(tag)
    return tag == "punct"
end
```

- `ai_candidate_filter` passes through the original `punct` candidate even when valid live AI properties exist;
- `ai_learned_translator.func` yields no stored row for the punctuation segment;
- select followed by commit leaves the learned TSV byte-for-byte unchanged.

**Step 2: Run the test and verify RED**

Run:

```zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: FAIL because the current filter injects an AI candidate into the punctuation segment.

**Step 3: Add the minimum production guard**

Add one local predicate:

```lua
local function ai_segment_is_punct(segment)
    return segment and segment.has_tag and segment:has_tag("punct")
end
```

Use it to:

- clear `env.ai_pending` and return from the selection notifier;
- return from `ai_learned_translator.func` before storage access;
- take the existing pass-through branch in `ai_candidate_filter`.

Do not alter non-punctuation behavior, filter order, mappings, storage format, or Squirrel code.

**Step 4: Run GREEN and the full Rime regression**

Run:

```zsh
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
git diff --check
```

Expected: exit 0 with `Rime auto-space regression OK` and `Rime AI regression OK`.

**Step 5: Commit**

```zsh
git add rime/rime.lua tests/config/rime-ai-regression.lua
git commit -m "fix(rime): keep AI out of punctuation"
```

### Task 2: Validate native behavior and repair runtime state

**Files:**
- Runtime deploy target: `~/Library/Rime`
- Runtime data: `~/Library/Rime/ai_weights.tsv`

**Step 1: Build in an isolated HOME**

Install the worktree configuration into a temporary HOME and build it with Squirrel's bundled `rime_deployer`. Seed punctuation rows only in that temporary HOME.

**Step 2: Run native librime acceptance**

Using the existing temporary C++ harness against librime 1.16.0, verify:

- clean and polluted temporary tables both make `,` commit `，` immediately;
- composition is empty after the key;
- ordinary `dzhc` AI/word candidates still work.

**Step 3: Deploy the guarded Lua code**

Run the repository's Darwin install path from the worktree, deploy Rime, and start a fresh Squirrel engine. Confirm installed `rime.lua` matches the reviewed worktree file.

**Step 4: Back up and clean exact polluted keys**

Create a timestamped mode-0600 backup, then atomically remove only rows where:

```text
schema_id == double_pinyin_flypy AND input is exactly "," or "/"
```

Do not delete by candidate text and do not match `/A` or other symbol inputs.

**Step 5: Verify live state**

Confirm the deployed schema loads, the cleaned table retains every unrelated row, and physical comma directly commits in a real Squirrel client. If UI automation cannot prove the final insertion, report that manual acceptance remains instead of claiming it.

### Task 3: Guard librime digit separators

**Files:**
- Modify: `tests/config/rime-ai-regression.lua`
- Modify: `rime/rime.lua`
- Runtime data: `~/Library/Rime/ai_weights.tsv`

**Step 1: Write and run the failing regression**

Extend the punctuation test table to run once with tag `punct` and once with tag `punct_number`. The latter must preserve the native `.` candidate, yield no learned candidate, and leave storage unchanged.

Run:

```zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: FAIL on the `punct_number` case because `ai_segment_is_punct` only recognizes `punct`.

**Step 2: Extend the shared guard**

Change the existing predicate to return true for either `punct` or `punct_number`. Do not change Rime punctuation mappings or `punctuator/digit_separators`.

**Step 3: Run GREEN and commit**

Run:

```zsh
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
git diff --check
```

Expected: exit 0 with both Rime regression success messages.

**Step 4: Deploy and clean the exact polluted row**

Deploy the guarded Lua code, create a mode-0600 timestamped backup, atomically remove only rows whose schema/input/text are exactly `double_pinyin_flypy`, `.`, and `。`, then reload Squirrel. Verify unrelated rows are byte-for-byte retained.
