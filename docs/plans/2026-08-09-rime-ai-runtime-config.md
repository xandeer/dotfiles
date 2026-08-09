# Rime AI Runtime Controls Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add reloadable global enablement and optional AI writing preferences while keeping the Squirrel response protocol immutable and the learned AI table private.

**Architecture:** Extend the existing Squirrel 1.1.2 Swift core with validated prompt composition and extend its controller bridge with an early enabled/configuration gate. Keep all user-facing values in `squirrel.custom.yaml`, the API key in Keychain, and learning in the existing Lua TSV pipeline with fail-closed `0600` atomic writes. Regenerate the existing two format patches instead of adding another patch layer.

**Tech Stack:** Rime YAML, librime-lua, Swift 6, InputMethodKit, Security.framework, URLSession, zsh, Ruby Fiddle, git format-patch, Xcode.

---

## Fixed boundaries

- Squirrel source baseline remains official tag `1.1.2`, commit `876adebaf2f612951dcdca8a591de65401222b9a`.
- `ai/enabled` defaults to `true`; only an explicit `false` disables the bridge.
- `ai/instructions` defaults to empty, is limited to 4096 Swift characters after trimming, allows line feed and tab, and rejects other Unicode control scalars.
- Invalid non-empty instructions suppress the cloud request rather than silently discarding user configuration.
- Optional preferences are delimited first. The immutable mandatory postamble comes last, explicitly wins conflicts, treats user JSON as untrusted data, ignores instructions inside it, prefers an existing candidate, and requires exactly `{"candidate":"..."}` with one line and at most 64 characters.
- Disabled state must invalidate debounce/task/generation and clear published Rime properties before any Keychain read or request construction.
- No settings UI, per-app policy, generation tuning, prompt version, retry, streaming, custom header, metrics, or AI content logging.
- Verification never installs Squirrel, writes a real Keychain item, calls a real endpoint, or deploys into the active user Rime directory.

## Task 1: Record the approved runtime-control design

**Files:**

- Create: `docs/plans/2026-08-09-rime-ai-runtime-config-design.md`
- Create: `docs/plans/2026-08-09-rime-ai-runtime-config.md`

**Step 1: Check the design against the current implementation**

Confirm that the design preserves the existing endpoint/model/Keychain flow, strict parser, secure-input handling, and independent learned table.

**Step 2: Run documentation checks**

```zsh
git diff --check
rg -n "ai/enabled|ai/instructions|0600|immutable" \
  docs/plans/2026-08-09-rime-ai-runtime-config*.md
```

Expected: both commands exit zero and each new contract is explicit.

**Step 3: Commit**

```zsh
git add docs/plans/2026-08-09-rime-ai-runtime-config-design.md \
  docs/plans/2026-08-09-rime-ai-runtime-config.md
git commit -m "docs: design Rime AI runtime controls"
```

## Task 2: Add failing runtime-control regressions

**Files:**

- Modify: `tests/config/squirrel-ai-core-regression.swift`
- Modify: `tests/config/squirrel-ai-bridge-regression.zsh`
- Modify: `tests/config/rime-config-regression.zsh`
- Modify: `tests/config/rime-ai-regression.lua`

**Step 1: Extend the Swift core regression**

Add request-builder coverage for:

- immutable final-postamble, untrusted-data, conflict-priority, and strict-response rules in every system message;
- empty instructions producing only the immutable contract;
- normalized multiline instructions in a delimited preferences section before the mandatory postamble;
- exactly 4096 characters accepted and 4097 rejected;
- NUL, carriage return, and other control scalars rejected while line feed and tab remain valid;
- API key absent from the JSON body with and without custom instructions.

**Step 2: Extend the bridge contract regression**

Require `scheduleAICandidate()` to:

- read `ai/enabled` before endpoint/model/input/Keychain/network work;
- treat nil as enabled and explicit false as invalidate-and-return;
- capture normalized `ai/instructions` before the timer;
- compare one captured enabled/endpoint/model/instructions configuration again at the timer and post-Keychain gates;
- pass the captured instructions into the reviewed core request builder;
- compare the complete captured configuration in `applyAICandidate()` before publishing any Rime property.

Keep the existing rejection of added logging and hard-coded secrets.

**Step 3: Extend Rime YAML and Lua regressions**

Require shipped values `ai/enabled: true` and `ai/instructions: ""`. In the temporary Lua data directory, verify:

- an existing broad-mode table in a temporary directory containing spaces and a single quote is corrected to `0600` at translator initialization;
- a newly persisted table is `0600` after atomic rename;
- permission setup failure disables learned reads/writes while live AI ordering still works;
- temp creation, path validation, open, write, close, and rename failures never replace the previous table or fall back to an unsafe create.

Do not touch `~/Library/Rime/ai_weights.tsv`.

**Step 4: Prove RED against current production code**

```zsh
/bin/zsh tests/config/rime-config-regression.zsh
/bin/zsh tests/config/squirrel-ai-bridge-regression.zsh \
  /private/tmp/codex-squirrel-1.1.2-019fe53f

core_test_dir="$(mktemp -d /tmp/squirrel-ai-core-red.XXXXXX)"
xcrun swiftc -parse-as-library \
  -framework Foundation -framework LocalAuthentication -framework Security \
  /private/tmp/codex-squirrel-1.1.2-019fe53f/sources/SquirrelAI.swift \
  tests/config/squirrel-ai-core-regression.swift \
  -o "$core_test_dir/squirrel-ai-core-regression"
"$core_test_dir/squirrel-ai-core-regression"
```

Expected: each affected suite fails only on a newly added runtime-control or permission contract.

**Step 5: Commit the tests**

```zsh
git add tests/config/squirrel-ai-core-regression.swift \
  tests/config/squirrel-ai-bridge-regression.zsh \
  tests/config/rime-config-regression.zsh \
  tests/config/rime-ai-regression.lua
git commit -m "test(rime): cover AI runtime controls"
```

## Task 3: Implement the Swift runtime controls

**Files in patched Squirrel checkout:**

- Modify: `sources/SquirrelAI.swift`
- Modify: `sources/SquirrelInputController.swift`

**Files in this repository:**

- Regenerate: `rime/squirrel-ai/patches/0001-feat-add-squirrel-ai-request-core.patch`
- Regenerate: `rime/squirrel-ai/patches/0002-feat-bridge-ai-candidates-into-rime.patch`

**Step 1: Implement the pure prompt core**

Add one immutable mandatory-protocol constant and a small normalization/composition helper. Change `SquirrelAI.request` to accept captured instructions and return nil for invalid input. Put optional preferences first and the mandatory conflict-winning protocol last. Do not place custom instructions in the user JSON snapshot and do not weaken the existing response parser.

**Step 2: Implement the controller gate**

At the beginning of `scheduleAICandidate()`, after the existing secure-input check but before endpoint/model/input access, read `config?.getBool("ai/enabled") ?? true`. Explicit false must call `invalidateAICandidate(clearProperties: true)` and return.

Capture a comparable runtime configuration containing enabled, endpoint, model, and core-normalized instructions before scheduling. Rebuild and compare the complete value at every asynchronous boundary before Keychain/request work. Pass only its validated instructions into `SquirrelAI.request`, carry the captured configuration to `applyAICandidate()`, and compare it again before setting any Rime property so a late response from an old model/instruction set cannot publish after reload.

**Step 3: Run focused GREEN checks**

Run the exact Swift core binary and bridge regression from Task 2. Expected: `Squirrel AI core regression OK` and `Squirrel AI bridge contract OK`.

**Step 4: Amend the two source commits and regenerate patches mechanically**

Keep the source history as two commits: core first, bridge second. Regenerate with `git format-patch -2`; do not hand-edit patch payloads. Confirm patch IDs/source diffs match and `git am` accepts them on a clean 1.1.2 checkout.

**Step 5: Commit packaged Swift changes**

```zsh
git add rime/squirrel-ai/patches/0001-*.patch \
  rime/squirrel-ai/patches/0002-*.patch
git commit -m "feat(rime): add AI runtime controls"
```

## Task 4: Enforce private learning storage and document usage

**Files:**

- Modify: `rime/rime.lua`
- Modify: `rime/darwin/squirrel.custom.yaml`
- Modify: `rime/squirrel-ai/README.md`

**Step 1: Implement fail-closed `0600` setup**

Squirrel's bundled Lua has no native chmod/POSIX module, so secure an existing table during translator initialization with absolute `/bin/chmod` and POSIX-safe single-quote escaping. Reject non-absolute paths and NUL/CR/LF before constructing a command.

For writes, use absolute `/usr/bin/mktemp -q` to create a unique same-directory `0600` temporary file. Validate the returned path, open only that existing empty file with `r+`, check write and close outcomes, then atomically rename. On any failure, remove only the exact temporary path, keep the previous destination, and never fall back to `io.open(..., "w")` on a predictable temp name.

**Step 2: Add YAML defaults and README examples**

Document `ai/enabled`, empty and multiline `ai/instructions`, the reload command, the immutable response contract, and the `0600` learned-table guarantee. State explicitly that changing enabled/instructions/endpoint/model needs reload but not recompilation; changing Swift protocol behavior still needs rebuilding the patched app.

**Step 3: Run Rime GREEN checks**

```zsh
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: `Rime AI regression OK`.

Run the repository's isolated `make -C rime install` plus `rime_deployer --build` flow against temporary directories. Confirm the compiled schema exists and no active user files changed.

**Step 4: Commit**

```zsh
git add rime/rime.lua rime/darwin/squirrel.custom.yaml rime/squirrel-ai/README.md
git commit -m "fix(rime): protect AI runtime data"
```

## Task 5: Replay, build, and review the complete package

**Files:**

- Verify all files changed since the design commit.

**Step 1: Fresh patch replay**

Create a new temporary checkout of official Squirrel 1.1.2, confirm exact base SHA, apply both packaged patches with `git am`, and require a clean tracked status.

**Step 2: Full automated verification**

Against the fresh replay run:

- Swift core regression;
- bridge contract regression;
- Rime/Lua regression;
- `bash ./action-install.sh` followed by `make debug`;
- patch/source `git diff --check`;
- scans for added AI logs, embedded keys, request bodies, generated products, and accidental active-user paths.

Expected: both contract markers, `Rime AI regression OK`, and `** BUILD SUCCEEDED **`.

**Step 3: Inspect the built bundle without installing**

Verify the executable is signed, expected entitlements remain, and no real API request, Keychain mutation, or active installation occurred.

**Step 4: Independent review**

Review behavior against this design, then review code quality and the complete branch diff. Fix any P1/P2 issue with a new failing regression before implementation.

**Step 5: Final status**

Require a clean feature worktree, preserve the unrelated main-worktree modification, and present merge/PR/keep/discard options. Installation remains a separate explicit user checkpoint.
