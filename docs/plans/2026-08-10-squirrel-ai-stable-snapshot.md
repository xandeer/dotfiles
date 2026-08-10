# Squirrel AI Stable Snapshot Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Prevent unchanged Squirrel AI requests from being canceled by an
unstable InputMethodKit client identifier.

**Architecture:** Remove the per-call `uniqueClientIdentifierString()` value
from `SquirrelAISnapshot`. Preserve the existing Rime session, monotonic
generation, application identifier, full composition snapshot, and controller
lifecycle invalidation as the stale-request boundary.

**Tech Stack:** Swift/Foundation, InputMethodKit, Squirrel 1.1.2, zsh/Ruby
contract regression, git format-patch, Xcode Release build.

---

### Task 1: Add failing snapshot-stability regressions

**Files:**
- Modify: `tests/config/squirrel-ai-core-regression.swift`
- Modify: `tests/config/squirrel-ai-bridge-regression.zsh`

1. Verify the current core and bridge regressions pass against the current
   patched source.
2. Remove `clientID` from the core fixture and add equality assertions for all
   retained ownership fields.
3. Replace the bridge requirement for `uniqueClientIdentifierString()` with a
   rejection inside `currentAISnapshot()`; retain positive assertions for
   session, generation, and `currentApp`.
4. Run both tests against the current source and require failures caused only by
   the obsolete `clientID` contract.

### Task 2: Implement the minimal upstream fix

**Files in a pinned temporary Squirrel checkout:**
- Modify: `sources/SquirrelAI.swift`
- Modify: `sources/SquirrelInputController.swift`

1. Apply the current two-patch series to exact base `876adeb`.
2. Remove `SquirrelAISnapshot.clientID` and amend the core commit.
3. Reapply the bridge commit, remove the `clientID:` initializer argument, and
   amend the bridge commit.
4. Run the core and bridge tests and require GREEN before packaging.

### Task 3: Regenerate and replay the patch series

**Files:**
- Modify: `rime/squirrel-ai/patches/0001-feat-add-squirrel-ai-request-core.patch`
- Modify: `rime/squirrel-ai/patches/0002-feat-bridge-ai-candidates-into-rime.patch`

1. Generate both files mechanically with `git format-patch`.
2. Apply them with `git am` in another clean exact-base checkout.
3. Require source-tree equality between implementation and replay checkouts.
4. Run the pure Swift core test, bridge contract, full Rime regression, secret
   scan, and `git diff --check`.

### Task 4: Build and independently review Release

1. Build the replayed source as Release using the pinned dependencies.
2. Verify the bundle and all nested code with deep/strict code-sign checks.
3. Require the existing network entitlement, no `get-task-allow`, no debug or
   preview payload, no added logging, and no embedded key/config values.
4. Resolve every P1/P2 review finding before installation.

### Task 5: Install and validate the live input method

1. Make a new recoverable backup of the exact installed Squirrel bundle.
2. Replace only `/Library/Input Methods/Squirrel.app`, restore expected
   ownership, register/enable/select it, and restart the input method.
3. Because the signed code changed, recreate the Keychain item interactively
   for the new trusted application without reading or logging the key.
4. Verify installed hashes, signature, entitlements, process path, active Rime
   configuration, and compiled Lua components.
5. In a non-secure editor, type a harmless composition and stop. Confirm the
   request crosses the debounce boundary and an `_ai_refresh` publication occurs
   without inspecting input, candidates, request, or response values.
