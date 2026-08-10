# Squirrel AI Disable Thinking Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make every Squirrel AI chat-completion request explicitly disable provider thinking while remaining non-streaming.

**Architecture:** Keep the existing OpenAI-compatible request flow and add one fixed protocol object, `thinking: {"type":"disabled"}`, beside the existing `stream: false`. Validate the actual encoded JSON in the pure Swift regression, then regenerate the two-patch Squirrel 1.1.2 patch series and reinstall the verified app.

**Tech Stack:** Swift/Foundation `Encodable`, Squirrel 1.1.2, URLSession, zsh regression scripts, git format-patch, Xcode debug build.

---

### Task 1: Add the failing request-contract regression

**Files:**
- Modify: `tests/config/squirrel-ai-core-regression.swift:283-305`

**Step 1: Write the failing test**

After decoding the request body in `testRequestBuilder`, require the exact
top-level protocol fields and the nested thinking value:

```swift
expectEqual(
  Set(json.keys),
  Set(["model", "messages", "thinking", "stream"]),
  "request body contains only the reviewed protocol fields"
)
expectEqual(json["stream"] as? Bool, false, "streaming is disabled")
let thinking = json["thinking"] as? [String: Any]
expectEqual(Set(thinking?.keys ?? []), Set(["type"]), "thinking has only its type")
expectEqual(thinking?["type"] as? String, "disabled", "thinking is disabled")
```

**Step 2: Run the test to verify RED**

Run:

```zsh
mkdir -p /private/tmp/squirrel-ai-disable-thinking-modules
CLANG_MODULE_CACHE_PATH=/private/tmp/squirrel-ai-disable-thinking-modules/clang \
SWIFT_MODULECACHE_PATH=/private/tmp/squirrel-ai-disable-thinking-modules/swift \
xcrun swiftc -parse-as-library \
  /private/tmp/squirrel-ai-final-verify.TedHTrPp/squirrel/sources/SquirrelAI.swift \
  tests/config/squirrel-ai-core-regression.swift \
  -framework Security \
  -framework LocalAuthentication \
  -o /private/tmp/squirrel-ai-disable-thinking-red
/private/tmp/squirrel-ai-disable-thinking-red
```

Expected: the binary traps because `thinking` is missing while the existing
`stream == false` assertion still passes.

**Step 3: Commit the RED test**

```bash
git add tests/config/squirrel-ai-core-regression.swift
git commit -m "test(rime): require non-thinking AI requests"
```

### Task 2: Implement the fixed request field in the upstream source

**Files:**
- Modify in a fresh pinned Squirrel checkout: `sources/SquirrelAI.swift:118-136,263-268`

**Step 1: Prepare the pinned checkout**

Create a temporary checkout at exact Squirrel 1.1.2 base `876adeb`, apply the
existing core patch, and verify `HEAD^` is the base commit.

**Step 2: Add the minimal encoder types**

Pass a fixed value when constructing `SquirrelAIRequest`:

```swift
thinking: SquirrelAIThinking(type: "disabled"),
stream: false
```

Extend the request payload and add the nested encoder:

```swift
private struct SquirrelAIRequest: Encodable {
  let model: String
  let messages: [SquirrelAIMessage]
  let thinking: SquirrelAIThinking
  let stream: Bool
}

private struct SquirrelAIThinking: Encodable {
  let type: String
}
```

Do not change timeout, parsing, logging, endpoint handling, or headers.

**Step 3: Run the pure core regression to verify GREEN**

Compile the modified `SquirrelAI.swift` with the committed regression using the
same `swiftc` command from Task 1, then run the binary.

Expected: `Squirrel AI core regression OK`.

**Step 4: Amend the upstream core commit**

```bash
git add sources/SquirrelAI.swift
git commit --amend --no-edit
```

**Step 5: Apply the existing bridge patch**

Apply `0002-feat-bridge-ai-candidates-into-rime.patch` on the amended core commit.
Expected: clean application with no controller source changes.

### Task 3: Regenerate and validate the packaged patch series

**Files:**
- Modify: `rime/squirrel-ai/patches/0001-feat-add-squirrel-ai-request-core.patch`
- Modify only if mechanical ancestry changes require it: `rime/squirrel-ai/patches/0002-feat-bridge-ai-candidates-into-rime.patch`

**Step 1: Mechanically regenerate both patches**

Run `git format-patch -2` from the pinned upstream checkout, writing the two
standard format-patch files into `rime/squirrel-ai/patches/`.

**Step 2: Verify fresh replay**

Create another exact-base checkout and run:

```zsh
git am /absolute/path/to/rime/squirrel-ai/patches/*.patch
```

Expected: both patches apply without conflict and the source tree matches the
upstream implementation checkout.

**Step 3: Run all relevant regressions**

Run the pure Swift core regression against the replay checkout, then:

```zsh
/bin/zsh tests/config/squirrel-ai-bridge-regression.zsh /path/to/replay
/bin/zsh tests/config/rime-config-regression.zsh
git diff --check
```

Expected: core prints `Squirrel AI core regression OK`, bridge prints
`Squirrel AI bridge contract OK`, Rime prints `Rime AI regression OK`, and
diff check is clean except the known inner format-patch context-marker warning
if checked as an outer working-tree diff.

**Step 4: Commit the implementation artifacts**

```bash
git add rime/squirrel-ai/patches tests/config/squirrel-ai-core-regression.swift
git commit -m "fix(rime): disable Squirrel AI thinking"
```

### Task 4: Build and review the patched app

**Files:**
- Verify: replayed Squirrel source tree
- Build artifact: `build/Build/Products/Debug/Squirrel.app`

**Step 1: Build**

Run `bash ./action-install.sh` only if the fresh checkout lacks its pinned build
dependencies, then run `make debug`.

Expected: exit 0 and `** BUILD SUCCEEDED **`.

**Step 2: Verify artifact**

Run deep strict code-sign verification and inspect entitlements. Require
`com.apple.security.network.client=true` and no new hard-coded credential or AI
request/response logging.

**Step 3: Request code review**

Review the test, source diff, regenerated patches, and build evidence. Resolve
all P1/P2 findings before installation.

### Task 5: Install and validate on the live Mac

**Files:**
- Replace: `/Library/Input Methods/Squirrel.app`
- Preserve: the current app backup, Rime config, and Keychain item

**Step 1: Make a fresh recoverable backup**

Copy the currently installed bundle to a timestamped directory under
`~/Library/Application Support/Squirrel AI Backups/`. Resolve and validate the
exact source and destination before replacement.

**Step 2: Replace only the Squirrel bundle**

Stop Squirrel, replace the exact bundle with the verified build, restore
`root:wheel` ownership, run the bundled registration/selection commands, and
launch Squirrel. Do not invoke the upstream broad `make install-debug` target.

**Step 3: Recreate the Keychain ACL**

Because the installed signature/build changed, rerun the interactive
`security add-generic-password -U ... -T "/Library/Input Methods/Squirrel.app" -w`
command. The user must enter and submit the API key; never read or log it.

**Step 4: Verify installation**

Require all of the following with fresh commands:

- installed bundle passes `codesign --verify --deep --strict`;
- Squirrel process runs from the installed bundle and loads the rebuilt dylib;
- active endpoint/model and compiled AI Lua components remain present;
- Keychain item exists without reading its data;
- the installed dylib hash matches the verified build artifact;
- core, bridge, and Rime regressions pass.

**Step 5: Verify the original symptom**

In a blank non-secure editor using the `double_pinyin_flypy` schema, type a
harmless composition and stop typing. Confirm the Rime lifecycle log gains an
`_ai_refresh` update without inspecting input, candidate, request, or response
content. If it still does not publish, stop and resume root-cause investigation
before changing timeout or parsing.
