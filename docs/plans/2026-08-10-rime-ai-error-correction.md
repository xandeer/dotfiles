# Rime AI Error Correction Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Configure Squirrel AI with a Simplified Chinese prompt that conservatively corrects contextual Chinese input errors, including nasal-final confusion and mistyped key order.

**Architecture:** Keep the immutable Swift request/response protocol unchanged and place all behavior tuning in `rime/darwin/squirrel.custom.yaml` under `ai/instructions`. Strengthen the existing Rime configuration regression so the approved prompt cannot silently revert to empty or lose its key correction boundaries.

**Tech Stack:** Rime YAML, Ruby/Psych assertions embedded in zsh, Squirrel 1.1.2 Swift regression harnesses.

---

During execution, use @superpowers:test-driven-development for the configuration contract and @superpowers:verification-before-completion before reporting success.

### Task 1: Add the failing prompt contract

**Files:**
- Modify: `tests/config/rime-config-regression.zsh:86-109`
- Test: `tests/config/rime-config-regression.zsh`

**Step 1: Replace the empty-default assertion with the approved contract**

Immediately after the `ai/enabled` assertion, add:

```ruby
instructions = patch.fetch("ai/instructions")
unless instructions.is_a?(String) && !instructions.empty? &&
    instructions == instructions.strip && instructions.length <= 4_096
  abort "expected non-empty normalized ai/instructions within the runtime limit"
end

[
  "简体中文输入法",
  "前后鼻音混淆",
  "按键顺序颠倒",
  "相邻按键误触",
  "漏键和多键",
  "优先选择已有候选",
  "不翻译、不扩写、不润色",
  "无法确定时",
].each do |requirement|
  abort "expected ai/instructions correction rule: #{requirement}" unless
    instructions.include?(requirement)
end
```

Remove the old `patch["ai/instructions"] == ""` assertion.

**Step 2: Run the focused test to verify RED**

Run:

```zsh
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: syntax check passes; the regression exits non-zero with `expected non-empty normalized ai/instructions within the runtime limit` because the shipped value is still empty.

**Step 3: Commit the RED contract**

```zsh
git add tests/config/rime-config-regression.zsh
git commit -m "test(rime): require AI correction prompt"
```

### Task 2: Add the Simplified Chinese custom prompt

**Files:**
- Modify: `rime/darwin/squirrel.custom.yaml:10`
- Modify: `rime/squirrel-ai/README.md:91-93`
- Test: `tests/config/rime-config-regression.zsh`

**Step 1: Replace the empty customization with the approved prompt**

Use this exact YAML block:

```yaml
  ai/instructions: |-
    你是简体中文输入法的纠错候选选择器。结合当前输入方案、原始输入编码、现有候选、最近上屏内容和光标前后文，推断用户原意并选择最合适的候选。
    重点识别错别字、同音误选、前后鼻音混淆，以及按键顺序颠倒、相邻按键误触、漏键和多键等输入错误；也可纠正上下文明确支持的漏字、多字、语法和标点错误。
    优先选择已有候选。仅当已有候选都不合适且纠正依据充分时，才生成一个新候选。
    只纠正错误，不翻译、不扩写、不润色、不改变语气，也不添加原文没有的信息。
    保留合理的人名、专有名词、技术术语、代码、网址、数字、标点和英文大小写。无法确定时，选择最贴近原输入和上下文的保守结果。
```

**Step 2: Localize the README example**

Keep the example generic and concise:

```yaml
  ai/instructions: |-
    优先纠正输入错误，并选择最符合前后文的简体中文候选。
    保留已经正确的人名、专有名词和技术术语。
```

Do not duplicate the complete production prompt in the README.

**Step 3: Run the focused test to verify GREEN**

Run:

```zsh
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: both commands exit zero with no error output.

**Step 4: Commit the implementation**

```zsh
git add rime/darwin/squirrel.custom.yaml rime/squirrel-ai/README.md
git commit -m "feat(rime): tune AI error correction"
```

### Task 3: Verify protocol composition and repository quality

**Files:**
- Verify: `rime/squirrel-ai/patches/0001-feat-add-squirrel-ai-request-core.patch`
- Verify: `rime/squirrel-ai/patches/0002-feat-bridge-ai-candidates-into-rime.patch`
- Test: `tests/config/squirrel-ai-core-regression.swift`
- Test: `tests/config/squirrel-ai-bridge-regression.zsh`

**Step 1: Replay the patch series onto the exact Squirrel base**

Use an existing local Squirrel Git checkout only after confirming it contains exact base `876adeb`; otherwise clone the pinned 1.1.2 source described in `rime/squirrel-ai/README.md`.

```zsh
verify_root="$(mktemp -d /private/tmp/rime-ai-error-correction.XXXXXX)"
git clone --no-hardlinks /private/tmp/codex-squirrel-1.1.2-019fe53f "$verify_root/squirrel"
git -C "$verify_root/squirrel" cat-file -e '876adeb^{commit}'
git -C "$verify_root/squirrel" checkout --detach 876adeb
git -C "$verify_root/squirrel" am "$PWD"/rime/squirrel-ai/patches/*.patch
```

Expected: both patches apply cleanly to the pinned base.

**Step 2: Run the pure Swift and bridge regressions**

```zsh
mkdir -p "$verify_root/modules/clang" "$verify_root/modules/swift"
CLANG_MODULE_CACHE_PATH="$verify_root/modules/clang" \
SWIFT_MODULECACHE_PATH="$verify_root/modules/swift" \
xcrun swiftc -parse-as-library \
  "$verify_root/squirrel/sources/SquirrelAI.swift" \
  tests/config/squirrel-ai-core-regression.swift \
  -framework Security \
  -framework LocalAuthentication \
  -o "$verify_root/squirrel-ai-core-regression"
"$verify_root/squirrel-ai-core-regression"
/bin/zsh tests/config/squirrel-ai-bridge-regression.zsh "$verify_root/squirrel"
```

Expected: the core binary exits zero and the bridge regression exits zero. This proves non-empty optional instructions still compose before the immutable protocol without changing the response contract.

**Step 3: Run final static checks**

```zsh
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
git diff --check HEAD~2..HEAD
git status --short
```

Expected: every test/check exits zero. Status shows only the user's pre-existing `config/.codex/config.toml` modification; no runtime Rime files under `$HOME` are changed.

**Step 4: Review the final diff**

Confirm that:

- `ai/instructions` is the only production configuration behavior change;
- the prompt is entirely Simplified Chinese and includes the approved error classes;
- the mandatory Swift postamble and response parser are unchanged;
- no endpoint, model, credential, Keychain item, installed Squirrel bundle, or active Rime configuration was modified.
