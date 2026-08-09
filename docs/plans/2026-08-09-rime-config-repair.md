# Rime Config Repair Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Repair the agreed Rime configuration failures without adding dependencies or changing unrelated input behavior.

**Architecture:** Keep the existing Xiaohe schema and dictionaries. Remove components whose required configuration is absent, restore routing for the already-present Unicode translator, and correct malformed or incompatible dictionary imports.

**Tech Stack:** Rime YAML, Rime dictionary format, zsh, bundled Squirrel `rime_deployer`

---

### Task 1: Add a focused failing regression

**Files:**
- Create: `tests/config/rime-config-regression.zsh`

**Step 1: Write assertions**

The script must reject active `reduce_english_filter`, active `xklbdz` translator/pattern, and active `cn_en`; require the Unicode recognizer; and require tab-separated `一骑当千` readings.

**Step 2: Verify RED**

Run:

```bash
zsh tests/config/rime-config-regression.zsh
```

Expected: FAIL because `reduce_english_filter` is still active.

### Task 2: Repair the active schema

**Files:**
- Modify: `rime/double_pinyin_flypy.schema.yaml`
- Test: `tests/config/rime-config-regression.zsh`

**Steps:**

1. Remove `lua_filter@reduce_english_filter`; rerun and expect the next assertion to fail.
2. Remove `reverse_lookup_translator@xklbdz` and its recognizer pattern; rerun and expect the Unicode assertion to fail.
3. Add `unicode: "^U[0-9A-Fa-f]+$"`; rerun and expect the dictionary assertion to fail.

### Task 3: Repair dictionary inputs

**Files:**
- Modify: `rime/cn_dicts/others.dict.yaml`
- Modify: `rime/melt_eng.dict.yaml`
- Test: `tests/config/rime-config-regression.zsh`

**Steps:**

1. Replace the two spaces after `一骑当千` with a tab; rerun and expect the `cn_en` assertion to fail.
2. Remove the `en_dicts/cn_en` import; rerun and expect PASS.

### Task 4: Verify the complete repair

**Files:**
- Read: `rime/double_pinyin_flypy.schema.yaml`
- Read: `rime/cn_dicts/others.dict.yaml`
- Read: `rime/melt_eng.dict.yaml`

**Steps:**

1. Parse the configuration YAML with Ruby/Psych.
2. Install into a temporary HOME and run bundled `rime_deployer --build`.
3. Require exit status 0 and no `Encode failure` or component errors.
4. Run `git diff --check` and review the scoped diff.
5. Do not commit unless explicitly requested.
