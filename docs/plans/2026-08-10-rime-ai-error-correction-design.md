# Rime AI Error Correction Design

## Goal

Tune the existing Squirrel AI candidate bridge for conservative, context-aware error correction. The customization itself is written in Simplified Chinese and lives only in Rime's runtime-configurable `ai/instructions` section.

## Confirmed Scope

- Correct likely Chinese input mistakes without turning the input method into a rewriting tool.
- Use the current schema, raw input, existing candidates, recent commits, and surrounding text as evidence of user intent.
- Explicitly consider front/back nasal-final confusion such as `an`/`ang`, `en`/`eng`, and `in`/`ing`.
- Explicitly consider transposed key order, adjacent-key slips, missing keys, and duplicated or extra keys.
- Also handle clear character, homophone, missing-character, extra-character, grammar, and punctuation errors when context provides strong evidence.
- Preserve names, proper nouns, technical terms, code, URLs, numbers, punctuation, and English capitalization when already plausible.
- Keep the built-in mandatory request/response protocol unchanged.

## Prompt Strategy

Use a conservative correction policy:

1. Infer the intended text from all available language context rather than from the raw key sequence alone.
2. Prefer an existing candidate whenever it can express the inferred intent.
3. Create a new candidate only when no existing candidate is suitable and the correction is strongly supported.
4. Correct only the likely error. Do not translate, expand, beautify, change tone, or add information.
5. When evidence is ambiguous, preserve the user's likely original wording and avoid speculative correction.

This is preferred over spelling-only correction, which misses contextual and key-order errors, and aggressive rewriting, which can silently change the user's intent.

## Configuration

Replace the empty value in `rime/darwin/squirrel.custom.yaml` with a multiline Simplified Chinese block scalar:

```yaml
  ai/instructions: |-
    你是中文输入法的纠错候选选择器。结合当前输入方案、原始输入编码、现有候选、最近上屏内容和光标前后文，推断用户原意并选择最合适的候选。
    重点识别错别字、同音误选、前后鼻音混淆，以及按键顺序颠倒、相邻按键误触、漏键和多键等输入错误；也可纠正上下文明确支持的漏字、多字、语法和标点错误。
    优先选择已有候选。仅当已有候选都不合适且纠正依据充分时，才生成一个新候选。
    只纠正错误，不翻译、不扩写、不润色、不改变语气，也不添加原文没有的信息。
    保留合理的人名、专有名词、技术术语、代码、网址、数字、标点和英文大小写。无法确定时，选择最贴近原输入和上下文的保守结果。
```

The immutable Swift postamble remains after this optional section and continues to enforce untrusted-input handling, preference for existing candidates, exact JSON output, a single-line result, and the length limit.

## Data Flow and Failure Behavior

No request shape or runtime code changes. Squirrel reads and normalizes `ai/instructions`, includes it in the optional-preferences section of the system message, and compares the captured configuration across asynchronous stale-result gates.

If the configured text is invalid or longer than the existing 4096-character limit, the AI request continues to fail closed while ordinary Rime candidates remain available. Ambiguous language evidence is handled through the conservative prompt policy rather than new runtime logic.

## Verification

- Update the Rime configuration regression so the shipped `ai/instructions` must be a non-empty multiline string rather than the previous empty default.
- Assert that the configured instructions retain the approved behavioral anchors: Simplified-Chinese correction, front/back nasal-final confusion, key-order and adjacent-key mistakes, existing-candidate preference, and no rewriting.
- Parse the YAML with the existing Ruby/Psych checks.
- Run `zsh -n` and `tests/config/rime-config-regression.zsh`.
- Run the focused Squirrel AI core and bridge regressions to ensure the custom layer still composes with the immutable protocol.
- Run `git diff --check`.

Changing this prompt requires only the documented Rime install and Squirrel reload path; rebuilding the custom Squirrel app is not required. Deployment to the active user configuration is outside the repository edit unless separately requested.
