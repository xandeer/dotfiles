# Rime AI Punctuation Guard Design

## Goal

Keep native punctuation candidates eligible for librime's direct-commit path while preserving AI candidates and learning for ordinary compositions.

## Approaches considered

1. Delete the polluted `ai_weights.tsv` rows only. This restores the current session but old code can recreate them.
2. Change punctuation YAML to `{ commit: "，" }`. This does not help because librime checks the selected candidate type before reading that mapping.
3. Reject `punct` segments at every AI candidate boundary, then remove existing punctuation rows after deployment. Chosen because it fixes both live and persisted injection without narrowing unrelated AI behavior.

## Design

Add one local `ai_segment_is_punct` predicate in `rime.lua` and reuse it at three existing boundaries:

- learned-candidate translation returns before reading storage;
- live AI filtering passes the original punctuation translation through unchanged;
- selection learning clears pending state and never writes punctuation.

Do not change the schema, punctuation mappings, filter order, learned-table format, or Squirrel bridge. Existing punctuation rows become inert as soon as the new Lua code loads. Runtime cleanup happens only after deployment, using the exact schema/input keys confirmed by the audit.

## Verification

Extend the existing Lua regression harness with a `punct` segment mock. The test must first fail on current production code, then prove that learned translation yields nothing, live filtering preserves the native punctuation candidate object, and select/commit emits no storage write. Run the full Rime configuration regression and an isolated native librime comma harness before any live deployment.

## Acceptance criteria

- Physical `,` directly commits `，` with polluted punctuation rows present.
- AI behavior for non-punctuation segments remains unchanged.
- Punctuation selection cannot create or increment AI learning rows.
- No repository or runtime data outside the isolated worktree changes before deployment approval.

## Digit-separator regression

Librime 1.16 tags `.` after a committed digit as `punct_number`, not `punct`, so the original guard misses the native decimal separator composition. Treat both tags as punctuation at the same three AI boundaries. Preserve librime's native behavior: `1.` remains an in-progress ASCII decimal separator, a following digit commits the number, and ordinary `.` still maps to `。`.

Verify this with the existing Lua harness using a `punct_number` segment, then remove only the exact runtime row `double_pinyin_flypy\t.\t。` after deployment.
