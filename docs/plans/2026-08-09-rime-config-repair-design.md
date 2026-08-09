# Rime Config Repair Design

## Goal

Make every actively registered Rime component usable and make a fresh deployment complete without dictionary encoding errors.

## Chosen approach

Use the smallest configuration-only repair:

- remove the unconfigured `reduce_english_filter`;
- remove the incomplete `xklbdz` translator and recognizer entry;
- add the missing recognizer pattern for the existing Unicode translator;
- replace the two invalid dictionary separators with tabs;
- stop importing the full-pinyin-only `cn_en` table into the Xiaohe double-pinyin English dictionary.

Keep `rime.lua`, Squirrel installation, grammar distribution, switch persistence, and install destinations unchanged.

## Alternatives considered

1. Restore and configure every dormant feature. Rejected because it needs additional dictionaries and filter data the current setup does not contain.
2. Apply the focused repair above. Chosen because it fixes all agreed failures with three configuration files.
3. Replace the configuration with current upstream Rime Ice. Rejected as an unrelated migration with much larger behavior changes.

## Verification

A small zsh regression checks the intended active configuration. A fresh bundled-Squirrel deployment must exit successfully without `Encode failure`, and the generated schema must contain the Unicode recognizer while excluding the removed components.
