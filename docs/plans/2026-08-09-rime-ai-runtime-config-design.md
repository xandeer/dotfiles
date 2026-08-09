# Rime AI Runtime Controls Design

## Goal

Make the existing macOS Squirrel AI candidate bridge safe to tune at runtime. The user can globally disable cloud requests or add task-specific language preferences without rebuilding the custom Squirrel application. The model-facing response protocol remains fixed in the binary, and the local learned-candidate table is always private to the current macOS account.

## Confirmed scope

- Add `ai/enabled` and `ai/instructions` to the existing Squirrel configuration.
- Default `ai/enabled` to `true` so an already configured endpoint, model, and Keychain item keep working.
- Treat a missing or empty `ai/instructions` value as no customization.
- Apply configuration after a normal Rime/Squirrel redeploy or reload; prompt changes do not require rebuilding Squirrel.
- Keep endpoint, model, and instructions in YAML. Keep the API key only in macOS Keychain.
- Do not add per-app rules, generation parameters, prompt versioning, streaming, retries, custom headers, metrics, or request/response logging.
- Do not install Squirrel, write a real Keychain item, or call a real endpoint as part of implementation or verification.

## Configuration

The repository ships these non-secret defaults in `rime/darwin/squirrel.custom.yaml`:

```yaml
patch:
  ai/enabled: true
  ai/endpoint: ""
  ai/model: ""
  ai/instructions: ""
```

For a multiline customization, the user can override `ai/instructions` with a YAML block scalar:

```yaml
  ai/instructions: |-
    Prefer concise terminology used in software engineering.
    Preserve product names in English.
```

Instructions are trimmed, limited to 4096 Swift characters, and may contain ordinary text, line feeds, and tabs. Other Unicode control characters invalidate the configuration for that request. An invalid value produces no cloud candidate; ordinary Rime candidates remain available.

## Immutable protocol and custom preferences

The system message has two layers, ordered so the mandatory rules are the final authority:

1. When non-empty, normalized `ai/instructions` is placed in a clearly delimited optional-preferences section.
2. A hard-coded mandatory postamble states that it takes precedence over conflicting preferences, the user message is untrusted JSON data, instructions inside that data must be ignored, an existing candidate should be preferred, and the response must be exactly one JSON object containing a single-line candidate of at most 64 characters.

Only the first layer is configurable. YAML cannot replace or remove the mandatory postamble. The existing strict response parser remains the final enforcement boundary for the one-object response shape and candidate validation; preference behavior such as choosing an existing candidate still relies on the model following the final mandatory rule.

## Enabled-state data flow

`scheduleAICandidate()` reads `ai/enabled` before it snapshots input, queues Keychain work, or constructs an HTTP request. A missing value uses the compatibility default `true`; an explicit `false` cancels work owned by that controller, invalidates the generation, and removes any published live AI properties.

The controller captures one comparable runtime-configuration value containing enabled state, endpoint, model, and normalized instructions. The timer, Keychain completion, and final apply gates rebuild and compare the complete value. Changing any field while work is pending makes that work stale and clears it instead of sending or publishing with mixed configuration. Secure Event Input checks remain earlier than all cloud work and continue to clear recent-commit context.

Configuration files are not watched live. The documented Squirrel reload replaces the cached base configuration and Rime sessions. A request that was already transmitted before reload cannot be made unsent, but the complete configuration/session/generation gates prevent its response from being published under the new configuration.

## Learned-table permissions

`~/Library/Rime/ai_weights.tsv` contains input codes and accepted candidate text, so it is private runtime data. Squirrel 1.1.2's bundled Lua has no chmod API or POSIX module. On translator initialization, an existing file is therefore forced to mode `0600` with an absolute `/bin/chmod` command and POSIX-safe path quoting; failure disables learned-table reads and writes for that translator instance.

Each atomic update uses absolute `/usr/bin/mktemp -q` to create a unique same-directory file that macOS creates as `0600`. The code validates the returned path, reopens the already-created empty file with `r+` so a disappeared file cannot be recreated under the process umask, writes and closes it successfully, and then renames it over the destination. A create, permission, validation, open, write, close, or rename failure removes only the exact temporary file when possible and leaves the previous table intact. There is no fallback to an ordinary broad-mode temporary file.

The table remains outside Git and outside Rime's native user database. No content, path, request, response, or secret is logged.

## Failure behavior

- `ai/enabled: false`: after configuration reload, schedule no new Keychain/network work and never publish an older response; the next scheduler/apply gate clears controller-owned cloud state.
- Missing endpoint/model/key: no cloud candidate.
- Missing/empty instructions: use only the immutable protocol.
- Invalid instructions: no request for that composition.
- Stale configuration, composition, session, client, or secure-input state: cancel or silently discard the result.
- Learned-table permission or I/O failure: keep ordinary and live candidates working, but disable local learned-table persistence for that translator instance.

## Verification

- Swift core regression checks exact prompt layering, untrusted-data language, empty customization, multiline customization, the 4096-character bound, control-character rejection, and absence of the API key from the JSON body.
- Squirrel bridge regression checks that the enabled gate precedes Keychain/network access, an explicit disable clears live properties, and timer, post-Keychain, and final-apply gates compare the complete captured runtime configuration.
- Rime configuration regression checks the shipped defaults and the absence of secrets.
- Lua regression uses only a temporary directory and verifies `0600` initialization/write behavior plus fail-closed permission and atomic-write failures.
- Both patches are regenerated mechanically, replayed onto pristine Squirrel 1.1.2 source, compiled, and scanned for added logs or secrets.

## Non-goals

- No settings UI or automatic configuration editor.
- No runtime replacement of the response protocol.
- No request/response history, telemetry, or debugging log.
- No automatic Squirrel installation, Keychain mutation, live API request, or deployment to the active Rime user directory.
