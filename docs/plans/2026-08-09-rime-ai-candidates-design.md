# macOS Rime AI Candidates Design

## Goal

Add cloud-assisted candidates and independent local ranking to Rime on macOS. The AI result must appear as the first item in Squirrel's native candidate list without blocking ordinary input or changing Rime's native user database.

## Confirmed behavior

- Target macOS and Squirrel 1.1.2 only.
- Enable the feature in every normal text field. Secure text fields never send requests.
- Use a user-configured OpenAI-compatible endpoint and model.
- Read the API key from macOS Keychain; never store it in YAML, Git, logs, or command arguments.
- Use both recent Rime commits and text around the macOS insertion point as context when available.
- Start a request after 300 ms without composition changes.
- Cancel in-flight work and discard stale responses when the input, caret, schema, session, or client changes.
- Prefer an existing Rime candidate, but allow the model to return a new candidate when none fits well.
- Put a valid live AI result at candidate position one. Do not commit it automatically.
- Persist learned AI entries in a separate table and merge their weights into later candidate ordering without writing Rime's native user database.

## Alternatives considered

1. **Pure Lua network translator.** Rejected because librime-lua network calls are synchronous. A request would block the input method, and Lua has no asynchronous completion callback that can safely refresh a later candidate list.
2. **External AI candidate window.** Rejected because it would not make the AI result the first native Rime candidate and would create a second focus and selection path.
3. **Custom librime C++ plugin.** Feasible but rejected for the first version because it adds a separate binary, ABI compatibility work, and more upgrade maintenance than necessary.
4. **Small Squirrel patch plus Lua candidate components.** Chosen. Squirrel already owns macOS input context and the main-thread Rime session, while Lua already owns the repository's candidate pipeline.

## Architecture

### Squirrel bridge

A small patch based on Squirrel 1.1.2 performs only work that Lua cannot do safely:

- observe composition changes and maintain a 300 ms debounce timer;
- collect the current Rime input, a bounded set of local candidates, recent commits, and a bounded surrounding-text window from the active `IMKTextInput` client;
- call the configured OpenAI-compatible endpoint asynchronously with `URLSession`;
- retrieve the bearer token directly through Security.framework;
- validate that the response still belongs to the active generation;
- publish the validated result into Rime context properties on the main thread;
- trigger a non-destructive candidate refresh and run the existing Squirrel update path.

The patch does not change librime itself. The stock Squirrel 1.1.2 application is the source baseline, but a custom build is required because stock Squirrel does not contain this asynchronous bridge.

### Rime Lua components

The existing `rime.lua` integration gains two small responsibilities:

1. A late candidate filter reads the live AI result and its input/generation metadata. When the metadata matches the current composition, it yields that candidate first, removes a duplicate from the incoming stream, and then yields all remaining candidates.
2. A local learned-candidate component reads an independent AI table and contributes matching entries with their stored weight. Accepted AI suggestions and later user choices update this table. Native Rime learning continues unchanged.

The live network result always outranks the learned table. Learned entries participate below it alongside the ordinary candidate stream.

### Configuration and storage

The existing `squirrel.custom.yaml` contains only the non-secret AI configuration, reusing Squirrel's current `SquirrelConfig` reader:

```yaml
patch:
  ai/endpoint: https://example.com/v1/chat/completions
  ai/model: example-model
```

The endpoint must be HTTPS and must not contain embedded credentials. Debounce, timeout, context bounds, and candidate-count bounds remain fixed implementation constants until a real need to tune them appears.

The API key is a generic-password item in the login Keychain:

- service: `im.rime.inputmethod.Squirrel.ai`
- account: `default`

Write or update it with an interactive prompt:

```bash
/usr/bin/security add-generic-password \
  -U \
  -a default \
  -s im.rime.inputmethod.Squirrel.ai \
  -T "/Library/Input Methods/Squirrel.app" \
  -w
```

Placing `-w` last makes `security` prompt instead of receiving the secret in an argument. Squirrel reads the item with `SecItemCopyMatching`. A missing or inaccessible item disables only the cloud request for that composition.

Delete the item with:

```bash
/usr/bin/security delete-generic-password \
  -a default \
  -s im.rime.inputmethod.Squirrel.ai
```

The learned table lives at `~/Library/Rime/ai_weights.tsv` and is runtime data, not a stowed or committed file. Each record identifies the schema, input code, candidate text, weight, and last-use time. Writes use a temporary file followed by an atomic replacement so interruption cannot leave a partially written table.

## Request and response contract

The request uses the configured endpoint as a Chat Completions-compatible URL and sends:

- the configured model;
- the active schema and raw composition input;
- a bounded list of current local candidates;
- a bounded recent-commit history;
- a bounded window before and after the insertion point when the client exposes it;
- an instruction to choose an existing candidate when possible and otherwise produce one new candidate.

The assistant response must contain one structured candidate result. The bridge accepts only a non-empty, single-line string within the configured internal length bound. It rejects control characters, malformed JSON, multiple results, and responses that no longer match the active generation. If the result duplicates an existing candidate, the Lua filter moves the existing candidate to first rather than displaying two copies.

Requests do not stream and do not retry. This keeps cancellation, cost, and ordering deterministic.

## Data flow

1. Each key event follows the normal synchronous Rime path, so local candidates appear immediately.
2. A composition change clears the previous live AI result, increments a generation token, cancels the previous task, and schedules a 300 ms timer.
3. When the timer fires, Squirrel snapshots the session, schema, input, caret, candidates, recent commits, and available surrounding text.
4. `URLSession` sends the request without blocking the input method thread.
5. Completion returns to the main thread. Squirrel compares every identity field and drops a stale result.
6. A valid result is stored in Rime context properties together with its input and generation. Squirrel toggles a transient refresh option and invokes its normal candidate update.
7. The Lua filter validates the metadata, yields the AI candidate first, deduplicates it, and passes through the remaining candidates.
8. Commit or cancellation clears the transient result. A committed selection updates the independent learned table.

## Failure and privacy behavior

- Missing configuration, missing Keychain access, timeout, network failure, rate limiting, invalid JSON, or invalid candidate text produces no AI candidate. Ordinary Rime behavior remains intact.
- Any edit, selection movement, schema switch, focus change, commit, or cancellation makes an older response ineligible even if task cancellation races with completion.
- Secure input fields never collect context or start a request.
- If an application does not expose surrounding text through InputMethodKit, the request falls back to Rime input, candidates, and recent commit history. The first version does not request Accessibility permission as a workaround.
- Request and response bodies and bearer tokens are never logged. Diagnostics may record only status categories, timing, and generation identifiers.

## Verification

Keep automated coverage small and focused:

- Swift checks for response parsing, candidate validation, Keychain-missing fallback, and stale-generation rejection;
- Lua checks that a live AI candidate is first, duplicates are removed, ordinary ordering survives without a live result, and learned weights persist and reload;
- a build check against the pinned Squirrel 1.1.2 source;
- a clean Rime deployment check using the repository's existing install path.

Manual acceptance covers a native AppKit application, a browser, and an Electron application:

- local candidates appear without waiting for the network;
- one pause produces at most one request;
- continued typing never flashes an older AI result;
- a valid AI result becomes native candidate one and Space selects it normally;
- duplicate candidates are not displayed twice;
- API failure leaves the candidate list usable;
- accepted learned entries remain available after Squirrel restarts;
- password fields send no request;
- applications without surrounding-text support still work with reduced context.

## Non-goals

- No automatic commit of AI text.
- No external candidate overlay.
- No librime core fork or new C++ plugin.
- No Linux, Windows, iOS, or per-application behavior in the first version.
- No Accessibility permission, streaming response, retry queue, settings UI, or multi-key profile management.
- No mutation of Rime's native user database.
