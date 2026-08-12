# Emacs Rime AI Candidates Design

## Goal

Adapt the existing macOS Squirrel AI candidate feature to `emacs-rime` while
preserving the same request contract and native Rime candidate behavior. Local
candidates must remain immediate; a valid AI result must become Rime's first
candidate and continue through the existing auto-spacing and learning pipeline.

## Confirmed behavior

- Use the same `ai/endpoint`, `ai/model`, `ai/enabled`, and `ai/instructions`
  values as Squirrel.
- Read the bearer token through Emacs `auth-source` from `host=ark`,
  `user=gptel`; do not add another secret store.
- Send the active schema, raw Rime input, at most eight current candidates, the
  last five Emacs Rime commits, and up to 128 UTF-16 units before and after
  point.
- Debounce for 300 ms, use a four-second timeout, do not stream, retry, cache,
  or follow redirects, and discard every stale response.
- Publish the result through the existing `_ai_candidate`, `_ai_input`,
  `_ai_generation`, and `_ai_refresh` Rime session contract.
- Reuse the existing Lua candidate filter, auto-spacing filter, deduplication,
  and learned translator.
- Share one learned table at `~/Library/Rime/ai_weights.tsv` between Emacs and
  Squirrel. The accepted operating assumption is that the two frontends are not
  used for input concurrently, so no cross-process lock is added.
- Do not exclude minibuffers, remote buffers, terminals, password prompts, or
  other sensitive buffers. Whenever `emacs-rime` has an active composition, its
  bounded candidate and surrounding context may be sent to the configured API.

## Existing reusable pipeline

The repository already installs the same `rime.lua` and schema into both
`~/.cache/rime` and `~/Library/Rime`. Emacs uses `~/.cache/rime` as its user
data directory.

The shared Rime layer already provides everything after publication:

1. `ai_candidate_filter` validates the three `_ai_*` properties, promotes a
   matching genuine candidate or creates one synthetic `ai` candidate, and
   yields it first.
2. `auto_space_filter` preserves the selected candidate's provenance while
   adding a direct Han/ASCII-letter boundary space when required.
3. `uniquifier` runs last against the final display text.
4. `ai_learned_translator` records committed AI choices and contributes learned
   candidates on later matching schema/input combinations.

The Emacs adaptation therefore supplies only the missing producer and session
publication bridge. It does not create another candidate window or duplicate
the Lua learning code.

## Alternatives considered

### 1. Small `emacs-rime` C patch plus an Emacs Lisp producer

Chosen. The C patch exposes the missing librime session operations. Emacs Lisp
owns debounce, buffer context, `auth-source`, asynchronous HTTPS, validation,
and lifecycle state. This keeps Rime mutations on the Emacs event loop and
reuses the installed standard libraries.

### 2. Small C patch plus `gptel-request`

Rejected. `gptel` already supplies asynchronous requests, but the current Emacs
configuration enables debug logging, which is inappropriate for input text,
request bodies, response bodies, and authorization headers. Disabling its
logging globally or maintaining a special internal logging path is more
coupling than using Emacs's built-in URL and JSON support directly.

### 3. External helper or maintained `emacs-rime` fork

Rejected. A helper still needs the C session-property bridge and adds IPC,
process lifecycle, and another secret boundary. A permanent fork makes upstream
package updates harder for a patch that changes only a few C bindings.

## Components

### Persistent upstream patch

Store a small patch in the dotfiles repository rather than editing
`straight/repos/emacs-rime`, which is package-manager state and can be replaced
by an update.

The patch adds these bindings to `emacs-rime/lib.c`:

- `rime-lib-set-property NAME VALUE`;
- `rime-lib-get-current-schema`;
- minimal user-config readers needed to obtain strings and booleans from the
  installed `squirrel.custom.yaml` through librime's YAML parser.

The existing bindings already provide input, context, options, key processing,
and candidate data. `get-property` is not required by the runtime and is not
added unless a concrete test needs it.

A repository-owned build path copies the current upstream `lib.c` and Makefile
to Emacs runtime state, applies the patch there, and builds the dynamic module.
It never edits the Straight checkout. Patch failure is explicit, so an
incompatible upstream update cannot silently build an unpatched module.

### Emacs producer

Keep the producer in one repository-owned Emacs Lisp module. It maintains one
global state object because the current `emacs-rime` module owns one global
librime session:

- monotonically increasing generation;
- active debounce timer;
- active URL request/process;
- current immutable snapshot;
- last five committed Rime strings.

The producer hooks the normal `emacs-rime` input, keybinding, clear,
deactivation, and buffer-change paths. It does not alter candidate rendering or
selection.

### Shared runtime configuration

`rime/Makefile` already installs `squirrel.custom.yaml` into both Rime user
directories. Emacs reads `patch/ai/endpoint`, `patch/ai/model`,
`patch/ai/enabled`, and `patch/ai/instructions` from its installed copy using
librime's configuration parser. This keeps one tracked source of truth without
adding an Emacs-only endpoint or prompt.

The runtime validates the same boundaries as Squirrel:

- endpoint is a complete HTTPS URL with a non-empty host and no embedded user
  or password;
- model is non-empty and already trimmed;
- instructions contain no disallowed control characters and are at most 4096
  characters;
- disabled or incomplete configuration produces no request.

The API key comes only from:

```text
auth-source host=ark user=gptel
```

It is never copied into YAML, command arguments, environment variables, or
logs.

### Shared learned table

Add one schema setting for the learned-table path, using
`~/Library/Rime/ai_weights.tsv`. `ai_learned_translator` expands the leading
home directory and otherwise keeps its current validation, `0600` permission,
whole-file merge, temporary-file write, and atomic rename behavior.

Both frontends then read and update the same file. No database, daemon, merge
queue, or cross-process lock is added. A comment records the accepted ceiling:
concurrent commits from Emacs and Squirrel can lose one update; add locking only
if concurrent use becomes real.

## Data flow

1. A normal key event follows the existing synchronous `emacs-rime` path and
   immediately displays local candidates.
2. The producer invalidates the prior generation, clears published AI
   properties, cancels the old timer/request, and schedules a 300 ms timer when
   the composition remains non-empty.
3. When the timer fires, the producer snapshots the current buffer identity,
   point, schema, raw Rime input, current first-page candidates, recent commits,
   bounded surrounding text, and validated runtime configuration.
4. It resolves the bearer token from `auth-source`, constructs the same Chat
   Completions payload as Squirrel, and starts one non-streaming asynchronous
   HTTPS request.
5. The callback returns on the Emacs event loop. It rejects network errors,
   redirects, oversized or malformed bodies, and invalid candidate text.
6. Before mutation, it rebuilds the snapshot and requires exact ownership by
   the active generation. A different buffer, point, schema, input, candidate
   set, context, or configuration makes the response stale.
7. A valid response sets `_ai_candidate`, `_ai_input`, and `_ai_generation`,
   toggles `_ai_refresh`, and calls the normal Rime redisplay path.
8. The existing Lua filters rebuild the menu with the AI result first. Normal
   Space/number selection commits it through Rime, and the existing learned
   translator updates the shared table.

## Request and response contract

The system message reuses the configured Simplified Chinese correction
instructions and appends the same mandatory protocol rules as Squirrel. The
user message is JSON data containing only:

- `schema`;
- `input`;
- `candidates`;
- `recentCommits`;
- `surroundingBefore`;
- `surroundingAfter`.

The request uses the configured model, `stream: false`, and disabled thinking.
The response must contain exactly one choice whose message content is exactly a
JSON object with one `candidate` key. The value must be non-empty, one line, at
most 64 characters, and contain no control character. Existing leading spaces
needed by the current auto-spacing round trip remain valid.

## Lifecycle and stale-response rules

Any input, caret movement, page change, selection, commit, cancellation, Rime
deactivation, or buffer change invalidates the current generation. Cancellation
is an optimization; ownership and full snapshot comparison are the correctness
barrier when cancellation races with completion.

Because librime state is global to the current Emacs module, there is exactly
one active debounce timer and one active HTTP request across all buffers. A
buffer-local parallel-session model is out of scope.

Commit history is frontend-local request context. Only successful Emacs Rime
commits enter the five-item Emacs history, each bounded to 128 characters. The
learned table is shared, but recent in-memory request history is not.

## Failure behavior

- Missing/disabled configuration, missing `auth-source` secret, network error,
  timeout, redirect, non-success HTTP status, invalid JSON, or invalid candidate
  leaves the ordinary Rime menu unchanged.
- A stale callback performs no Rime mutation and no redisplay.
- Request/response bodies, surrounding text, candidate text, and bearer tokens
  are never logged. Diagnostics may report only a coarse failure category and
  generation number.
- Failure to secure or atomically update the learned file retains the existing
  Lua fail-closed behavior for learning without disabling live AI candidates.
- If the upstream `emacs-rime` patch no longer applies, module compilation stops
  with an actionable error instead of falling back to a module that cannot
  publish properties.

## Verification

### Automated

- Apply the stored patch to the currently pinned `emacs-rime` source in a clean
  temporary directory and compile the dynamic module against the configured
  librime and Emacs headers.
- Load the module in batch Emacs and verify the added bindings exist and can set
  properties, read the current schema, and read the installed AI configuration.
- Run an Emacs Lisp regression harness for endpoint/config validation, UTF-16
  context bounds, request JSON, strict response parsing, commit-history bounds,
  generation invalidation, and stale-response rejection.
- Extend the existing Rime AI regression to verify expansion and use of the
  shared learned-table path while preserving permissions, atomic writes,
  ordering, auto-spacing provenance, and fail-closed storage behavior.
- Run the existing complete Rime configuration regression to prevent filter
  order or Squirrel behavior regressions.

### Native acceptance

In a real Emacs session using `emacs-rime`:

- local candidates appear without waiting for the network;
- one 300 ms pause produces at most one request;
- a valid response becomes the native first candidate and Space/number keys
  select it normally;
- an existing candidate is moved rather than duplicated;
- typing, moving point, switching buffers, or cancelling before completion
  never flashes an old response;
- network and authentication failures leave normal input usable;
- the active endpoint/model/instructions match Squirrel and the secret resolves
  from `ark/gptel` in `auth-source`;
- a candidate learned in Emacs is visible to Squirrel, and one learned in
  Squirrel is visible to Emacs after the relevant composition refresh.

## Non-goals

- No settings UI, external candidate panel, helper daemon, or permanent
  `emacs-rime` fork.
- No gptel request path, streaming, retry, response cache, or multiple API key
  profiles.
- No sensitive-buffer filtering or Secure Event Input emulation.
- No multi-session or per-buffer concurrent request model.
- No database, cross-process lock, or conflict resolver for the learned table.
- No changes to native Rime user-database learning.
