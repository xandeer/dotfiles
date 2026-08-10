# Squirrel AI Stable Snapshot Design

## Context

The installed Squirrel AI bridge reaches its 300 ms debounce callback but never
opens a network connection. `currentAISnapshot()` currently stores
`client.uniqueClientIdentifierString()` and later requires a newly constructed
snapshot to equal the original one.

The InputMethodKit SDK documents `uniqueClientIdentifierString()` as a wrapper
around `NSProcessInfo.globallyUniqueString`. Repeated calls produce different
values. The debounce snapshot can therefore never equal the original snapshot,
so every request is invalidated before the Keychain read.

## Goal

Make an unchanged composition produce equal AI snapshots across debounce and
asynchronous lifecycle checks, while continuing to reject stale sessions,
inputs, applications, candidates, history, and surrounding context.

## Design

- Remove `clientID` from `SquirrelAISnapshot`.
- Stop calling `uniqueClientIdentifierString()` in `currentAISnapshot()`.
- Keep the existing snapshot fields: Rime session, generation, schema, input,
  caret, application bundle identifier, candidates, recent commits, and bounded
  surrounding context.
- Keep the existing `ownsAICandidate` check and all invalidation boundaries.
  `session + generation` own the asynchronous request, while `appID` and the
  remaining snapshot data detect relevant state changes.
- Continue invalidating on controller activation/deactivation, session
  creation/destruction, composition commit/cancel, key processing, selection,
  paging, caret movement, and controller destruction.

Caching one call to `uniqueClientIdentifierString()` is deliberately avoided:
it would add controller state for a value that provides no stronger ownership
guarantee than the existing lifecycle and generation guards.

## Security and privacy

This change does not broaden data collection or transmission. It does not
change the request body, endpoint, model, API key handling, surrounding-context
bounds, secure-input gates, response parser, or logging. No diagnostic logging
is added because the root cause is now directly proven.

## Verification

Use test-driven development:

1. Change the pure Swift fixture to construct snapshots without `clientID` and
   require all retained stale-gate fields to affect equality.
2. Change the bridge contract to reject any
   `uniqueClientIdentifierString()` call in snapshot construction while keeping
   the session, generation, and application guards.
3. Confirm both tests fail against the current patch series.
4. Remove only the core snapshot field and controller initializer argument.
5. Regenerate both format patches mechanically from the pinned Squirrel 1.1.2
   base, replay them in a clean checkout, and run core, bridge, and Rime
   regressions.
6. Build and verify a Release bundle, install only the exact Squirrel app, then
   confirm a harmless stationary composition reaches the network/publication
   lifecycle without inspecting sensitive values.
