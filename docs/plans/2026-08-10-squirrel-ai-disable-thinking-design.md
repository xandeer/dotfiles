# Squirrel AI Disable Thinking Design

## Context

The installed Squirrel AI bridge is active, but its log has never recorded the
`_ai_refresh` transition that follows a successfully parsed cloud candidate.
The request currently uses a four-second timeout and sends a non-streaming chat
completion without explicitly disabling provider-side thinking.

## Goal

Make every Squirrel AI chat-completion request explicitly non-thinking and
non-streaming so the provider can return the short IME candidate with minimum
latency.

## Request contract

The encoded JSON request body will contain:

```json
{
  "model": "<configured model>",
  "messages": [],
  "thinking": {
    "type": "disabled"
  },
  "stream": false
}
```

`thinking` is a fixed protocol field, not user configuration. `stream` remains
fixed at `false`. The API key stays only in the Authorization header.

## Scope

- Extend the Swift request encoder with the fixed `thinking.type` value.
- Add a core regression assertion that decodes the actual request body and
  requires both `thinking.type == "disabled"` and `stream == false`.
- Regenerate only the request-core patch; the controller bridge patch should
  remain source-equivalent except for patch ancestry metadata if regeneration
  requires it.
- Rebuild and reinstall the patched Squirrel app, preserving the current Rime
  configuration and Keychain item.

## Non-goals

- Do not change the four-second timeout.
- Do not loosen response parsing.
- Do not add request, response, candidate, context, or API-key logging.
- Do not enable streaming or add provider-specific runtime configuration.

## Verification

Use test-driven development: first make the core regression fail against the
current patch, then implement the minimal encoder change and make it pass.
Replay both patches on the pinned Squirrel 1.1.2 base, run the core and bridge
regressions, run a full debug build, verify the installed signature and loaded
artifact, and finally confirm a candidate publication through the existing
`_ai_refresh` lifecycle marker without logging sensitive content.
