# Emacs RemSpark Tracking Actions Design

## Goal

Keep the existing global Transient clock menu layout and labels, but route its resume, pause, and complete shortcuts to RemSpark instead of the local Org clock.

## Current State

- `x/transient-global-group` defines the clock shortcuts in `emacs.d/.emacs.d/lisp/x-transients.el`.
- `i` (`In last`) currently invokes `org-clock-in-last`.
- `l` (`Out`) currently invokes `org-clock-out`.
- `k` (`Done current`) currently invokes `x/org-done-current`.
- The neighboring organization, working, reading, noting, and current-clock navigation shortcuts are separate actions and should remain unchanged.
- `init.el` loads `x-start-process` before `x-transients`; the helper turns a fixed command string into the argv list supplied as `make-process :command`.
- The older `x/open` helper concatenates its target into an unquoted shell command. A RemSpark URL contains `?`, which zsh treats as a glob, so that helper is not safe for these actions.

## Decision

Replace only the three Transient command handlers with interactive lambdas that call `x/start-process` using `/usr/bin/open` and the corresponding RemSpark URL:

| Key | Existing label | RemSpark URL |
| --- | --- | --- |
| `i` | `In last` | `remspark://trackingAction?action=resume` |
| `l` | `Out` | `remspark://trackingAction?action=pause` |
| `k` | `Done current` | `remspark://trackingAction?action=complete` |

The keys and labels remain unchanged. Each fixed command becomes exactly two `make-process` argv elements: `/usr/bin/open` and the complete RemSpark URL. This bypasses shell expansion while keeping the implementation local to the Transient definition and avoiding one-off command wrappers.

## Data Flow

1. The user opens `x/transient-global-group` and selects `i`, `l`, or `k`.
2. The corresponding interactive lambda passes a fixed `/usr/bin/open <URL>` command to `x/start-process`.
3. `x/start-process` supplies `make-process` with `:command ("/usr/bin/open" "remspark://trackingAction?action=...")`.
4. macOS dispatches the custom URL to RemSpark without a shell interpreting `?`.
5. RemSpark performs the requested tracking action.

## Scope

- Modify only the three handlers in `emacs.d/.emacs.d/lisp/x-transients.el`.
- Add a batch Emacs regression script for the three key-to-URL mappings and their unchanged labels.

## Non-Goals

- Do not rename the menu entries or change their keys.
- Do not remove or modify the Org clock functions themselves.
- Do not change the `g`, `w`, `r`, `n`, or `j` clock-menu actions.
- Do not add RemSpark state handling or URL-response handling to Emacs.

## Error Handling

Retain the existing `x/start-process` behavior. Process creation receives an explicit argv list, so the URL is never subject to shell glob expansion. If `/usr/bin/open` cannot launch or dispatch the URL, the existing process error and sentinel reporting remain responsible for surfacing the failure; no additional retry or notification layer is needed.

## Testing

Add a zsh regression script that loads the real `x-start-process.el` and `x-transients.el` in batch Emacs, stubs only the process boundary (`make-process` and `set-process-sentinel`), captures every real `make-process :command` value, and invokes the runtime Transient suffix commands. Keep `x/open` as a rejecting stub so an accidental shell-based fallback cannot pass. Assert:

- Exactly three process dispatches occur.
- Each dispatch contains exactly two argv elements: `/usr/bin/open` and the expected RemSpark URL for `i`, `l`, or `k`.
- Their descriptions remain `In last`, `Out`, and `Done current`.
- The module still loads successfully with its minimal batch-test stubs.
