# Emacs RemSpark Tracking Actions Design

## Goal

Keep the existing global Transient clock menu layout and labels, but route its resume, pause, and complete shortcuts to RemSpark instead of the local Org clock.

## Current State

- `x/transient-global-group` defines the clock shortcuts in `emacs.d/.emacs.d/lisp/x-transients.el`.
- `i` (`In last`) currently invokes `org-clock-in-last`.
- `l` (`Out`) currently invokes `org-clock-out`.
- `k` (`Done current`) currently invokes `x/org-done-current`.
- The neighboring organization, working, reading, noting, and current-clock navigation shortcuts are separate actions and should remain unchanged.
- The repository already provides the autoloaded `x/open` helper, which invokes macOS `/usr/bin/open` for a supplied target.

## Decision

Replace only the three Transient command handlers with interactive lambdas that call `x/open` using the corresponding RemSpark URL:

| Key | Existing label | RemSpark URL |
| --- | --- | --- |
| `i` | `In last` | `remspark://trackingAction?action=resume` |
| `l` | `Out` | `remspark://trackingAction?action=pause` |
| `k` | `Done current` | `remspark://trackingAction?action=complete` |

The keys and labels remain unchanged. The existing `x/open` helper keeps the implementation local to the Transient definition and avoids adding one-off command wrappers.

## Data Flow

1. The user opens `x/transient-global-group` and selects `i`, `l`, or `k`.
2. The corresponding interactive lambda passes its fixed URL to `x/open`.
3. macOS dispatches the custom URL to RemSpark.
4. RemSpark performs the requested tracking action.

## Scope

- Modify only the three handlers in `emacs.d/.emacs.d/lisp/x-transients.el`.
- Add a batch Emacs regression script for the three key-to-URL mappings and their unchanged labels.

## Non-Goals

- Do not rename the menu entries or change their keys.
- Do not remove or modify the Org clock functions themselves.
- Do not change the `g`, `w`, `r`, `n`, or `j` clock-menu actions.
- Do not add RemSpark state handling or URL-response handling to Emacs.

## Error Handling

Retain the existing `x/open` behavior. If macOS cannot dispatch a URL, Emacs exposes the command failure through the existing shell-command output; no additional retry or notification layer is needed for these fixed local URLs.

## Testing

Add a zsh regression script that loads `x-transients.el` in batch Emacs with a capturing `x/open` stub, invokes the runtime Transient suffix commands, and asserts:

- `i`, `l`, and `k` open the expected RemSpark URLs.
- Their descriptions remain `In last`, `Out`, and `Done current`.
- The module still loads successfully with its minimal batch-test stubs.
