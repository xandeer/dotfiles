# Org Capture Journal Hidden Design

## Goal

Keep the journal capture target behavior, but stop showing the journal file in the selected window while `x/find-journal-location` prepares the target buffer.

## Current State

- [x-org-capture.el](/Users/kevin/projects/personal/dotfiles/emacs.d/.emacs.d/lisp/x-org-capture.el:75) uses `(function #'x/find-journal-location)` as the target for the `j` org-capture template.
- `x/find-journal-location` currently calls `org-roam-dailies-capture-today` directly.
- `org-roam-dailies-capture-today` navigates to the daily note buffer, which means the selected window visibly switches from the capture buffer to the journal file.
- For a function target, `org-capture` only needs the current buffer and point to be correct when the function returns. It does not require the target buffer to be displayed.

## Decision

Prepare the daily journal buffer off-screen, then hand that buffer back to `org-capture` with `set-buffer`.

The function should:

1. Call `org-roam-dailies-capture-today` inside a temporary window-preserving context.
2. Record the resulting journal buffer and point.
3. Restore the original visible window state.
4. Switch the current buffer to the journal buffer without displaying it.
5. Continue with the existing heading-search and insertion logic in that hidden target buffer.

## Scope

- Modify only [x-org-capture.el](/Users/kevin/projects/personal/dotfiles/emacs.d/.emacs.d/lisp/x-org-capture.el:75).
- Add a regression test that proves the selected window keeps showing the capture buffer while the current buffer becomes the journal buffer.

## Non-Goals

- Do not change any other org-capture templates.
- Do not change the journal heading structure or insertion format.
- Do not refactor org-roam dailies setup.

## Testing

Add a batch Emacs regression script that stubs `org-roam-dailies-capture-today` to switch to a fake journal buffer, then asserts:

- `x/find-journal-location` leaves the selected window on the capture buffer.
- The current buffer after the call is the journal buffer.
- Point lands on the existing journal insertion line.
