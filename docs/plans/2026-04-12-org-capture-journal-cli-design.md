# Org Capture Journal CLI Design

## Goal

Allow command-line callers to append journal content without creating a `*Capture*` buffer or entering the `org-capture` UI.

## Decision

Add a direct helper command, `x/journal-capture-string`, instead of trying to force `org-capture-string` into a non-UI workflow.

- Reuse the existing journal target discovery logic so CLI writes land in the same daily location as interactive journal capture.
- Insert one inline timestamped journal entry directly into the journal buffer.
- Save the journal file and return without changing the selected window or current buffer.

## Scope

- Modify [x-org-capture.el](/Users/kevin/projects/personal/dotfiles/emacs.d/.emacs.d/lisp/x-org-capture.el:121).
- Update the CLI regression to call `x/journal-capture-string` directly.

## Non-Goals

- Do not change the existing `j` interactive capture template.
- Do not keep a separate `J` template if the direct command replaces it.
- Do not route CLI journal entry creation through `org-capture`.

## Testing

Run a batch Emacs regression that stubs the journal target, calls `x/journal-capture-string`, and asserts:

- the original selected/current buffer stays in place,
- no `*Capture*` buffer is required,
- the journal buffer gets a new inline timestamped entry containing the provided text.
