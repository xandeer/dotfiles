# Journal Capture Bin Design

## Goal

Expose the new direct journal CLI write path as a shell command in `~/bin`.

## Decision

Add [bin/bin/journal-capture](/Users/kevin/projects/personal/dotfiles/bin/bin/journal-capture) as a thin `bash` wrapper around `emacsclient -n -e '(x/journal-capture-string ...)'`.

- When arguments are provided, join them into one text payload and send that payload to Emacs.
- When no arguments are provided and stdin is piped, read stdin as the payload instead.
- Encode the payload as base64 in the shell wrapper so multi-line input and shell quoting do not corrupt the Elisp call.

## Scope

- Create one executable wrapper in `bin/bin/`.
- Add one regression test covering both argv and stdin input modes.

## Non-Goals

- Do not add another Emacs command for file-based import.
- Do not add interactive prompts in the shell wrapper.
- Do not change the existing `x/journal-capture-string` behavior.

## Testing

Add a `tests/bin` regression that stubs `emacsclient`, runs `journal-capture` with argv and stdin inputs, and asserts the wrapper forwards:

- `-n`
- `-e`
- an Elisp expression calling `x/journal-capture-string` with the expected base64-decoded payload.
