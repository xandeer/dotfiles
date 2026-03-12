# Kitty Notification Center Clear Shortcut Design

## Goal

Add a `kitty` shortcut that triggers a best-effort Notification Center cleanup on macOS, so `ctrl+q>c` attempts to clear all currently visible notifications without blocking the active terminal session.

## Current State

- [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf) already defines a `ctrl+q>s` chord for saving the startup session and uses background shell launches for side-effect actions.
- [tests/config/kitty-config-regression.zsh](/Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh) already guards repository-managed `kitty` configuration invariants.
- The repository does not currently include any script that automates macOS Notification Center.
- macOS does not expose a stable public API for clearing all apps' Notification Center entries, so a global clear operation must use UI automation rather than `UserNotifications`.

## Decision

Use a dedicated repository-managed shell script invoked by a new `kitty` key mapping.

- Add `config/.config/kitty/kitty-clear-notification-center` as the single entry point.
- Bind `ctrl+q>c` in [config/.config/kitty/kitty.conf](/Users/kevin/projects/personal/dotfiles/config/.config/kitty/kitty.conf) to launch that script in the background.
- Implement the cleanup logic with `osascript` and `System Events`, because the target is the system Notification Center UI rather than notifications owned by one app.
- Keep the AppleScript inside the shell wrapper so the script remains executable on its own, easy to comment, and easy to inspect in regression tests.

This matches the repository's current `kitty` tooling style better than inlining a long AppleScript block in `kitty.conf`, and it keeps the automation replaceable if a future macOS release changes Notification Center structure.

## Behavior

The script should behave as a bounded, best-effort cleanup pass.

1. Open Notification Center if it is not already visible.
2. Wait briefly for the UI tree to appear.
3. Repeatedly attempt to remove notifications in this order:
   - click visible `Clear` / `Clear All` buttons, including grouped notification clears
   - fall back to per-notification dismiss or close buttons when no bulk clear controls are available
4. Stop when a pass makes no progress or a small fixed iteration limit is reached.
5. Close Notification Center before exiting so the shortcut does not leave the side panel open.

The script should treat localized button titles as data rather than one hard-coded English string. It should at least search for the English and Chinese clear/dismiss labels used on this machine class, while tolerating missing labels by skipping unmatched elements.

## Dependencies And Constraints

- The script depends on macOS `Accessibility` permission for the app that runs `osascript` and `System Events`.
- It may also require `Automation` permission for controlling `System Events`, depending on the local security prompts macOS shows.
- The current user must be in an unlocked graphical session; the shortcut is not intended for headless execution.
- Notification Center UI is not a stable API. The implementation must expect layout, role, and label changes across macOS releases and fail softly.

Because of those constraints, the feature should be described as best-effort cleanup rather than a guaranteed global clear operation.

## Error Handling

- If `System Events` is unavailable or not authorized, print a short error to stderr and exit non-zero.
- If Notification Center opens but no clearable elements are found, exit successfully without extra output.
- If one UI element cannot be clicked, ignore that element and continue the pass.
- Use a short delay between cleanup passes so Notification Center has time to redraw after a click.
- Cap the number of cleanup passes to avoid hanging when UI state does not converge.

## Verification

- Extend [tests/config/kitty-config-regression.zsh](/Users/kevin/projects/personal/dotfiles/tests/config/kitty-config-regression.zsh) to require the new shortcut mapping and the new script file.
- Add a dedicated regression script that asserts the cleanup wrapper is executable, embeds `osascript`, references `System Events`, and contains the cleanup loop and Notification Center entry points.
- Extract the AppleScript block from the wrapper during the regression test and run `osacompile` on it so syntax regressions fail in CI-friendly local runs.
- Run both focused regression tests plus `git diff --check` before claiming completion.

## Non-Goals

- Do not attempt to clear notifications through private database files or by mutating system state on disk.
- Do not add a generic macOS automation framework for other `kitty` actions.
- Do not change the existing agent notification or tab-title behavior.
- Do not guarantee that every notification from every app can be dismissed on every macOS version.
