# Kitty Unread Clear On Active Tab Design

**Problem**

The current kitty unread-state integration clears `[codex]` and `[claude]` title markers as soon as the window regains keyboard focus. This matches window-level focus, but it is too aggressive for multi-tab use: when kitty becomes active again, the unread marker disappears before the user actually visits the tab that produced the result.

**Goal**

Keep unread title markers until the user activates the specific tab that owns the unread result. Returning to the kitty OS window alone should not clear the marker.

**Current Behavior**

- `bin/bin/agent-notify-kitty` emits `OSC 1337 SetUserVar` with `agent_notify=codex|claude`.
- `config/.config/kitty/title_watcher.py` stores unread state per window and renders `[codex]` or `[claude]` into the composed title.
- `on_focus_change()` clears both the kitty notification and the unread marker as soon as that window becomes focused.

This is correct for a single-window mental model, but wrong once multiple tabs are involved.

**Requirements**

- Preserve unread state per kitty window.
- Do not clear unread state merely because the kitty OS window regains focus while another tab remains active.
- Clear unread state only when the tab containing the unread window becomes the active tab.
- Keep the existing repo-aware title composition behavior unchanged.
- Keep desktop notification close behavior aligned with unread-state clearing.

**Approaches Considered**

**1. Keep window-focus clearing**

This is the current behavior. It is simple, but it fails the tab-based workflow because unread state disappears too early.

**2. Clear on active-tab transition**

Track unread state per window as today, but refine clearing into two cases:

- keep `on_focus_change()` for the focused unread window itself
- add a tab-level check in the global `on_tab_bar_dirty()` callback so unread state in background tabs clears only when that tab actually becomes active

This matches the desired behavior and reuses the current per-window state model.

**3. Clear explicitly on next command or manual action**

This is robust but adds friction and changes the product behavior more than necessary.

**Recommendation**

Use approach 2.

It changes only the clear timing, preserves the existing notification and title composition model, and maps directly to the user's expectation: unread state survives until the relevant tab is actually opened.

**Design**

**State model**

- Keep `NOTIFICATION_SOURCE_BY_WINDOW_ID` as the source of unread truth.
- Keep `LAST_CHILD_TITLE_BY_WINDOW_ID` unchanged.
- Do not add a separate unread-tab data structure unless runtime inspection shows the active-tab lookup is insufficient.

**Event handling**

- `on_set_user_var()` remains the place where unread state is set and titles are refreshed.
- `on_focus_change()` continues to clear unread state only for the window that actually becomes focused.
- Add tab-aware clearing in `on_tab_bar_dirty()`, but only when the active tab changes.

**Tab-aware clearing flow**

1. Kitty receives a result from Codex or Claude for window `W`.
2. The watcher stores unread state for `W` and updates the visible title to include `[codex]` or `[claude]`.
3. When tab state changes, `on_tab_bar_dirty()` inspects the active tab in the current OS window.
4. If `W` belongs to the active tab, the watcher closes the matching kitty desktop notification and clears `agent_notify` for `W`.
5. The watcher refreshes the title for `W`, removing the unread marker.

To avoid clearing on unrelated tab-bar redraws triggered by title updates, the watcher should first detect that the active tab actually changed since the previous `on_tab_bar_dirty()` event for that tab manager.

**Tab membership lookup**

The watcher will need a helper that determines whether a given window id belongs to the current active tab. The implementation should use kitty's `tab_manager.active_tab` and the tab's exposed window listing methods rather than trying to infer tab membership from titles or focus history.

**Notification behavior**

- Keep stable notification ids so the matching notification can still be explicitly closed.
- Optionally switch notification visibility from `o=unfocused` to `o=invisible` in a follow-up if we want system notifications to align more closely with inactive-tab semantics.
- This change is not required for the title-marker fix and should stay out unless tests show it is necessary.

**Error handling**

- If tab membership cannot be resolved, leave the unread marker intact rather than clearing it early.
- If notification closing fails, still clear the unread title marker when the active tab is reached.
- Ignore unrelated `on_tab_bar_dirty()` events when no unread windows exist.

**Testing**

- Extend `tests/config/kitty-config-regression.zsh`.
- Lock in that `on_tab_bar_dirty()` exists and is responsible for active-tab clearing.
- Verify unread markers remain present when another tab's window receives focus.
- Verify unread markers are cleared only after the unread window becomes part of the active tab in a fake `tab_manager`.
- Verify same-tab `on_tab_bar_dirty()` calls do not clear unread state unless the active tab actually changed.
- Preserve all existing expectations for repo-aware titles and prompt-prefix stripping.

**Files Affected**

- Modify `config/.config/kitty/title_watcher.py`
- Modify `tests/config/kitty-config-regression.zsh`

**Non-Goals**

- Changing how Codex or Claude emit notifications.
- Reworking title formatting or tab-title templates.
- Introducing manual unread dismissal controls.
