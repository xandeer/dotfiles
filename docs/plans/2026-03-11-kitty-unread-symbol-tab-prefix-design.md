# Kitty Unread Symbol Tab Prefix Design

**Problem**

The current kitty unread marker is rendered as `[codex]` or `[claude]` inside the composed title text. It is visible, but too verbose, and it appears after the tab index because it is part of `{title}`.

**Goal**

Render unread state as a single colored `◆` marker at the very front of the tab title, before the tab index.

**Requirements**

- Use a single unread marker regardless of source.
- Render the marker as `◆`.
- Color only the marker, not the rest of the tab title.
- Place the marker before the tab index.
- Keep existing repo-aware title behavior and unread clear timing.

**Approaches Considered**

**1. Keep using `{title}` and change `[codex]` to `◆`**

This shortens the marker, but it still leaves the symbol after the tab index because `{title}` is rendered after `{index}` in the current template.

**2. Set complete tab titles from the watcher**

This would let the watcher control placement directly, but it couples tab numbering and formatting to watcher internals and duplicates kitty's built-in tab title formatting behavior.

**3. Use kitty's custom tab-title function**

Switch `tab_title_template` to `{custom}` and add `tab_bar.py` with `draw_title(data)`. Keep the watcher responsible for tagging the title with unread state, but let `draw_title()` transform that state into a colored `◆` prefix before the index.

**Recommendation**

Use approach 3.

It is the smallest change that can place the marker before the tab index without moving tab numbering logic into the watcher.

**Design**

- Keep the unread source stored in the watcher so clearing behavior does not change.
- Change the visible title marker from `[codex]` / `[claude]` to a source-agnostic `◆`.
- Update `tab_title_template` to `{custom}`.
- Add `config/.config/kitty/tab_bar.py` with `draw_title(data)` that:
  - detects whether the current title begins with `◆ `
  - strips that marker from the body text
  - emits a colored `◆ ` prefix before the tab index
  - keeps existing bell/activity symbols and progress text intact
- Use an amber/orange foreground color for `◆`.

**Testing**

- Extend `tests/config/kitty-config-regression.zsh` to expect the new `tab_title_template`.
- Add parser-backed checks for the new `tab_bar.py`.
- Change watcher expectations from `[codex]` / `[claude]` to `◆`.
- Keep unread clear timing tests unchanged.

**Files Affected**

- Modify `config/.config/kitty/kitty.conf`
- Modify `config/.config/kitty/title_watcher.py`
- Create `config/.config/kitty/tab_bar.py`
- Modify `tests/config/kitty-config-regression.zsh`

**Non-Goals**

- Keeping per-agent unread markers in the tab title.
- Changing system notification payloads.
- Reworking tab bar style beyond the unread marker prefix.
