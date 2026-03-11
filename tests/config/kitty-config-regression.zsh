#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
kitty_conf="$repo_root/config/.config/kitty/kitty.conf"
title_watcher_py="$repo_root/config/.config/kitty/title_watcher.py"
tab_bar_py="$repo_root/config/.config/kitty/tab_bar.py"

[[ -f "$kitty_conf" ]] || {
  print -u2 "expected repository-managed kitty config at $kitty_conf"
  exit 1
}

rg -Fx 'allow_remote_control no' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty remote control to stay disabled by default"
  exit 1
}

if rg -Fx 'tab_bar_style custom' "$kitty_conf" >/dev/null; then
  print -u2 "expected kitty single-tab titles to avoid the ineffective custom tab bar path"
  exit 1
fi

rg -Fx 'watcher title_watcher.py' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to load a title watcher for all windows"
  exit 1
}

if rg -F 'shell_integration no-rc' "$kitty_conf" >/dev/null; then
  print -u2 "expected kitty shell integration to use the default mode"
  exit 1
fi

rg -Fx 'tab_title_template "{custom}"' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to use a custom tab title renderer"
  exit 1
}

rg -Fx 'tab_bar_edge top' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to place the tab bar on the top edge"
  exit 1
}

rg -Fx 'hide_window_decorations titlebar-only' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to hide only the macOS title bar"
  exit 1
}

rg -Fx 'window_margin_width 6' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to use a safe window margin for titlebar-only mode"
  exit 1
}

rg -Fx 'placement_strategy center' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty to center extra cell space"
  exit 1
}

[[ -f "$title_watcher_py" ]] || {
  print -u2 "expected kitty title watcher at $title_watcher_py"
  exit 1
}

[[ -f "$tab_bar_py" ]] || {
  print -u2 "expected kitty custom tab title renderer at $tab_bar_py"
  exit 1
}

rg -F 'def on_title_change' "$title_watcher_py" >/dev/null || {
  print -u2 "expected kitty title watcher to react to child title changes"
  exit 1
}

rg -F 'def draw_title' "$tab_bar_py" >/dev/null || {
  print -u2 "expected kitty custom tab title renderer to define draw_title"
  exit 1
}

rg -F 'def on_set_user_var' "$title_watcher_py" >/dev/null || {
  print -u2 "expected kitty title watcher to react to unread-state user vars"
  exit 1
}

rg -F 'def on_focus_change' "$title_watcher_py" >/dev/null || {
  print -u2 "expected kitty title watcher to react to focus changes"
  exit 1
}

rg -F 'def on_tab_bar_dirty' "$title_watcher_py" >/dev/null || {
  print -u2 "expected kitty title watcher to react to active tab changes"
  exit 1
}

rg -F 'window.set_window_title' "$title_watcher_py" >/dev/null || {
  print -u2 "expected kitty title watcher to update the visible window title"
  exit 1
}

rg -F 'window.set_user_var' "$title_watcher_py" >/dev/null || {
  print -u2 "expected kitty title watcher to clear unread notification state when it is consumed"
  exit 1
}

temp_root="$(mktemp -d)"
trap 'rm -rf "$temp_root"' EXIT

mkdir -p "$temp_root/repo/.git" "$temp_root/repo/src" "$temp_root/plain/nested" "$temp_root/worktree/pkg"
: > "$temp_root/worktree/.git"

watcher_state="$(
  kitty +runpy 'import importlib.util, sys
import kitty.tab_bar as kitty_tab_bar
spec = importlib.util.spec_from_file_location("repo_title_watcher", sys.argv[1])
module = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(module)
tab_bar_spec = importlib.util.spec_from_file_location("repo_tab_bar", sys.argv[9])
tab_bar = importlib.util.module_from_spec(tab_bar_spec)
assert tab_bar_spec.loader is not None
tab_bar_spec.loader.exec_module(tab_bar)
print("repo={}".format(module.repo_name_for_path(sys.argv[2]) or ""))
print("worktree={}".format(module.repo_name_for_path(sys.argv[3]) or ""))
print("plain={}".format(module.repo_name_for_path(sys.argv[4]) or ""))
print("repo_title={}".format(module.compose_window_title(sys.argv[2], sys.argv[5])))
print("dedup_title={}".format(module.compose_window_title(sys.argv[2], sys.argv[6])))
print("plain_title={}".format(module.compose_window_title(sys.argv[4], sys.argv[7])))
print("codex_title={}".format(module.compose_window_title(sys.argv[2], sys.argv[5], "codex")))
print("claude_title={}".format(module.compose_window_title(sys.argv[2], sys.argv[5], "claude")))
print("prompt_title={}".format(module.compose_window_title(sys.argv[2], sys.argv[8])))

class FakeProgress:
    last_focused_progress_percent = "42%"


runtime_fmt = kitty_tab_bar.Formatter

print("custom_plain={}".format(tab_bar.draw_title({
    "title": "repo | shell",
    "index": 2,
    "fmt": runtime_fmt,
    "bell_symbol": "",
    "activity_symbol": "",
    "tab": FakeProgress(),
})))
print("custom_unread_ok={}".format(tab_bar.draw_title({
    "title": "◆ repo | shell",
    "index": 2,
    "fmt": runtime_fmt,
    "bell_symbol": "",
    "activity_symbol": "",
    "tab": FakeProgress(),
}) == f"{runtime_fmt.fg._FFB000}◆ {runtime_fmt.fg.default}2: 42%repo | shell"))
print("custom_legacy_unread_ok={}".format(tab_bar.draw_title({
    "title": "[codex] repo | shell",
    "index": 2,
    "fmt": runtime_fmt,
    "bell_symbol": "",
    "activity_symbol": "",
    "tab": FakeProgress(),
}) == f"{runtime_fmt.fg._FFB000}◆ {runtime_fmt.fg.default}2: 42%repo | shell"))

class FakeNotificationManager:
    def __init__(self):
        self.calls = []

    def handle_notification_cmd(self, channel_id, osc_code, raw):
        self.calls.append((channel_id, osc_code, raw))


class FakeWindow:
    def __init__(self, window_id, cwd, title):
        self.id = window_id
        self.cwd_of_child = cwd
        self.title = title
        self.user_vars = {}

    def get_cwd_of_root_child(self):
        return self.cwd_of_child

    def set_window_title(self, title):
        self.title = title

    def set_user_var(self, key, value):
        self.user_vars[key] = value


class FakeBoss:
    def __init__(self):
        self.notification_manager = None
        self.all_windows = []


class FakeTab:
    def __init__(self, windows):
        self._windows = list(windows)

    def list_windows(self):
        return [{"id": window.id} for window in self._windows]


class FakeTabManager:
    def __init__(self, active_tab):
        self.active_tab = active_tab


boss = FakeBoss()
boss.notification_manager = FakeNotificationManager()
window = FakeWindow(7, sys.argv[2], "shell")
other_window = FakeWindow(8, sys.argv[2], "logs")
boss.all_windows = [window, other_window]
tab_manager = FakeTabManager(FakeTab([other_window]))
module.on_tab_bar_dirty(boss, other_window, {"tab_manager": tab_manager})
module.on_title_change(boss, window, {"title": "shell", "from_child": True})
module.on_set_user_var(boss, window, {"key": "agent_notify", "value": "codex"})
print("focus_title_before={}".format(window.title))
module.on_focus_change(boss, other_window, {"focused": True})
print("other_focus_title_after={}".format(window.title))
print("other_focus_source_after={}".format(module.notification_source_for_window(window)))
tab_manager.active_tab = FakeTab([other_window])
module.on_tab_bar_dirty(boss, window, {"tab_manager": tab_manager})
print("inactive_tab_title_after={}".format(window.title))
print("inactive_tab_source_after={}".format(module.notification_source_for_window(window)))
tab_manager.active_tab = FakeTab([window])
module.on_tab_bar_dirty(boss, window, {"tab_manager": tab_manager})
print("active_tab_title_after={}".format(window.title))
print("active_tab_source_after={}".format(module.notification_source_for_window(window)))
print("active_tab_user_var={}".format(window.user_vars.get("agent_notify")))
print("active_tab_close={}".format(boss.notification_manager.calls[0][2] if boss.notification_manager.calls else ""))

sticky_window = FakeWindow(9, sys.argv[2], "build")
boss.all_windows = [window, other_window, sticky_window]
module.on_title_change(boss, sticky_window, {"title": "build", "from_child": True})
sticky_tab_manager = FakeTabManager(FakeTab([sticky_window]))
module.on_tab_bar_dirty(boss, sticky_window, {"tab_manager": sticky_tab_manager})
module.on_set_user_var(boss, sticky_window, {"key": "agent_notify", "value": "claude"})
print("same_tab_before_dirty={}".format(sticky_window.title))
sticky_tab_manager.active_tab = FakeTab([sticky_window])
module.on_tab_bar_dirty(boss, sticky_window, {"tab_manager": sticky_tab_manager})
print("same_tab_after_dirty={}".format(sticky_window.title))
print("same_tab_source_after={}".format(module.notification_source_for_window(sticky_window)))' \
    "$title_watcher_py" \
    "$temp_root/repo/src" \
    "$temp_root/worktree/pkg" \
    "$temp_root/plain/nested" \
    "nvim" \
    "repo" \
    "shell" \
    "~/projects/personal/remio> codex" \
    "$tab_bar_py"
)"

print -r -- "$watcher_state" | rg -Fx 'repo=repo' >/dev/null || {
  print -u2 "expected kitty title watcher to resolve git repo roots from nested directories"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'worktree=worktree' >/dev/null || {
  print -u2 "expected kitty title watcher to handle git worktree .git files"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'plain=' >/dev/null || {
  print -u2 "expected kitty title watcher to return no repo name outside git repositories"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'repo_title=repo | nvim' >/dev/null || {
  print -u2 "expected kitty title watcher to prefix repo names ahead of command titles"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'dedup_title=repo' >/dev/null || {
  print -u2 "expected kitty title watcher to avoid repeating identical repo and title text"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'plain_title=nested | shell' >/dev/null || {
  print -u2 "expected kitty title watcher to fall back to directory names outside git repos"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'codex_title=◆ repo | nvim' >/dev/null || {
  print -u2 "expected kitty title watcher to add a compact unread marker to titles"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'claude_title=◆ repo | nvim' >/dev/null || {
  print -u2 "expected kitty title watcher to reuse the same unread marker for Claude titles"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'prompt_title=repo | codex' >/dev/null || {
  print -u2 "expected kitty title watcher to strip repeated prompt paths ahead of command titles"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'custom_plain=2: 42%repo | shell' >/dev/null || {
  print -u2 "expected kitty custom tab title renderer to keep the index, progress, and title ordering under the real kitty formatter"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'custom_unread_ok=True' >/dev/null || {
  print -u2 "expected kitty custom tab title renderer to place a colored unread marker before the tab index under the real kitty formatter"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'custom_legacy_unread_ok=True' >/dev/null || {
  print -u2 "expected kitty custom tab title renderer to preserve unread styling for legacy watcher prefixes"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'focus_title_before=◆ repo | shell' >/dev/null || {
  print -u2 "expected kitty title watcher to show unread state before focus returns"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'other_focus_title_after=◆ repo | shell' >/dev/null || {
  print -u2 "expected kitty title watcher to preserve unread state when another tab's window receives focus"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'other_focus_source_after=codex' >/dev/null || {
  print -u2 "expected kitty title watcher to keep unread source when another tab's window receives focus"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'inactive_tab_title_after=◆ repo | shell' >/dev/null || {
  print -u2 "expected kitty title watcher to preserve unread state while another tab is active"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'inactive_tab_source_after=codex' >/dev/null || {
  print -u2 "expected kitty title watcher to keep unread source while another tab is active"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'active_tab_title_after=repo | shell' >/dev/null || {
  print -u2 "expected kitty title watcher to clear unread state when the unread tab becomes active"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'active_tab_source_after=' >/dev/null || {
  print -u2 "expected kitty title watcher to clear unread source when the unread tab becomes active"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'active_tab_user_var=None' >/dev/null || {
  print -u2 "expected kitty title watcher to clear the unread user var when the unread tab becomes active"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'active_tab_close=i=agent-notify-7-codex:p=close;' >/dev/null || {
  print -u2 "expected kitty title watcher to close the matching system notification when the unread tab becomes active"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'same_tab_before_dirty=◆ repo | build' >/dev/null || {
  print -u2 "expected kitty title watcher to show unread state before a same-tab dirty event"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'same_tab_after_dirty=◆ repo | build' >/dev/null || {
  print -u2 "expected kitty title watcher to preserve unread state when the active tab has not changed"
  exit 1
}

print -r -- "$watcher_state" | rg -Fx 'same_tab_source_after=claude' >/dev/null || {
  print -u2 "expected kitty title watcher to keep unread source when the active tab has not changed"
  exit 1
}

config_state="$(
  kitty +runpy 'from kitty.config import load_config; import sys; opts = load_config(sys.argv[1]); print(f"allow_remote_control={opts.allow_remote_control}"); print(f"shell={opts.shell}")' "$kitty_conf"
)"

[[ "$config_state" == *'allow_remote_control=no'* ]] || {
  print -u2 "expected kitty remote control to stay disabled"
  exit 1
}

[[ "$config_state" == *'shell=/etc/profiles/per-user/kevin/bin/nu -l'* ]] || {
  print -u2 "expected kitty to launch nushell as a login shell"
  exit 1
}

rg -Fx 'map cmd+t new_tab_with_cwd' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty cmd+t to open a new tab in the current working directory"
  exit 1
}

rg -Fx 'map ctrl+shift+t new_tab_with_cwd' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty ctrl+shift+t to open a new tab in the current working directory"
  exit 1
}

if rg -n '^map .+ new_tab($| )' "$kitty_conf" >/dev/null; then
  print -u2 "expected kitty config to avoid bare new_tab mappings that lose the active cwd"
  exit 1
fi

invalid_tab_launches="$(
  rg -n 'launch .*--type=tab|launch --type=tab' "$kitty_conf" | rg -v -- '--cwd=current' || true
)"
if [[ -n "$invalid_tab_launches" ]]; then
  print -u2 "expected kitty tab launches to include --cwd=current"
  exit 1
fi

rg -F 'map cmd+, launch --type=background /bin/zsh -lc' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty config edit shortcut to use a background zsh launcher"
  exit 1
}

rg -F '~/.config/kitty/kitty.conf' "$kitty_conf" >/dev/null || {
  print -u2 "expected kitty config edit shortcut to target the home-relative config path"
  exit 1
}

if rg -F '/opt/homebrew/bin/emacsclient' "$kitty_conf" >/dev/null; then
  print -u2 "expected kitty config to avoid Homebrew-specific editor paths"
  exit 1
fi

if rg -F '/Users/kevin/projects/personal/dotfiles' "$kitty_conf" >/dev/null; then
  print -u2 "expected kitty config to avoid repository-specific absolute paths"
  exit 1
fi
