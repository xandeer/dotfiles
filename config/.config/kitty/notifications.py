from pathlib import Path
import sys

from kitty.notifications import Channel, NotificationCommand


CONFIG_DIR = Path(__file__).resolve().parent
if str(CONFIG_DIR) not in sys.path:
    sys.path.insert(0, str(CONFIG_DIR))

import title_watcher


APPROVAL_PREFIX = "Approval requested:"
CODEX_SOURCE = "codex"


def notification_source_for_title(title: object) -> str:
    normalized_title = str(title or "").strip()
    if normalized_title.startswith(APPROVAL_PREFIX):
        return CODEX_SOURCE
    return ""


def mark_window_unread(channel_id: int, source: str) -> bool:
    if not channel_id or not source:
        return False

    channel = Channel()
    ui_state = channel.ui_state(channel_id)
    if bool(getattr(ui_state, "has_focus", False)):
        return False

    window = channel.window_for_id(channel_id)
    if window is None or bool(getattr(window, "destroyed", False)):
        return False

    title_watcher.store_notification_source(window, source)
    window.set_user_var(title_watcher.AGENT_NOTIFY_VAR, source)
    title_watcher.refresh_window_title(window)
    return True


def main(notification_cmd: NotificationCommand) -> bool:
    source = notification_source_for_title(notification_cmd.title)
    if source:
        mark_window_unread(int(notification_cmd.channel_id or 0), source)
    return True
