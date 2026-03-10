from pathlib import Path
from typing import Any

from kitty.boss import Boss
from kitty.window import Window

AGENT_NOTIFY_VAR = "agent_notify"
AGENT_SOURCES = {"codex", "claude"}
LAST_CHILD_TITLE_BY_WINDOW_ID: dict[int, str] = {}
NOTIFICATION_SOURCE_BY_WINDOW_ID: dict[int, str] = {}


def repo_name_for_path(path: str | None) -> str | None:
    if not path:
        return None

    current = Path(path).expanduser()
    if current.is_file():
        current = current.parent

    for candidate in (current, *current.parents):
        if candidate.joinpath(".git").exists():
            return candidate.name or str(candidate)

    return None


def directory_name_for_path(path: str | None) -> str:
    if not path:
        return ""

    current = Path(path).expanduser()
    if current.is_file():
        current = current.parent

    return current.name or str(current)


def notification_source_for_value(value: object) -> str:
    if isinstance(value, bytes):
        try:
            value = value.decode("utf-8")
        except UnicodeDecodeError:
            return ""

    source = str(value or "").strip().lower()
    if source in AGENT_SOURCES:
        return source
    return ""


def notification_id_for_window(window_id: int, source: str) -> str:
    return f"agent-notify-{window_id}-{source}"


def strip_notification_marker(title: str) -> str:
    stripped_title = title.strip()

    for source in AGENT_SOURCES:
        prefix = f"[{source}] "
        if stripped_title.startswith(prefix):
            return stripped_title[len(prefix) :]

    return stripped_title


def child_title_from_display_title(path: str | None, title: str) -> str:
    current_title = title.strip()
    label = repo_name_for_path(path) or directory_name_for_path(path)

    if label:
        prefix = f"{label} | "
        if current_title.startswith(prefix):
            current_title = current_title[len(prefix) :]

    return strip_notification_marker(current_title)


def compose_window_title(
    path: str | None, title: str, notification_source: str | None = None
) -> str:
    label = repo_name_for_path(path) or directory_name_for_path(path)
    current_title = child_title_from_display_title(path, title)
    source = notification_source_for_value(notification_source)
    marker = f"[{source}] " if source else ""

    if not label:
        return f"{marker}{current_title}".strip()
    if not current_title:
        return f"{label} | {marker}".strip()
    if current_title == label:
        if source:
            return f"{label} | [{source}]"
        return label

    return f"{label} | {marker}{current_title}"


def window_id(window: Window) -> int:
    return int(getattr(window, "id", 0))


def notification_source_for_window(window: Window) -> str:
    return NOTIFICATION_SOURCE_BY_WINDOW_ID.get(window_id(window), "")


def store_notification_source(window: Window, value: object) -> str:
    source = notification_source_for_value(value)
    current_window_id = window_id(window)

    if source:
        NOTIFICATION_SOURCE_BY_WINDOW_ID[current_window_id] = source
    else:
        NOTIFICATION_SOURCE_BY_WINDOW_ID.pop(current_window_id, None)

    return source


def remember_child_title(window: Window, title: str) -> None:
    LAST_CHILD_TITLE_BY_WINDOW_ID[window_id(window)] = title.strip()


def base_title_for_window(window: Window) -> str:
    current_window_id = window_id(window)
    cached_title = LAST_CHILD_TITLE_BY_WINDOW_ID.get(current_window_id)

    if cached_title:
        return cached_title

    cwd = window.cwd_of_child or window.get_cwd_of_root_child()
    return child_title_from_display_title(cwd, str(window.title))


def refresh_window_title(window: Window) -> None:
    cwd = window.cwd_of_child or window.get_cwd_of_root_child()
    composed_title = compose_window_title(
        cwd,
        base_title_for_window(window),
        notification_source_for_window(window),
    )
    if composed_title and composed_title != window.title:
        window.set_window_title(composed_title)


def close_notification_for_window(boss: Boss, window: Window, source: str) -> None:
    if not source:
        return

    notification_manager = getattr(boss, "notification_manager", None)
    if notification_manager is None:
        return

    try:
        notification_manager.handle_notification_cmd(
            window_id(window),
            99,
            f"i={notification_id_for_window(window_id(window), source)}:p=close;",
        )
    except Exception:
        return


def update_window_title(window: Window, raw_title: str, from_child: bool) -> None:
    if not from_child:
        return

    remember_child_title(window, raw_title)
    refresh_window_title(window)


def on_title_change(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    del boss
    update_window_title(
        window,
        str(data.get("title") or ""),
        bool(data.get("from_child")),
    )


def on_set_user_var(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    del boss

    if data.get("key") != AGENT_NOTIFY_VAR:
        return

    store_notification_source(window, data.get("value"))
    refresh_window_title(window)


def on_focus_change(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    if not bool(data.get("focused")):
        return

    source = notification_source_for_window(window)
    if not source:
        return

    close_notification_for_window(boss, window, source)
    window.set_user_var(AGENT_NOTIFY_VAR, None)
    store_notification_source(window, None)
    refresh_window_title(window)
