from pathlib import Path
from typing import Any

from kitty.boss import Boss
from kitty.window import Window

AGENT_NOTIFY_VAR = "agent_notify"
AGENT_SOURCES = {"codex", "claude"}
UNREAD_MARKER = "◆"
DEBUG_LOG_PATH = Path("/tmp/kitty-title-watcher.log")
LAST_CHILD_TITLE_BY_WINDOW_ID: dict[int, str] = {}
NOTIFICATION_SOURCE_BY_WINDOW_ID: dict[int, str] = {}
LAST_ACTIVE_TAB_ID_BY_TAB_MANAGER_ID: dict[int, int] = {}


def debug_log(message: str) -> None:
    try:
        DEBUG_LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
        with DEBUG_LOG_PATH.open("a", encoding="utf-8") as handle:
            handle.write(f"{message}\n")
    except Exception:
        return


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

    prefixes = [f"{UNREAD_MARKER} "]
    prefixes.extend(f"[{source}] " for source in AGENT_SOURCES)

    for prefix in prefixes:
        if stripped_title.startswith(prefix):
            return stripped_title[len(prefix) :]

    return stripped_title


def strip_prompt_path_prefix(title: str) -> str:
    prompt_prefix, separator, command = title.partition(">")
    normalized_prefix = prompt_prefix.strip()
    path_like_prefixes = ("~/", "./", "../", "/")

    if not separator:
        return title
    if not normalized_prefix:
        return title
    if not (
        "/" in normalized_prefix
        or normalized_prefix in {"~", ".", "..", "/"}
        or normalized_prefix.startswith(path_like_prefixes)
    ):
        return title

    stripped_command = command.strip()
    return stripped_command or title


def child_title_from_display_title(path: str | None, title: str) -> str:
    current_title = strip_notification_marker(title.strip())
    label = repo_name_for_path(path) or directory_name_for_path(path)

    if label:
        prefix = f"{label} | "
        if current_title.startswith(prefix):
            current_title = current_title[len(prefix) :]

    return strip_prompt_path_prefix(strip_notification_marker(current_title))


def compose_window_title(
    path: str | None, title: str, notification_source: str | None = None
) -> str:
    label = repo_name_for_path(path) or directory_name_for_path(path)
    current_title = child_title_from_display_title(path, title)
    source = notification_source_for_value(notification_source)
    marker = f"{UNREAD_MARKER} " if source else ""

    if not label:
        body = current_title
    elif not current_title or current_title == label:
        body = label
    else:
        body = f"{label} | {current_title}"

    return f"{marker}{body}".strip()


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
        debug_log(
            f"close skip window={window_id(window)} source={source} reason=no-notification-manager"
        )
        return

    client_id = notification_id_for_window(window_id(window), source)
    tracked_notifications = getattr(
        notification_manager, "in_progress_notification_commands_by_client_id", None
    )
    tracked_notification = None
    if tracked_notifications is not None:
        tracked_notification = tracked_notifications.get(client_id)
    debug_log(
        "close begin "
        f"window={window_id(window)} source={source} client_id={client_id} "
        f"tracked={tracked_notification is not None}"
    )

    if tracked_notification is not None:
        desktop_integration = getattr(notification_manager, "desktop_integration", None)
        close_notification = getattr(desktop_integration, "close_notification", None)
        desktop_notification_id = getattr(
            tracked_notification, "desktop_notification_id", None
        )
        if callable(close_notification) and desktop_notification_id is not None:
            try:
                close_succeeded = bool(close_notification(desktop_notification_id))
            except Exception:
                close_succeeded = False

            supports_close_events = bool(
                getattr(desktop_integration, "supports_close_events", False)
            )
            debug_log(
                "close tracked "
                f"window={window_id(window)} client_id={client_id} "
                f"desktop_id={desktop_notification_id} "
                f"supports_close_events={supports_close_events} "
                f"close_succeeded={close_succeeded}"
            )
            if close_succeeded and not supports_close_events:
                purge_notification = getattr(notification_manager, "purge_notification", None)
                if callable(purge_notification):
                    try:
                        purge_notification(tracked_notification)
                        debug_log(
                            f"close purge window={window_id(window)} client_id={client_id}"
                        )
                    except Exception:
                        pass
            return

    try:
        notification_manager.handle_notification_cmd(
            window_id(window),
            99,
            f"i={client_id}:p=close;",
        )
        debug_log(
            f"close fallback window={window_id(window)} source={source} client_id={client_id}"
        )
    except Exception:
        debug_log(
            f"close fallback-error window={window_id(window)} source={source} client_id={client_id}"
        )
        return


def clear_unread_state_for_window(boss: Boss, window: Window) -> None:
    source = notification_source_for_window(window)
    if not source:
        return

    close_notification_for_window(boss, window, source)
    window.set_user_var(AGENT_NOTIFY_VAR, None)
    store_notification_source(window, None)
    refresh_window_title(window)


def resolve_window_reference(boss: Boss, candidate: object) -> Window | None:
    if hasattr(candidate, "set_window_title") and hasattr(candidate, "get_cwd_of_root_child"):
        return candidate  # type: ignore[return-value]

    candidate_id = None
    if isinstance(candidate, dict):
        candidate_id = candidate.get("id")
    else:
        candidate_id = getattr(candidate, "id", None)

    if candidate_id is None:
        return None

    try:
        normalized_candidate_id = int(candidate_id)
    except (TypeError, ValueError):
        return None

    for window in list(getattr(boss, "all_windows", []) or []):
        if window_id(window) == normalized_candidate_id:
            return window

    return None


def windows_in_active_tab(boss: Boss, tab_manager: object) -> list[Window]:
    active_tab = getattr(tab_manager, "active_tab", None)
    if active_tab is None:
        return []

    try:
        active_windows = list(active_tab.list_windows())
    except Exception:
        return []

    resolved_windows: list[Window] = []
    for active_window in active_windows:
        resolved_window = resolve_window_reference(boss, active_window)
        if resolved_window is not None:
            resolved_windows.append(resolved_window)

    return resolved_windows


def record_active_tab_change(tab_manager: object) -> bool:
    active_tab = getattr(tab_manager, "active_tab", None)
    if active_tab is None:
        return False

    tab_manager_id = id(tab_manager)
    active_tab_id = id(active_tab)
    previous_active_tab_id = LAST_ACTIVE_TAB_ID_BY_TAB_MANAGER_ID.get(tab_manager_id)
    LAST_ACTIVE_TAB_ID_BY_TAB_MANAGER_ID[tab_manager_id] = active_tab_id

    if previous_active_tab_id is None:
        return False

    return previous_active_tab_id != active_tab_id


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

    source = store_notification_source(window, data.get("value"))
    debug_log(
        f"set_user_var window={window_id(window)} value={data.get('value')} source={source}"
    )
    refresh_window_title(window)


def on_focus_change(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    if not bool(data.get("focused")):
        return

    debug_log(f"focus window={window_id(window)}")
    clear_unread_state_for_window(boss, window)


def on_tab_bar_dirty(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    del window

    if not NOTIFICATION_SOURCE_BY_WINDOW_ID:
        return

    tab_manager = data.get("tab_manager")
    if not record_active_tab_change(tab_manager):
        return

    for active_window in windows_in_active_tab(boss, tab_manager):
        debug_log(f"tab-active window={window_id(active_window)}")
        clear_unread_state_for_window(boss, active_window)
