UNREAD_MARKER = "◆"
UNREAD_PREFIX = f"{UNREAD_MARKER} "
LEGACY_UNREAD_PREFIXES = ("[codex] ", "[claude] ")


def strip_unread_prefix(title: str) -> tuple[str, bool]:
    if title.startswith(UNREAD_PREFIX):
        return title[len(UNREAD_PREFIX) :], True

    for prefix in LEGACY_UNREAD_PREFIXES:
        if title.startswith(prefix):
            return title[len(prefix) :], True

    return title, False


def draw_title(data: dict[object, object]) -> str:
    title = str(data.get("title") or "")
    title, unread = strip_unread_prefix(title)

    fmt = data["fmt"]
    index = data["index"]
    tab = data.get("tab")
    progress = str(getattr(tab, "last_focused_progress_percent", "") or "")
    unread_prefix = (
        f"{fmt.fg._FFB000}{UNREAD_PREFIX}{fmt.fg.default}" if unread else ""
    )
    return f"{unread_prefix}{index}: {progress}{title}"
