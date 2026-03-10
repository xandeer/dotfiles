from pathlib import Path
from typing import Any

from kitty.boss import Boss
from kitty.window import Window


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


def compose_window_title(path: str | None, title: str) -> str:
    label = repo_name_for_path(path) or directory_name_for_path(path)
    current_title = title.strip()

    if not label:
        return current_title
    if not current_title or current_title == label:
        return label

    prefix = f"{label} | "
    if current_title.startswith(prefix):
        return current_title
    return f"{label} | {current_title}"


def update_window_title(window: Window, raw_title: str, from_child: bool) -> None:
    if not from_child:
        return

    cwd = window.cwd_of_child or window.get_cwd_of_root_child()
    composed_title = compose_window_title(cwd, raw_title)
    if composed_title and composed_title != window.title:
        window.set_window_title(composed_title)


def on_title_change(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    del boss
    update_window_title(
        window,
        str(data.get("title") or ""),
        bool(data.get("from_child")),
    )
