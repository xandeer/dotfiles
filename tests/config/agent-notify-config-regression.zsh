#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
codex_config="$repo_root/config/.codex/config.toml"
claude_config="$repo_root/config/.claude/settings.json"
script_path="/Users/kevin/bin/agent-notify-kitty"

[[ -f "$codex_config" ]] || {
  print -u2 "expected repository-managed Codex config at $codex_config"
  exit 1
}

rg -F "notify = [\"$script_path\", \"codex\"]" "$codex_config" >/dev/null || {
  print -u2 "expected Codex config to wire notify to $script_path"
  exit 1
}

rg -F "\"Stop\"" "$claude_config" >/dev/null || {
  print -u2 "expected Claude settings to define a Stop hook"
  exit 1
}

rg -F "\"command\": \"$script_path claude\"" "$claude_config" >/dev/null || {
  print -u2 "expected Claude Stop hook to call $script_path claude"
  exit 1
}
