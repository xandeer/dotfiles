#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
script="$repo_root/bin/bin/agent-notify-kitty"

codex_payload='{"type":"agent-turn-complete","last-assistant-message":"Codex finished with a diff."}'
claude_payload='{"hook_event_name":"Stop","last_assistant_message":"Claude finished the response."}'

codex_title="$(printf '%s' 'Codex result' | base64 | tr -d '\n')"
claude_title="$(printf '%s' 'Claude Code result' | base64 | tr -d '\n')"

codex_output="$(KITTY_WINDOW_ID=1 "$script" codex "$codex_payload")"
claude_output="$(printf '%s' "$claude_payload" | KITTY_WINDOW_ID=1 "$script" claude)"

[[ "$codex_output" == *$'\e]99;'* ]] || {
  print -u2 "expected Codex output to emit OSC 99"
  exit 1
}

[[ "$codex_output" == *'o=unfocused'* ]] || {
  print -u2 "expected Codex output to notify only when kitty is unfocused"
  exit 1
}

[[ "$codex_output" == *"$codex_title"* ]] || {
  print -u2 "expected Codex output to encode the Codex title"
  exit 1
}

[[ "$codex_output" != *']99;i='* ]] || {
  print -u2 "expected Codex output to avoid a fixed notification id"
  exit 1
}

[[ "$claude_output" == *$'\e]99;'* ]] || {
  print -u2 "expected Claude output to emit OSC 99"
  exit 1
}

[[ "$claude_output" == *'o=unfocused'* ]] || {
  print -u2 "expected Claude output to notify only when kitty is unfocused"
  exit 1
}

[[ "$claude_output" == *"$claude_title"* ]] || {
  print -u2 "expected Claude output to encode the Claude title"
  exit 1
}

[[ "$claude_output" != *']99;i='* ]] || {
  print -u2 "expected Claude output to avoid a fixed notification id"
  exit 1
}
