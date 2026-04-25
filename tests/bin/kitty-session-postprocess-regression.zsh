#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
script="$repo_root/config/.config/kitty/kitty-session-postprocess"

temp_root="$(mktemp -d)"
trap 'rm -rf "$temp_root"' EXIT

fake_bin="$temp_root/fake-bin"
session_file="$temp_root/startup.kitty-session"
fake_codex="$fake_bin/codex"
fake_claude="$fake_bin/claude"

mkdir -p "$fake_bin"

cat > "$fake_codex" <<'EOF'
#!/bin/zsh
exit 0
EOF
chmod +x "$fake_codex"

cat > "$fake_claude" <<'EOF'
#!/bin/zsh
exit 0
EOF
chmod +x "$fake_claude"

cat > "$session_file" <<EOF
cd /Users/kevin/projects/personal/remio
launch /bin/zsh -lc 'cd /Users/kevin/projects/personal/remio && exec codex resume --last'
cd /Users/kevin/projects/personal/tools
launch 'kitty-unserialize-data={"id": 9}' --var=agent_notify=codex '--title=◆ kevin | dotfiles | codex' /opt/homebrew/bin/codex resume --last
cd /Users/kevin
launch 'kitty-unserialize-data={"id": 1, "cmd_at_shell_startup": ["$fake_codex", "-C", "/Users/kevin/projects/personal/dotfiles"]}' '--title=dotfiles | codex'
cd /Users/kevin/projects/personal/remio
launch 'kitty-unserialize-data={"id": 2}' '--title=remio | ✳ Claude Code' /opt/homebrew/bin/claude -c
cd /Users/kevin
launch 'kitty-unserialize-data={"id": 3}' '--title=kevin | nu in kevin'
EOF

PATH="$fake_bin:$PATH" "$script" "$session_file"

rg -Fx 'cd /Users/kevin/projects/personal/dotfiles' "$session_file" >/dev/null || {
  print -u2 "expected kitty session postprocess to rewrite the saved cd line to the codex workspace when codex startup args include an explicit cwd"
  exit 1
}

rg -Fx 'launch codex resume --last' "$session_file" >/dev/null || {
  print -u2 "expected kitty session postprocess to rewrite codex windows into a short codex resume launch"
  exit 1
}

[[ "$(rg -Fc 'launch codex resume --last' "$session_file")" == "3" ]] || {
  print -u2 "expected kitty session postprocess to normalize serialized, legacy shell-style, and legacy direct-path codex launches into the short codex resume form"
  exit 1
}

rg -Fx 'launch claude -c' "$session_file" >/dev/null || {
  print -u2 "expected kitty session postprocess to rewrite claude windows into a short claude -c launch"
  exit 1
}

rg -Fx "launch 'kitty-unserialize-data={\"id\": 3}' '--title=kevin | nu in kevin'" "$session_file" >/dev/null || {
  print -u2 "expected kitty session postprocess to leave non-agent windows unchanged"
  exit 1
}

if rg -F "$fake_codex" "$session_file" >/dev/null; then
  print -u2 "expected kitty session postprocess to remove serialized codex binary paths after rewriting the launch line"
  exit 1
fi

if rg -F "$fake_claude" "$session_file" >/dev/null; then
  print -u2 "expected kitty session postprocess to remove serialized claude binary paths after rewriting the launch line"
  exit 1
fi

if rg -F "launch /bin/zsh -lc 'cd /Users/kevin/projects/personal/remio && exec codex resume --last'" "$session_file" >/dev/null; then
  print -u2 "expected kitty session postprocess to remove legacy shell-style codex launch lines after normalizing them to the direct codex resume form"
  exit 1
fi

if rg -F '/opt/homebrew/bin/codex resume --last' "$session_file" >/dev/null; then
  print -u2 "expected kitty session postprocess to remove legacy direct-path codex launch lines, even when kitty saves extra launch options on the same line"
  exit 1
fi

if rg -F '/opt/homebrew/bin/claude -c' "$session_file" >/dev/null; then
  print -u2 "expected kitty session postprocess to remove legacy direct-path claude launch lines, even when kitty saves extra launch options on the same line"
  exit 1
fi
