#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
script="$repo_root/bin/bin/sk"
tmp_dir="$(mktemp -d /tmp/sk-wrapper-regression.XXXXXX)"
mock_npx="$tmp_dir/npx"
args_file="$tmp_dir/args"

cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cat >"$mock_npx" <<'EOF'
#!/bin/sh

printf '%s\n' "$@" >"$SK_WRAPPER_ARGS_FILE"
exit "${SK_WRAPPER_EXIT_CODE:-0}"
EOF
chmod +x "$mock_npx"

[[ -x "$script" ]] || {
  print -u2 "expected sk wrapper to be executable at $script"
  exit 1
}

set +e
PATH="$tmp_dir:$PATH" \
  SK_WRAPPER_ARGS_FILE="$args_file" \
  SK_WRAPPER_EXIT_CODE=23 \
  "$script" add vercel-labs/agent-skills --list >/dev/null 2>&1
exit_code=$?
set -e

[[ $exit_code -eq 23 ]] || {
  print -u2 "expected sk wrapper to preserve npx exit status, got: $exit_code"
  exit 1
}

expected_args=$'skills\nadd\nvercel-labs/agent-skills\n--list'
actual_args="$(<"$args_file")"

[[ "$actual_args" == "$expected_args" ]] || {
  print -u2 "expected sk wrapper to invoke npx with skills subcommand and forwarded args"
  print -u2 "expected:"
  print -u2 "$expected_args"
  print -u2 "actual:"
  print -u2 "$actual_args"
  exit 1
}
