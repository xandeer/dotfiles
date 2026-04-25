#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
script="$repo_root/bin/bin/journal-capture"
tmp_dir="$(mktemp -d /tmp/journal-capture-regression.XXXXXX)"
mock_emacsclient="$tmp_dir/emacsclient"
args_file="$tmp_dir/args"

cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cat >"$mock_emacsclient" <<'EOF'
#!/bin/sh

printf '%s\n' "$@" >"$JOURNAL_CAPTURE_ARGS_FILE"
exit "${JOURNAL_CAPTURE_EXIT_CODE:-0}"
EOF
chmod +x "$mock_emacsclient"

[[ -x "$script" ]] || {
  print -u2 "expected journal-capture wrapper to be executable at $script"
  exit 1
}

set +e
PATH="$tmp_dir:$PATH" \
  JOURNAL_CAPTURE_ARGS_FILE="$args_file" \
  JOURNAL_CAPTURE_EXIT_CODE=17 \
  "$script" hello world >/dev/null 2>&1
exit_code=$?
set -e

[[ $exit_code -eq 17 ]] || {
  print -u2 "expected journal-capture wrapper to preserve emacsclient exit status, got: $exit_code"
  exit 1
}

expected_args=$'-n\n-e\n(x/journal-capture-string (base64-decode-string "aGVsbG8gd29ybGQ="))'
actual_args="$(<"$args_file")"

[[ "$actual_args" == "$expected_args" ]] || {
  print -u2 "expected argv mode to forward a base64-decoded hello world payload"
  print -u2 "expected:"
  print -u2 "$expected_args"
  print -u2 "actual:"
  print -u2 "$actual_args"
  exit 1
}

set +e
PATH="$tmp_dir:$PATH" \
  JOURNAL_CAPTURE_ARGS_FILE="$args_file" \
  JOURNAL_CAPTURE_EXIT_CODE=19 \
  /bin/sh -c "printf 'line1\nline2\n' | \"$script\"" >/dev/null 2>&1
exit_code=$?
set -e

[[ $exit_code -eq 19 ]] || {
  print -u2 "expected journal-capture wrapper to preserve emacsclient exit status for stdin mode, got: $exit_code"
  exit 1
}

expected_args=$'-n\n-e\n(x/journal-capture-string (base64-decode-string "bGluZTEKbGluZTIK"))'
actual_args="$(<"$args_file")"

[[ "$actual_args" == "$expected_args" ]] || {
  print -u2 "expected stdin mode to forward a base64-decoded multi-line payload"
  print -u2 "expected:"
  print -u2 "$expected_args"
  print -u2 "actual:"
  print -u2 "$actual_args"
  exit 1
}
