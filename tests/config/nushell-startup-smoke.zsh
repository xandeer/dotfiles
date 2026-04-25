#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
original_home="${HOME:A}"
tmp_home="$(mktemp -d /tmp/nushell-startup-home.XXXXXX)"
nu_bin="$(command -v nu)"

zoxide_cache_dir="$tmp_home/.cache/zoxide"
oh_my_posh_cache_dir="$tmp_home/.cache/oh-my-posh"
zoxide_cache_file="$zoxide_cache_dir/init.nu"
oh_my_posh_cache_file="$oh_my_posh_cache_dir/init.nu"
expected_zoxide_cache="$tmp_home/expected-zoxide-init.nu"
expected_oh_my_posh_cache="$tmp_home/expected-oh-my-posh-init.nu"
startup_stdout="$tmp_home/startup.stdout"
path_output="$tmp_home/path.out"
startup_missing_fnm_path_stdout="$tmp_home/startup-missing-fnm-path.stdout"

cleanup() {
  rm -rf "$tmp_home"
}
trap cleanup EXIT

mkdir -p \
  "$tmp_home/.config" \
  "$tmp_home/projects/others" \
  "$tmp_home/bin" \
  "$zoxide_cache_dir" \
  "$oh_my_posh_cache_dir"

if [[ -f "$original_home/.config/env.nu" ]]; then
  ln -s "$original_home/.config/env.nu" "$tmp_home/.config/env.nu"
else
  print 'load-env {}' > "$tmp_home/.config/env.nu"
fi

[[ -d "$original_home/projects/others/nu_scripts" ]] || {
  print -u2 "expected nu_scripts at $original_home/projects/others/nu_scripts"
  exit 1
}

[[ -f "$original_home/.cache/zoxide/init.nu" ]] || {
  print -u2 "expected zoxide cache at $original_home/.cache/zoxide/init.nu"
  exit 1
}

[[ -f "$original_home/.cache/oh-my-posh/init.nu" ]] || {
  print -u2 "expected oh-my-posh cache at $original_home/.cache/oh-my-posh/init.nu"
  exit 1
}

ln -s "$original_home/projects/others/nu_scripts" "$tmp_home/projects/others/nu_scripts"

{
  print '# startup-smoke sentinel: zoxide'
  cat "$original_home/.cache/zoxide/init.nu"
} > "$expected_zoxide_cache"
cp "$expected_zoxide_cache" "$zoxide_cache_file"

{
  print '# startup-smoke sentinel: oh-my-posh'
  cat "$original_home/.cache/oh-my-posh/init.nu"
} > "$expected_oh_my_posh_cache"
cp "$expected_oh_my_posh_cache" "$oh_my_posh_cache_file"

HOME="$tmp_home" PATH="$tmp_home/bin:$tmp_home/bin:$PATH" "$nu_bin" \
  --config "$repo_root/config/.config/nushell/config.nu" \
  --env-config "$repo_root/config/.config/nushell/env.nu" \
  -c 'print "__PATH_BEGIN__"; $env.PATH | split row (char esep) | each {|entry| print $entry }' \
  > "$startup_stdout"

cmp -s "$expected_zoxide_cache" "$zoxide_cache_file" || {
  print -u2 "expected startup to preserve the existing zoxide cache file"
  exit 1
}

cmp -s "$expected_oh_my_posh_cache" "$oh_my_posh_cache_file" || {
  print -u2 "expected startup to preserve the existing oh-my-posh cache file"
  exit 1
}

if grep -Fq 'Adding FNM to path:' "$startup_stdout"; then
  print -u2 "expected startup stdout to omit the fnm path debug message"
  exit 1
fi

if grep -Fq 'Adding FNM vars to shell env:' "$startup_stdout"; then
  print -u2 "expected startup stdout to omit the fnm env debug message"
  exit 1
fi

awk '$0 == "__PATH_BEGIN__" { in_path = 1; next } in_path { print }' "$startup_stdout" > "$path_output"

[[ -s "$path_output" ]] || {
  print -u2 "expected startup output to contain PATH data after __PATH_BEGIN__"
  exit 1
}

[[ "$(sed -n '1p' "$path_output")" == "$tmp_home/bin" ]] || {
  print -u2 "expected the first PATH entry to remain $tmp_home/bin"
  exit 1
}

[[ "$(grep -Fxc "$tmp_home/bin" "$path_output")" -eq 1 ]] || {
  print -u2 "expected the duplicated $tmp_home/bin PATH entry to be preserved only once"
  exit 1
}

cat > "$tmp_home/bin/fnm" <<'EOF'
#!/bin/sh
printf '%s\n' 'export FNM_DIR=/tmp/fnm'
printf '%s\n' 'export FNM_LOGLEVEL=info'
EOF
chmod +x "$tmp_home/bin/fnm"

HOME="$tmp_home" PATH="$tmp_home/bin:$tmp_home/bin:$PATH" "$nu_bin" \
  --config "$repo_root/config/.config/nushell/config.nu" \
  --env-config "$repo_root/config/.config/nushell/env.nu" \
  -c 'version | get version' \
  > "$startup_missing_fnm_path_stdout"

if grep -Fq 'Adding FNM' "$startup_missing_fnm_path_stdout"; then
  print -u2 "expected fnm startup without PATH to remain quiet"
  exit 1
fi
