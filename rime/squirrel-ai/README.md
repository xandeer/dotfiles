# Squirrel AI candidates (macOS, Squirrel 1.1.2)

This patch set supports only macOS with Squirrel 1.1.2. When both `ai/endpoint` and `ai/model` are configured, every app with Squirrel enabled can send AI requests.

Each request can send the configured model plus the current Rime schema and raw input, at most eight candidates, the last five committed strings, and up to 128 UTF-16 units before and after the marked text. Secure Event Input suppresses the request and clears commit history. Custom password fields that do not enable macOS's public Secure Event Input signal cannot be detected.

The API key is read from the login Keychain and used only as the `Authorization` header. Never put it in YAML or logs.

## Build the pinned source

```zsh
repo_root="/absolute/path/to/dotfiles"
squirrel_root="$(mktemp -d /private/tmp/squirrel-src.XXXXXX)"
squirrel_checkout="$squirrel_root/squirrel"

git clone --recurse-submodules --branch 1.1.2 --single-branch \
  https://github.com/rime/squirrel.git "$squirrel_checkout"
test "$(git -C "$squirrel_checkout" rev-parse --short HEAD)" = 876adeb
git -C "$squirrel_checkout" am \
  "$repo_root"/rime/squirrel-ai/patches/*.patch

cd "$squirrel_checkout"
bash ./action-install.sh
make debug
test -x "$squirrel_checkout/build/Build/Products/Debug/Squirrel.app/Contents/MacOS/Squirrel"
```

Never run `make install-debug`: its upstream `permission-check` can recursively `chown` all of `/Library/Input Methods`.

## Install only after an explicit checkpoint

Stop here until replacing the installed input method is explicitly approved. Then validate both exact bundles, record the printed backup path, move only the current `Squirrel.app`, copy the built bundle, and run the upstream postinstall script:

```zsh
set -eu
built_app="$squirrel_checkout/build/Build/Products/Debug/Squirrel.app"
installed_app="/Library/Input Methods/Squirrel.app"
test -d "$built_app/Contents"
test -x "$built_app/Contents/MacOS/Squirrel"
test -d "$installed_app/Contents"
test -x "$installed_app/Contents/MacOS/Squirrel"

backup_parent="$HOME/Library/Application Support/Squirrel AI Backups"
mkdir -p "$backup_parent"
chmod 700 "$backup_parent"
backup_root="$(mktemp -d "$backup_parent/squirrel-backup.XXXXXX")"
chmod 700 "$backup_root"
backup_app="$backup_root/Squirrel.app"
print -r -- "Backup: $backup_app"
sudo mv "$installed_app" "$backup_app"
test -x "$backup_app/Contents/MacOS/Squirrel"
sudo ditto "$built_app" "$installed_app"
test -x "$installed_app/Contents/MacOS/Squirrel"
sudo chown -R root:wheel "$installed_app"
(
  cd "$squirrel_checkout"
  DSTROOT="/Library/Input Methods" RIME_NO_PREBUILD=1 /bin/bash scripts/postinstall
)
```

If `ditto`, the targeted ownership repair, or postinstall fails, preserve the failed bundle for diagnosis and restore the exact recorded persistent backup; do not broadly delete or change ownership:

```zsh
set -eu
installed_app="/Library/Input Methods/Squirrel.app"
backup_app="$HOME/Library/Application Support/Squirrel AI Backups/squirrel-backup.RECORDED/Squirrel.app"
test -x "$backup_app/Contents/MacOS/Squirrel"
failed_root="$(mktemp -d /private/tmp/squirrel-failed.XXXXXX)"
if [[ -e "$installed_app" ]]; then
  sudo mv "$installed_app" "$failed_root/Squirrel.app"
fi
sudo mv "$backup_app" "$installed_app"
restored_executable="$installed_app/Contents/MacOS/Squirrel"
test -x "$restored_executable"
console_user="$(/usr/bin/stat -f%Su /dev/console)"
/usr/bin/sudo -u "$console_user" /usr/bin/killall Squirrel >/dev/null 2>&1 || true
"$restored_executable" --register-input-source
/usr/bin/sudo -u "$console_user" "$restored_executable" --enable-input-source
/usr/bin/sudo -u "$console_user" "$restored_executable" --select-input-source
```

## Configure Rime and Keychain

Set the complete HTTPS Chat Completions URL and non-secret model name in `$repo_root/rime/darwin/squirrel.custom.yaml`:

```yaml
patch:
  ai/enabled: true
  ai/endpoint: "https://YOUR-ENDPOINT.example/v1/chat/completions"
  ai/model: "YOUR-MODEL"
  ai/instructions: |-
    优先纠正输入错误，并选择最符合前后文的简体中文候选。
    除非上下文明确表明有误，否则原样保留人名、专有名词和技术术语。
    只纠正错误，不扩写、不润色；无法确定时，保留最贴近原输入的结果。
```

`ai/enabled` is the global switch for every app using Squirrel. Set it to `false` to prevent new AI requests after reload; this does not retract a request that was already sent. `ai/instructions` is optional runtime guidance inserted before the mandatory built-in protocol postamble. Leave it as `""` for the default behavior. `ai/instructions` cannot override the mandatory request/response protocol.

Changing `ai/enabled` or `ai/instructions` requires only `--reload`; it does not require rebuilding Squirrel. Endpoint and model changes use the same reload path. Changing the mandatory Swift protocol behavior still requires regenerating the patches and rebuilding Squirrel. Configuration changes take effect on reload, not through a promised live file watcher.

From that same dotfiles checkout, install the Rime files and ask Squirrel to reload/deploy them. Do not use the top-level `make rime` target from a worktree.

```zsh
make -C "$repo_root/rime" install
"/Library/Input Methods/Squirrel.app/Contents/MacOS/Squirrel" --reload
```

Only after the custom app is installed, write the key interactively. The final `-w` prompts for it, so never pass the key through chat, a command argument, an environment variable, or a file. Repeat this after installing a differently signed or rebuilt app.

```zsh
/usr/bin/security add-generic-password \
  -U \
  -a default \
  -s im.rime.inputmethod.Squirrel.ai \
  -T "/Library/Input Methods/Squirrel.app" \
  -w
```

Delete only this key with:

```zsh
/usr/bin/security delete-generic-password \
  -a default \
  -s im.rime.inputmethod.Squirrel.ai
```

Learned AI choices are kept in `~/Library/Rime/ai_weights.tsv`; it is runtime data and is not tracked. Existing storage is tightened to mode `0600` when the learned translator starts, and new atomic files are created at mode `0600`. If private storage cannot be secured or written, learning fails closed for that translator instance without disabling live AI candidate ordering. Deleting the file resets only AI learning.

## Roll back or upgrade

For rollback, move the current exact `/Library/Input Methods/Squirrel.app` aside, move the recorded persistent backup `Squirrel.app` back to that exact path, then use its executable to stop the old process and run `--register-input-source`, `--enable-input-source`, and `--select-input-source` as shown above. Do not broadly delete or `chown` anything under `/Library/Input Methods`.

For a Squirrel upgrade, start from a clean checkout of the new tag, apply these two patches with `git am`, resolve and review any conflicts, rerun the core and bridge regressions, and rebuild. Never assume the 1.1.2 patches are compatible with another release.
