#!/bin/zsh

set -eu

repo_root=${0:A:h:h:h}
patch_file="$repo_root/emacs.d/.emacs.d/patches/emacs-rime/0001-add-ai-session-bridge.patch"
upstream=${EMACS_RIME_SOURCE:-$HOME/projects/personal/dotfiles/emacs.d/.emacs.d/straight/repos/emacs-rime}
pinned=3eeef9c445fa056a4b32137f9ef72c27ced2d4ab
librime_root=${LIBRIME_ROOT:-$HOME/syncthing/personal/configs/librime/}
module_header_root=${EMACS_MODULE_HEADER_ROOT:-/opt/homebrew/opt/emacs-plus@30/include}
emacs=${EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
shared_data_dir=${RIME_SHARED_DATA_DIR:-$tmp/shared}

[[ -f $patch_file ]] || { print -u2 "missing patch: $patch_file"; exit 1; }
[[ -d $upstream/.git ]] || { print -u2 "missing emacs-rime checkout: $upstream"; exit 1; }

before_head=$(git -C "$upstream" rev-parse HEAD)
before_status=$(git -C "$upstream" status --porcelain)
before_hash=$(shasum "$upstream/lib.c" "$upstream/Makefile")

mkdir -p "$tmp/source" "$tmp/user" "$tmp/shared"
git -C "$upstream" show "${pinned}:lib.c" > "$tmp/source/lib.c"
git -C "$upstream" show "${pinned}:Makefile" > "$tmp/source/Makefile"

(
  cd "$tmp/source"
  git apply --check "$patch_file"
  git apply "$patch_file"
)

rg -U -q 'rime->api = rime_get_api\(\);\n  rime->session_id = 0;' "$tmp/source/lib.c" || {
  print -u2 "patched module must initialize the guarded session id"
  exit 1
}

added_bindings=(${(f)"$(sed -n '/^+.*emacs_defun.*"rime-lib-/s/.*"\(rime-lib-[^"]*\)".*/\1/p' "$patch_file")"})
expected_bindings=(
  rime-lib-set-property
  rime-lib-get-current-schema
  rime-lib-user-config-get-string
  rime-lib-user-config-get-bool
)
[[ ${#added_bindings} -eq 4 && "${(j: :)${(on)added_bindings}}" == "${(j: :)${(on)expected_bindings}}" ]] || {
  print -u2 "patch must register exactly the four AI bridge bindings"
  exit 1
}

make -C "$tmp/source" lib \
  MODULE_FILE_SUFFIX=.dylib \
  LIBRIME_ROOT="${librime_root%/}/" \
  EMACS_MODULE_HEADER_ROOT="$module_header_root"

cat > "$tmp/user/squirrel.custom.yaml" <<'YAML'
patch:
  ai:
    enabled: true
    endpoint: "https://example.invalid/v1/chat/completions"
    model: "test-model"
    instructions: "test"
YAML

cat > "$tmp/shared/default.yaml" <<'YAML'
config_version: "1"
schema_list:
  - schema: test
YAML

cat > "$tmp/shared/test.schema.yaml" <<'YAML'
schema:
  schema_id: test
  name: Test
  version: "1"
engine:
  processors:
    - selector
    - express_editor
  segmentors:
    - fallback_segmentor
  translators:
    - echo_translator
YAML

GLOG_minloglevel=2 MallocPreScribble=1 "$emacs" --batch -Q --eval "
(progn
  (module-load \"$tmp/source/librime-emacs.dylib\")
  (dolist (function '(rime-lib-set-property
                       rime-lib-get-current-schema
                       rime-lib-user-config-get-string
                       rime-lib-user-config-get-bool))
    (unless (fboundp function)
      (error \"missing binding: %S\" function)))
  (when (rime-lib-get-current-schema)
    (error \"schema read succeeded without a session\"))
  (when (rime-lib-set-property \"_ai_candidate\" \"测试\")
    (error \"property publication succeeded without a session\"))
  (unwind-protect
      (progn
        (rime-lib-start \"$shared_data_dir\" \"$tmp/user\")
        (unless (equal (rime-lib-user-config-get-string
                        \"squirrel.custom\" \"patch/ai/endpoint\")
                       \"https://example.invalid/v1/chat/completions\")
          (error \"nested user config string was not read\"))
        (unless (eq (rime-lib-user-config-get-bool
                     \"squirrel.custom\" \"patch/ai/enabled\" nil)
                    t)
          (error \"nested user config bool was not read\"))
        (unless (eq (rime-lib-user-config-get-bool
                     \"squirrel.custom\" \"patch/ai/missing\" nil)
                    nil)
          (error \"missing bool did not preserve nil default\"))
        (unless (eq (rime-lib-user-config-get-bool
                     \"squirrel.custom\" \"patch/ai/missing\" t)
                    t)
          (error \"missing bool did not preserve t default\"))
        (let ((schema (rime-lib-get-current-schema)))
          (unless (and (stringp schema) (> (length schema) 0))
            (error \"current schema is not a non-empty string: %S\" schema)))
        (unless (rime-lib-set-property \"_ai_candidate\" \"测试\")
          (error \"property publication failed\")))
    (ignore-errors (rime-lib-finalize))))
"

(
  cd "$upstream"
  git apply --check "$patch_file"
)

[[ $(git -C "$upstream" rev-parse HEAD) == $before_head ]]
[[ $(git -C "$upstream" status --porcelain) == $before_status ]]
[[ $(shasum "$upstream/lib.c" "$upstream/Makefile") == $before_hash ]]

print "Emacs Rime module regression OK"
