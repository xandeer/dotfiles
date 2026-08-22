#!/bin/zsh

set -eu

repo_root=${0:A:h:h:h}
patch_file="$repo_root/emacs.d/.emacs.d/patches/emacs-rime/0001-add-ai-session-bridge.patch"
build_script="$repo_root/emacs.d/.emacs.d/libexec/build-emacs-rime-module.zsh"
x_rime="$repo_root/emacs.d/.emacs.d/lisp/x-rime.el"
x_rime_lisp_dir="$repo_root/emacs.d/.emacs.d/lisp"
squirrel_config="$repo_root/rime/darwin/squirrel.custom.yaml"
upstream=${EMACS_RIME_SOURCE:-$HOME/projects/personal/dotfiles/emacs.d/.emacs.d/straight/repos/emacs-rime}
pinned=3eeef9c445fa056a4b32137f9ef72c27ced2d4ab
librime_root=${LIBRIME_ROOT:-/Library/Input Methods/Squirrel.app/Contents/Frameworks}
librime_header_root=${LIBRIME_HEADER_ROOT:-$HOME/syncthing/personal/configs/librime/}
module_header_root=${EMACS_MODULE_HEADER_ROOT:-/opt/homebrew/opt/emacs-plus@30/include}
emacs=${EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
shared_data_dir=${RIME_SHARED_DATA_DIR:-$tmp/shared}
straight_build=${EMACS_RIME_BUILD_DIR:-$upstream:h:h/build/rime}
expected_lua_plugin=${RIME_LUA_PLUGIN:-${librime_root%/}/rime-plugins/librime-lua.dylib}

[[ -f $patch_file ]] || { print -u2 "missing patch: $patch_file"; exit 1; }
[[ -x $build_script ]] || { print -u2 "missing executable build script: $build_script"; exit 1; }
[[ -d $upstream/.git ]] || { print -u2 "missing emacs-rime checkout: $upstream"; exit 1; }
[[ -f $x_rime ]] || { print -u2 "missing Emacs Rime config: $x_rime"; exit 1; }
[[ -f $squirrel_config ]] || { print -u2 "missing Squirrel config: $squirrel_config"; exit 1; }
[[ -f $expected_lua_plugin ]] || {
  print -u2 "missing ABI-matched librime Lua plugin: $expected_lua_plugin"
  exit 1
}

tree_fingerprint() {
  local dir=$1 entry

  if [[ ! -d $dir ]]; then
    print MISSING
    return
  fi
  for entry in "$dir"/*(DN); do
    if [[ -L $entry ]]; then
      print -r -- "L ${entry:t} -> $(readlink "$entry")"
    elif [[ -f $entry ]]; then
      print -r -- "F ${entry:t} $(shasum "$entry" | cut -d ' ' -f 1)"
    else
      print -r -- "D ${entry:t}"
    fi
  done
}

before_head=$(git -C "$upstream" rev-parse HEAD)
before_status=$(git -C "$upstream" status --porcelain)
before_hash=$(shasum "$upstream/lib.c" "$upstream/Makefile")
before_build=$(tree_fingerprint "$straight_build")

mkdir -p "$tmp/upstream" "$tmp/patched" "$tmp/user" "$tmp/shared" \
  "$tmp/output" "$tmp/build-tmp" "$tmp/emacs/libexec" \
  "$tmp/librime headers" "$tmp/emacs headers"
ln -s "${librime_header_root%/}/include" "$tmp/librime headers/include"
ln -s "${module_header_root%/}/emacs-module.h" "$tmp/emacs headers/emacs-module.h"
git -C "$upstream" show "${pinned}:lib.c" > "$tmp/upstream/lib.c"
git -C "$upstream" show "${pinned}:Makefile" > "$tmp/upstream/Makefile"
cp "$tmp/upstream/lib.c" "$tmp/upstream/Makefile" "$tmp/patched/"

(
  cd "$tmp/patched"
  git apply --check "$patch_file"
  git apply "$patch_file"
)

rg -U -q 'rime->api = rime_get_api\(\);\n  rime->session_id = 0;' "$tmp/patched/lib.c" || {
  print -u2 "patched module must initialize the guarded session id"
  exit 1
}
rg -U -q 'rime->session_id = 0;\n  rime->first_run = true;' "$tmp/patched/lib.c" || {
  print -u2 "patched module must initialize first_run"
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

TMPDIR="$tmp/build-tmp" \
EMACS_RIME_SOURCE="$tmp/upstream" \
EMACS_RIME_MODULE_DIR="$tmp/output" \
LIBRIME_ROOT="${librime_root%/}" \
LIBRIME_HEADER_ROOT="$tmp/librime headers" \
EMACS_MODULE_HEADER_ROOT="$tmp/emacs headers" \
MODULE_FILE_SUFFIX=.dylib \
  /bin/zsh -x "$build_script" 2> "$tmp/build.trace"

[[ -f $tmp/output/librime-emacs.dylib ]] || {
  print -u2 "builder did not install the requested module"
  exit 1
}
[[ -z $(find "$tmp/build-tmp" -maxdepth 1 -name 'emacs-rime-module.*' -print -quit) ]] || {
  print -u2 "builder left its temporary source behind"
  exit 1
}
source_copy_trace=$(rg -F "cp $tmp/upstream/" "$tmp/build.trace" || true)
[[ $(print -r -- "$source_copy_trace" | wc -l | tr -d ' ') -eq 1 &&
   $source_copy_trace == *"$tmp/upstream/lib.c"* &&
   $source_copy_trace == *"$tmp/upstream/Makefile"* ]] || {
  print -u2 "builder must copy only upstream lib.c and Makefile"
  exit 1
}
otool -L "$tmp/output/librime-emacs.dylib" | rg -q '@rpath/librime'
otool -l "$tmp/output/librime-emacs.dylib" |
  rg -F -q "path ${librime_root%/}/"
otool -L "$expected_lua_plugin" | rg -q '@rpath/librime\.1\.dylib'

cp "$squirrel_config" "$tmp/user/squirrel.custom.yaml"
cp "$repo_root/rime/rime.lua" "$tmp/shared/rime.lua"

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
    - lua_processor@select_character
    - speller
    - selector
    - navigator
    - express_editor
  segmentors:
    - abc_segmentor
    - fallback_segmentor
  translators:
    - echo_translator
  filters:
    - lua_filter@ai_candidate_filter
    - lua_filter@auto_space_filter
speller:
  alphabet: abcdefghijklmnopqrstuvwxyz
YAML

GLOG_minloglevel=2 MallocPreScribble=1 "$emacs" --batch -Q --eval "
(progn
  (module-load \"$tmp/output/librime-emacs.dylib\")
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
                       \"https://ark.cn-beijing.volces.com/api/v3/chat/completions\")
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
        (unless (rime-lib-process-key ?a 0)
          (error \"test input was not accepted\"))
        (let* ((context (rime-lib-get-context))
               (menu (alist-get 'menu context))
               (first (caar (alist-get 'candidates menu))))
          (unless (equal first \"a\")
            (error \"unexpected baseline candidate: %S\" first)))
        (dolist (pair '((\"_ai_candidate\" . \"测试\")
                        (\"_ai_input\" . \"a\")
                        (\"_ai_generation\" . \"1\")))
          (unless (rime-lib-set-property (car pair) (cdr pair))
            (error \"property publication failed: %s\" (car pair))))
        (rime-lib-set-option \"_ai_refresh\"
                             (not (rime-lib-get-option \"_ai_refresh\")))
        (let* ((context (rime-lib-get-context))
               (menu (alist-get 'menu context))
               (first (caar (alist-get 'candidates menu))))
          (unless (equal first \"测试\")
            (error \"Lua AI candidate was not first: %S\" first)))
        (unless (rime-lib-process-key 32 0)
          (error \"AI candidate selection was not accepted\"))
        (unless (equal (rime-lib-get-commit) \"测试\")
          (error \"unexpected AI candidate commit\"))
        (dolist (key (string-to-list \"harness\"))
          (unless (rime-lib-process-key key 0)
            (error \"Return input was not accepted: %c\" key)))
        (unless (rime-lib-process-key 65293 0)
          (error \"Return was not accepted\"))
        (unless (equal (rime-lib-get-commit) \" harness\")
          (error \"Han-to-Return spacing was not committed\"))
        (unless (rime-lib-process-key ?a 0)
          (error \"post-Return input was not accepted\"))
        (let* ((context (rime-lib-get-context))
               (menu (alist-get 'menu context))
               (first (caar (alist-get 'candidates menu))))
          (unless (equal first \" 测试\")
            (error \"Return-to-Han spacing was not shown: %S\" first)))
        (unless (rime-lib-process-key 32 0)
          (error \"spaced AI candidate selection was not accepted\"))
        (unless (equal (rime-lib-get-commit) \" 测试\")
          (error \"Return-to-Han spacing was not committed\")))
    (ignore-errors (rime-lib-finalize))))
"

cp "$build_script" "$tmp/emacs/libexec/"
chmod +x "$tmp/emacs/libexec/build-emacs-rime-module.zsh"

"$emacs" --batch -Q --eval "
(progn
  (require 'cl-lib)
  (add-to-list 'load-path \"$x_rime_lisp_dir\")
  (setq user-emacs-directory \"$tmp/emacs/\")
  (defvar rime--module-path nil)
  (defvar rime--root \"$tmp/package/\")
  (defvar rime-librime-root nil)
  (defvar rime-emacs-module-header-root nil)
  (defvar rime-mode-map (make-sparse-keymap))
  (defvar rime-active-mode-map (make-sparse-keymap))
  (defun rime-compile-module () (error \"upstream compiler called\"))
  (provide 'rime)
  (load-file \"$x_rime\")
  (load-file \"$x_rime\")
  (unless (eq (lookup-key rime-active-mode-map (kbd \"RET\"))
              #'x/rime-return)
    (error \"RET is not bound to x/rime-return\"))
  (unless (eq (lookup-key rime-active-mode-map (kbd \"<return>\"))
              #'x/rime-return)
    (error \"<return> is not bound to x/rime-return\"))
  (let (sent previewed)
    (cl-letf (((symbol-function 'rime-send-keybinding)
               (lambda () (setq sent last-input-event)))
              ((symbol-function 'rime--commit-preview)
               (lambda () (setq previewed t))))
      (setq rime-return-insert-raw t)
      (let ((last-input-event ?\r))
        (x/rime-return))
      (unless (and (eq sent 'return) (not previewed))
        (error \"raw Return was not normalized and sent: %S %S\"
               sent previewed))
      (setq sent nil
            previewed nil
            rime-return-insert-raw nil)
      (let ((last-input-event ?\r))
        (x/rime-return))
      (unless (and previewed (not sent))
        (error \"non-raw Return did not preserve preview: %S %S\"
               sent previewed))))
  (unless (equal rime--module-path
                 (expand-file-name
                  (concat \"var/rime/librime-emacs\" module-file-suffix)
                  user-emacs-directory))
    (error \"unexpected module path: %S\" rime--module-path))
  (unless (advice-member-p #'x/rime-compile-module 'rime-compile-module)
    (error \"custom module compiler is not installed as advice\"))
  (let ((count 0))
    (advice-mapc (lambda (advice _props)
                   (when (eq advice #'x/rime-compile-module)
                     (setq count (1+ count))))
                 'rime-compile-module)
    (unless (= count 1)
      (error \"module compiler advice installed %d times\" count)))
  (setq rime--root \"$tmp/package/\"
        rime-librime-root \"$tmp/librime\"
        rime-emacs-module-header-root \"$tmp/include\")
  (let (called captured-environment)
    (cl-letf (((symbol-function 'call-process)
               (lambda (program _infile _destination _display &rest arguments)
                 (setq called (cons program arguments)
                       captured-environment (copy-sequence process-environment))
                 0)))
      (x/rime-compile-module))
    (unless (equal called
                   (list (expand-file-name
                          \"libexec/build-emacs-rime-module.zsh\"
                          user-emacs-directory)))
      (error \"compiler invoked unexpected command: %S\" called))
    (let ((process-environment captured-environment))
      (dolist (expected
               '((\"EMACS_RIME_SOURCE\" . \"$tmp/package/\")
                 (\"EMACS_RIME_MODULE_DIR\" . \"$tmp/emacs/var/rime/\")
                 (\"LIBRIME_ROOT\" . \"/Library/Input Methods/Squirrel.app/Contents/Frameworks\")
                 (\"LIBRIME_HEADER_ROOT\" . \"$tmp/librime\")
                 (\"EMACS_MODULE_HEADER_ROOT\" . \"$tmp/include\")
                 (\"MODULE_FILE_SUFFIX\" . \".dylib\")))
        (unless (equal (getenv (car expected)) (cdr expected))
          (error \"unexpected %s: %S\"
                 (car expected) (getenv (car expected)))))))
  (let ((failed nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _arguments) 9)))
      (condition-case nil
          (x/rime-compile-module)
        (error (setq failed t))))
    (unless failed
      (error \"nonzero builder exit did not fail compilation\"))))
"

(
  cd "$upstream"
  git apply --check "$patch_file"
)

[[ $(git -C "$upstream" rev-parse HEAD) == $before_head ]]
[[ $(git -C "$upstream" status --porcelain) == $before_status ]]
[[ $(shasum "$upstream/lib.c" "$upstream/Makefile") == $before_hash ]]
[[ $(tree_fingerprint "$straight_build") == $before_build ]]

print "Emacs Rime module regression OK"
