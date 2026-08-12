#!/bin/zsh

set -eu

script_dir=${0:A:h}
repo_root=${script_dir:h:h:h}
source_dir=${EMACS_RIME_SOURCE:-$HOME/.emacs.d/straight/repos/emacs-rime}
patch_file=${EMACS_RIME_PATCH:-$repo_root/emacs.d/.emacs.d/patches/emacs-rime/0001-add-ai-session-bridge.patch}
librime_root=${LIBRIME_ROOT:-/Library/Input Methods/Squirrel.app/Contents/Frameworks}
librime_header_root=${LIBRIME_HEADER_ROOT:-$HOME/syncthing/personal/configs/librime}
module_header_root=${EMACS_MODULE_HEADER_ROOT:-/opt/homebrew/opt/emacs-plus@30/include}
module_dir=${EMACS_RIME_MODULE_DIR:-$HOME/.emacs.d/var/rime}
suffix=${MODULE_FILE_SUFFIX:-.dylib}

[[ -f $source_dir/lib.c ]] || { print -u2 "missing emacs-rime source: $source_dir/lib.c"; exit 1; }
[[ -f $source_dir/Makefile ]] || { print -u2 "missing emacs-rime source: $source_dir/Makefile"; exit 1; }
[[ -f $patch_file ]] || { print -u2 "missing emacs-rime patch: $patch_file"; exit 1; }
[[ -f $librime_header_root/include/rime_api.h ]] || { print -u2 "missing librime headers: $librime_header_root"; exit 1; }
[[ -f $librime_root/librime.1.dylib ]] || { print -u2 "missing Squirrel librime: $librime_root"; exit 1; }
[[ -f $librime_root/rime-plugins/librime-lua.dylib ]] || { print -u2 "missing Squirrel librime-lua: $librime_root"; exit 1; }
[[ -f $module_header_root/emacs-module.h ]] || { print -u2 "missing Emacs module headers: $module_header_root"; exit 1; }
[[ -n $suffix ]] || { print -u2 "MODULE_FILE_SUFFIX must not be empty"; exit 1; }

build_dir=$(mktemp -d "${TMPDIR:-/tmp}/emacs-rime-module.XXXXXX")
stage=
cleanup() {
  [[ -z $stage || ! -e $stage ]] || rm -f -- "$stage"
  rm -rf -- "$build_dir"
}
trap cleanup EXIT

cp "$source_dir/lib.c" "$source_dir/Makefile" "$build_dir/"
(
  cd "$build_dir"
  git apply --check "$patch_file"
  git apply "$patch_file"
)

make -C "$build_dir" lib \
  MODULE_FILE_SUFFIX="$suffix" \
  CFLAGS="-fPIC -O2 -Wall -I\"${librime_header_root%/}/include/\" -I\"${module_header_root%/}/\"" \
  LDFLAGS="-dynamiclib \"${librime_root%/}/librime.1.dylib\" -Wl,-rpath,\"${librime_root%/}/\"" \
  EMACS_MODULE_HEADER_ROOT="${module_header_root%/}/"

target="$module_dir/librime-emacs$suffix"
mkdir -p "$module_dir"
stage=$(mktemp "$module_dir/.librime-emacs${suffix}.XXXXXX")
cp "$build_dir/librime-emacs$suffix" "$stage"
chmod 755 "$stage"
mv -f "$stage" "$target"
stage=

print "Built $target"
