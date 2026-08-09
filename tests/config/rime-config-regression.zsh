#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
schema="$repo_root/rime/double_pinyin_flypy.schema.yaml"
others_dict="$repo_root/rime/cn_dicts/others.dict.yaml"
melt_eng_dict="$repo_root/rime/melt_eng.dict.yaml"

reject_match() {
  local pattern="$1" file="$2" message="$3" rg_status=0

  rg -- "$pattern" "$file" >/dev/null || rg_status=$?
  case "$rg_status" in
    0)
      print -u2 -- "$message"
      exit 1
      ;;
    1) ;;
    *)
      print -u2 -- "failed to inspect $file with rg (exit $rg_status)"
      exit 1
      ;;
  esac
}

for config_file in "$schema" "$others_dict" "$melt_eng_dict"; do
  [[ -f "$config_file" ]] || {
    print -u2 "expected Rime config at $config_file"
    exit 1
  }
done

reject_match '^[[:space:]]*-[[:space:]]*lua_filter@reduce_english_filter([[:space:]]|$)' "$schema" \
  "expected active lua_filter@reduce_english_filter to be removed"
reject_match '^[[:space:]]*-[[:space:]]*reverse_lookup_translator@xklbdz([[:space:]]|$)' "$schema" \
  "expected active reverse_lookup_translator@xklbdz to be removed"
reject_match '^[[:space:]]*reverse_lookup:[[:space:]]*"\^u\[a-z\]\+\$"[[:space:]]*(#.*)?$' "$schema" \
  'expected active reverse_lookup: "^u[a-z]+$" xklbdz recognizer pattern to be removed'

rg '^[[:space:]]*unicode:[[:space:]]*"\^U\[0-9A-Fa-f\]\+\$"[[:space:]]*(#.*)?$' "$schema" >/dev/null || {
  print -u2 'expected active unicode: "^U[0-9A-Fa-f]+$" recognizer pattern'
  exit 1
}

for reading in 'yi qi dang qian' 'yi ji dang qian'; do
  reject_match "^一骑当千 +${reading}([[:space:]]|$)" "$others_dict" \
    "expected malformed space-separated 一骑当千 $reading entry to be removed"
  rg -- $'^一骑当千\t'"$reading"$'(\t[^\t]+)?$' "$others_dict" >/dev/null || {
    print -u2 "expected 一骑当千 $reading to use a TAB separator"
    exit 1
  }
done

reject_match '^[[:space:]]*-[[:space:]]*en_dicts/cn_en([[:space:]]|$)' "$melt_eng_dict" \
  "expected active en_dicts/cn_en import to be removed"
