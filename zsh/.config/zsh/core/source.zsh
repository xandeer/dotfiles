[[ -n ${ZSH_CORE_SOURCE_LOADED:-} ]] && return
typeset -g ZSH_CORE_SOURCE_LOADED=1

source_if_exists() {
  local file="$1"

  [[ -r "$file" && -f "$file" ]] || return 0
  source "$file"
}

source_dir() {
  emulate -L zsh
  setopt local_options null_glob

  local dir="$1"
  local file

  [[ -d "$dir" ]] || return 0

  for file in "$dir"/*.zsh; do
    source_if_exists "$file"
  done
}
