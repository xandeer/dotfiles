[[ -n ${ZSH_HOST_KEVINS_MAC_STUDIO_LOADED:-} ]] && return
typeset -g ZSH_HOST_KEVINS_MAC_STUDIO_LOADED=1

[[ "$(hostname -s 2>/dev/null)" == "Kevins-Mac-Studio" ]] || return 0

set_proxy() {
  local is_termux
  is_termux="$(command -v termux-setup-storage)"

  if [[ -z "$is_termux" ]]; then
    export px='http://127.0.0.1:8010'
    export https_proxy="$px"
    export http_proxy="$px"
    export all_proxy="$px"
  fi
}

alias p11='set_proxy'
