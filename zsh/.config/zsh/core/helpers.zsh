[[ -n ${ZSH_CORE_HELPERS_LOADED:-} ]] && return
typeset -g ZSH_CORE_HELPERS_LOADED=1

zsh_has_command() {
  (( $+commands[$1] ))
}

zsh_is_darwin() {
  [[ $OSTYPE == darwin* ]]
}

zsh_is_linux() {
  [[ $OSTYPE == linux* ]]
}
