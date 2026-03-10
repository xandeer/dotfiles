[[ -n ${ZSH_HOST_DARWIN_LOADED:-} ]] && return
typeset -g ZSH_HOST_DARWIN_LOADED=1

[[ $OSTYPE == darwin* ]] || return 0

if zsh_has_command darwin-rebuild; then
  alias ors='darwin-rebuild switch'
fi
