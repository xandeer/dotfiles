[[ -n ${ZSH_HOST_LINUX_LOADED:-} ]] && return
typeset -g ZSH_HOST_LINUX_LOADED=1

[[ $OSTYPE == linux* ]] || return 0

if zsh_has_command nixos-rebuild; then
  alias ors='sudo nixos-rebuild switch'
fi
