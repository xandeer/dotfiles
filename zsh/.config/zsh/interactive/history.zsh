[[ -n ${ZSH_INTERACTIVE_HISTORY_LOADED:-} ]] && return
typeset -g ZSH_INTERACTIVE_HISTORY_LOADED=1

if zplug check "mollifier/anyframe"; then
  zstyle ":anyframe:selector:" use fzf
  zstyle ":anyframe:selector:fzf-tmux:" command 'fzf-tmux --extended'
fi
