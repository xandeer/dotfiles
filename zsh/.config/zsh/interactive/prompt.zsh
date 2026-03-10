[[ -n ${ZSH_INTERACTIVE_PROMPT_LOADED:-} ]] && return
typeset -g ZSH_INTERACTIVE_PROMPT_LOADED=1

SPACESHIP_PROMPT_ORDER=(
  time
  dir
  git
  exec_time
  line_sep
  jobs
  char
)

SPACESHIP_TIME_SHOW=true
SPACESHIP_EXIT_CODE_SHOW=true
SPACESHIP_EXEC_TIME_SHOW=true
