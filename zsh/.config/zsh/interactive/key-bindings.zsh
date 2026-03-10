[[ -n ${ZSH_INTERACTIVE_KEY_BINDINGS_LOADED:-} ]] && return
typeset -g ZSH_INTERACTIVE_KEY_BINDINGS_LOADED=1

my-accept-line() {
  if [[ ${#${(z)BUFFER}} -eq 0 ]]; then
    echo

    if git rev-parse --git-dir > /dev/null 2>&1; then
      git status -sb
    else
      ls -G
    fi
  fi

  zle accept-line
}
zle -N my-accept-line
bindkey '^M' my-accept-line

if zsh_has_command fzf; then
  __fzf_use_tmux__() {
    [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ]
  }

  __fzfcmd() {
    __fzf_use_tmux__ &&
      echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
  }

  fzf-history-widget() {
    local selected num
    setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
    selected=( $(fc -rl 1 |
      FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(qqq)LBUFFER} +m" $(__fzfcmd)) )
    local ret=$?

    if [[ -n "$selected" ]]; then
      num=$selected[1]
      if [[ -n "$num" ]]; then
        zle vi-fetch-history -n $num
      fi
    fi

    zle redisplay
    typeset -f zle-line-init >/dev/null && zle zle-line-init
    return $ret
  }

  zle -N fzf-history-widget
  bindkey '^R' fzf-history-widget
fi

if zplug check "zsh-users/zsh-history-substring-search"; then
  zmodload zsh/terminfo

  [[ -n ${terminfo[kcuu1]:-} ]] && bindkey "$terminfo[kcuu1]" history-substring-search-up
  [[ -n ${terminfo[kcud1]:-} ]] && bindkey "$terminfo[kcud1]" history-substring-search-down
  bindkey "^[[1;5A" history-substring-search-up
  bindkey "^[[1;5B" history-substring-search-down
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down
  bindkey -M emacs '^P' history-substring-search-up
  bindkey -M emacs '^N' history-substring-search-down
  bindkey -M vicmd '^p' history-substring-search-up
  bindkey -M vicmd '^n' history-substring-search-down
  bindkey -M vicmd 'k' history-substring-search-up
  bindkey -M vicmd 'j' history-substring-search-down
fi

if zplug check "zsh-users/zsh-autosuggestions"; then
  bindkey '^ ' autosuggest-accept
  bindkey '\es' autosuggest-execute
fi

if zplug check "hchbaw/zce.zsh"; then
  bindkey '\eg' zce
fi
