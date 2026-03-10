if zsh_has_command fnm; then
  eval "$(fnm env --shell zsh)" || export PATH="$HOME/.local/share/fnm/aliases/default/bin:$PATH"

  if [[ -n ${FNM_MULTISHELL_PATH:-} ]]; then
    export PATH="$FNM_MULTISHELL_PATH/bin:$PATH"
  fi
fi
