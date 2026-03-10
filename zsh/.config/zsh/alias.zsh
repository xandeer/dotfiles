zsh_config_dir="${ZDOTDIR:-$HOME}/.config/zsh"
if [[ -r "$zsh_config_dir/interactive/aliases.zsh" ]]; then
  source "$zsh_config_dir/interactive/aliases.zsh"
fi
unset zsh_config_dir
