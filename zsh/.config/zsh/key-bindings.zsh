[[ -o interactive ]] || return 0

zsh_config_dir="${ZDOTDIR:-$HOME}/.config/zsh"
if [[ -r "$zsh_config_dir/interactive/key-bindings.zsh" ]]; then
  source "$zsh_config_dir/interactive/key-bindings.zsh"
fi
unset zsh_config_dir
