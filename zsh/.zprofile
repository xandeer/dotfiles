typeset -g ZSH_CONFIG_DIR="${ZDOTDIR:-$HOME}/.config/zsh"

if [[ -z ${ZSH_CORE_HELPERS_LOADED:-} && -r "$ZSH_CONFIG_DIR/core/helpers.zsh" ]]; then
  source "$ZSH_CONFIG_DIR/core/helpers.zsh"
fi

if [[ -z ${ZSH_CORE_SOURCE_LOADED:-} && -r "$ZSH_CONFIG_DIR/core/source.zsh" ]]; then
  source "$ZSH_CONFIG_DIR/core/source.zsh"
fi

source_if_exists "$HOME/.profile"
source_dir "$ZSH_CONFIG_DIR/login"
