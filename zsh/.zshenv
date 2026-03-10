typeset -g ZSH_CONFIG_DIR="${ZDOTDIR:-$HOME}/.config/zsh"

if [[ -r "$ZSH_CONFIG_DIR/core/helpers.zsh" ]]; then
  source "$ZSH_CONFIG_DIR/core/helpers.zsh"
fi

if [[ -r "$ZSH_CONFIG_DIR/core/source.zsh" ]]; then
  source "$ZSH_CONFIG_DIR/core/source.zsh"
fi
