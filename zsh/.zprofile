typeset -g ZSH_CONFIG_DIR="${ZDOTDIR:-$HOME}/.config/zsh"

if [[ -z ${ZSH_CORE_HELPERS_LOADED:-} && -r "$ZSH_CONFIG_DIR/core/helpers.zsh" ]]; then
  source "$ZSH_CONFIG_DIR/core/helpers.zsh"
fi

if [[ -z ${ZSH_CORE_SOURCE_LOADED:-} && -r "$ZSH_CONFIG_DIR/core/source.zsh" ]]; then
  source "$ZSH_CONFIG_DIR/core/source.zsh"
fi

source_if_exists "$HOME/.profile"

nix_init="$HOME/.nix-profile/etc/profile.d/nix.sh"
if [ -e "$nix_init" ]; then
  . "$nix_init"
fi
export NIX_PATH="darwin-config=$HOME/.nixpkgs/darwin-configuration.nix:/nix/var/nix/profiles/per-user/root/channels:$HOME/.nix-defexpr/channels"

if [[ -x /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"

  brew_wrap="$(brew --prefix)/etc/brew-wrap"
  if [ -f "$brew_wrap" ]; then
    source "$brew_wrap"
  fi
  unset brew_wrap
fi

if zsh_has_command fnm; then
  eval "$(fnm env --shell zsh)" || export PATH="$HOME/.local/share/fnm/aliases/default/bin:$PATH"

  if [[ -n ${FNM_MULTISHELL_PATH:-} ]]; then
    export PATH="$FNM_MULTISHELL_PATH/bin:$PATH"
  fi
fi

unset nix_init
