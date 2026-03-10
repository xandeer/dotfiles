# Shared environment variables that should work in POSIX shells.

export XMODIFIERS="@im=fcitx"
export DOTS_DIR="$HOME/projects/personal/dotfiles"

export EDITOR="emacsclient"
export GIT_EDITOR="vim"

export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--bind='ctrl-o:execute(code {})+abort'"

export NIX_PATH="darwin-config=$HOME/.nixpkgs/darwin-configuration.nix:/nix/var/nix/profiles/per-user/root/channels:$HOME/.nix-defexpr/channels"

if [ -f "$HOME/.config/auth.env" ]; then
  . "$HOME/.config/auth.env"
fi
