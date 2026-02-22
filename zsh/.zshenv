nix_init=$HOME/.nix-profile/etc/profile.d/nix.sh
if [ -e $nix_init ]; then . $nix_init; fi # added by Nix installer
export NIX_PATH="darwin-config=$HOME/.nixpkgs/darwin-configuration.nix:/nix/var/nix/profiles/per-user/root/channels:$HOME/.nix-defexpr/channels"

# export PATH=/opt/homebrew/bin:$HOME/.nix-profile/bin:$PATH
eval "$(/opt/homebrew/bin/brew shellenv)"
if [ -f $(brew --prefix)/etc/brew-wrap ];then
  source $(brew --prefix)/etc/brew-wrap
fi

if command -v fnm >/dev/null 2>&1; then
  eval "$(fnm env --shell zsh)" || export PATH="$HOME/.local/share/fnm/aliases/default/bin:$PATH"
  export PATH="$FNM_MULTISHELL_PATH/bin:$PATH"
fi
