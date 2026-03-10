nix_init="$HOME/.nix-profile/etc/profile.d/nix.sh"
if [ -e "$nix_init" ]; then
  . "$nix_init"
fi

unset nix_init
