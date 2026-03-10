# Optional local machine overrides belong here.

local_profile="$HOME/.config/profile.local.sh"
if [ -f "$local_profile" ]; then
  . "$local_profile"
fi

unset local_profile
