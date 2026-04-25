profile_path="$HOME/.config/profile.d/path.sh"
if [[ -r "$profile_path" ]]; then
  source "$profile_path"
fi

case "$OSTYPE" in
  darwin*)
    profile_os="$HOME/.config/profile.d/darwin.sh"
    ;;
  linux*)
    profile_os="$HOME/.config/profile.d/linux.sh"
    ;;
  *)
    profile_os=""
    ;;
esac

if [[ -n "$profile_os" && -r "$profile_os" ]]; then
  source "$profile_os"
fi

unset profile_os
unset profile_path
