# Shared POSIX-compatible shell entry point.
#
# Files in ~/.config/profile.d/*.sh should remain portable across sh-like shells.

profile_dir="$HOME/.config/profile.d"

if [ -d "$profile_dir" ]; then
  for file in "$profile_dir"/*.sh; do
    [ -r "$file" ] || continue
    [ -f "$file" ] || continue
    . "$file"
  done
fi

unset file
unset profile_dir
