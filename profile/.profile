# Shared POSIX-compatible shell entry point.
#
# Files in ~/.config/profile.d/*.sh should remain portable across sh-like shells.

profile_dir="$HOME/.config/profile.d"

source_profile_file() {
  file="$1"

  [ -r "$file" ] || return 0
  [ -f "$file" ] || return 0
  . "$file"
}

if [ -d "$profile_dir" ]; then
  source_profile_file "$profile_dir/env.sh"
  source_profile_file "$profile_dir/path.sh"

  case "$(uname -s 2>/dev/null)" in
    Darwin)
      source_profile_file "$profile_dir/darwin.sh"
      ;;
    Linux)
      source_profile_file "$profile_dir/linux.sh"
      ;;
  esac

  source_profile_file "$profile_dir/local.sh"
fi

unset file
unset profile_dir
