profile_env="$HOME/.config/profile.d/env.sh"

if [[ -r "$profile_env" ]]; then
  source "$profile_env"
fi

unset profile_env
