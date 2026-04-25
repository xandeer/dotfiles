zsh_config_dir="${ZDOTDIR:-$HOME}/.config/zsh"
host_name="$(hostname -s 2>/dev/null)"
host_proxy="$zsh_config_dir/host/$host_name.zsh"

if [[ -r "$host_proxy" ]]; then
  source "$host_proxy"
fi

unset host_name
unset host_proxy
unset zsh_config_dir
