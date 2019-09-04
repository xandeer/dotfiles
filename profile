#!/usr/bin/env sh

export XMODIFIERS="@im=fcitx"

for file in $HOME/.config/zsh/{proxy,alias,functions,path,extra,exports}; do
	if [[ -r "$file" ]] && [[ -f "$file" ]]; then
		source "$file"
	fi
done
unset file
