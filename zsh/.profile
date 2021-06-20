#!/usr/bin/env sh

export XMODIFIERS="@im=fcitx"
for file in $HOME/.config/zsh/*.zsh; do
	if [[ -r "$file" ]] && [[ -f "$file" ]]; then
		source "$file"
	fi
done
unset file

if [ -e /Users/kevin/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/kevin/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
