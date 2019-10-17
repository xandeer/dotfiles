UNAME := $(shell uname -s)
CONFIGS := doom git mr plantuml tmux zsh

ifeq ($(UNAME), Linux)
CONFIGS += i3 i3status rofi xresources
endif

.PHONY: install
install: ## Installs the dotfiles by stow.
	for config in $(CONFIGS); do \
		stow -d $(CURDIR) -t $(HOME) $$config; \
	done

.PHONY: update
update: pull install ## Git pull and install all.

.PHONY: pull
pull: ## Git pull.
	git pull;

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
