UNAME := $(shell uname -s)
CONDIR := $(HOME)/.config
CONFIGS := doom zsh tmux
ifeq ($(UNAME), Linux)
CONFIGS := i3 i3status doom zsh tmux
endif

.PHONY: install
install: ## Installs the dotfiles.
	mkdir -p $(CONDIR);
	for config in $(CONFIGS); do \
		ln -snf $(CURDIR)/$$config $(CONDIR)/$$config; \
	done

	mkdir -p $(HOME)/.local/share;
	ln -snf $(CURDIR)/plantuml $(HOME)/.local/share/plantuml;

	ln -snf $(CURDIR)/zsh/zshrc $(HOME)/.zshrc;
	ln -snf $(CURDIR)/profile $(HOME)/.profile;
	ln -snf $(CURDIR)/tmux/tmux.conf $(HOME)/.tmux.conf;
	ln -fn $(CURDIR)/gitignore $(HOME)/.gitignore;

ifeq ($(UNAME), Linux)
	ln -snf $(CURDIR)/Xresources $(HOME)/.Xresources;
	xrdb -merge $(HOME)/.Xresources;
endif

.PHONY: update
update: pull install ## Git pull and install all.

.PHONY: pull
pull: ## Git pull.
	git pull;

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
