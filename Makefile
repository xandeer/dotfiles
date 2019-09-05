UNAME := $(shell uname -s)
CONDIR := $(HOME)/.config
CONFIGS := i3 i3status doom zsh tmux

.PHONY: install
install: bin dotfiles etc ## Installs the bin and etc directory files and the dotfiles.

.PHONY: bin
bin: ## Installs the bin directory files.
	# add aliases for things in bin
	for file in $(shell find $(CURDIR)/bin -type f -not -name ".*.swp"); do \
		f=$$(basename $$file); \
		sudo ln -sf $$file /usr/local/bin/$$f; \
	done

.PHONY: dotfiles
dotfiles: ## Installs the dotfiles.
	# add aliases for dotfiles
	mkdir -p $(CONDIR);
	for config in $(CONFIGS); do \
		ln -snf $(CURDIR)/$$config $(CONDIR)/$$config; \
	done

	mkdir -p $(HOME)/.local/share;
	ln -snf $(CURDIR)/fonts $(HOME)/.local/share/fonts;
	ln -snf $(CURDIR)/plantuml $(HOME)/.local/share/plantuml;

	ln -snf $(CURDIR)/Xresources $(HOME)/.Xresources;
	ln -snf $(CURDIR)/zsh/zshrc $(HOME)/.zshrc;
	ln -snf $(CURDIR)/profile $(HOME)/.profile;
	ln -snf $(CURDIR)/tmux/tmux.conf $(HOME)/.tmux.conf;
	ln -fn $(CURDIR)/gitignore $(HOME)/.gitignore;

ifeq ($(UNAME), Linux)
	xrdb -merge $(HOME)/.Xresources;
endif

.PHONY: etc
etc: ## Installs the etc directory files.
	for file in $(shell find $(CURDIR)/etc -type f -not -name ".*.swp"); do \
		f=$$(echo $$file | sed -e 's|$(CURDIR)||'); \
		sudo mkdir -p $$(dirname $$f); \
		sudo ln -f $$file $$f; \
	done

.PHONY: update
update: pull install ## Git pull and install all.

.PHONY: pull
pull: ## Git pull.
	git pull;

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
