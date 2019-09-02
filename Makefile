.PHONY: all
all: bin dotfiles etc ## Installs the bin and etc directory files and the dotfiles.

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
	ln -fn $(CURDIR)/gitignore $(HOME)/.gitignore;
	mkdir -p $(HOME)/.config;
	ln -snf $(CURDIR)/i3 $(HOME)/.config/i3;
	ln -snf $(CURDIR)/i3status $(HOME)/.config/i3status;
	ln -snf $(CURDIR)/doom $(HOME)/.config/doom;
	ln -snf $(CURDIR)/zsh $(HOME)/.config/zsh;
	ln -snf $(CURDIR)/tmux $(HOME)/.config/tmux;

	mkdir -p $(HOME)/.local/share;
	ln -snf $(CURDIR)/fonts $(HOME)/.local/share/fonts;

	ln -snf $(CURDIR)/Xresources $(HOME)/.Xresources;
	xrdb -merge $(HOME)/.Xresources;
	ln -snf $(CURDIR)/zsh/zshrc $(HOME)/.zshrc;
	ln -snf $(CURDIR)/profile $(HOME)/.profile;
	ln -snf $(CURDIR)/tmux/tmux.conf $(HOME)/.tmux.conf;
	ln -snf $(CURDIR)/tmux/tmux.conf $(HOME)/.tmux.conf;

.PHONY: etc
etc: ## Installs the etc directory files.
	for file in $(shell find $(CURDIR)/etc -type f -not -name ".*.swp"); do \
		f=$$(echo $$file | sed -e 's|$(CURDIR)||'); \
		sudo mkdir -p $$(dirname $$f); \
		sudo ln -f $$file $$f; \
	done

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
