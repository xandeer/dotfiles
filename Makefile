.PHONY: all
all: help

TERMUX_CHECK := termux-setup-storage
IS_TERMUX := $(shell command -v $(TERMUX_CHECK))

UNAME := $(shell uname -s)
# CONFIGS := doom fonts git mr tmux zsh ideavim bin
CONFIGS := config mr tmux zsh bin lein stardict gpg wakatime authinfo ssh

ifneq ($(IS_TERMUX),)
UNAME := TERMUX
endif

ifneq ($(UNAME), TERMUX)
CONFIGS += mbsync
endif

ifeq ($(UNAME), Linux)
CONFIGS += compton i3 polybar rofi xresources
MACHINE := nix-14
NIX_TARGET := /
NIX_SUDO := sudo
NIX_DIR := /etc/nixos/
endif

ifeq ($(UNAME), Darwin)
MACHINE := darwin-17
NIX_TARGET := $(HOME)
NIX_SUDO :=
NIX_DIR := $(HOME)/.nixpkgs/
endif

MACHINE_DIR := $(CURDIR)/nix/machines/$(MACHINE)

.PHONY: install
install: ## Install the dotfiles by stow.
	mkdir -p $(HOME)/.local/share
	for config in $(CONFIGS); do \
		stow -d $(CURDIR) -t $(HOME) $$config; \
	done
	if test -z "$(IS_TERMUX)"; then \
		$(NIX_SUDO) stow -d $(MACHINE_DIR) -t $(NIX_TARGET) nix; \
		$(NIX_SUDO) ln -sf $(MACHINE_DIR)/*.nix $(NIX_DIR); \
	fi

.PHONY: update
update: pull install ## Git pull and install all.

.PHONY: pull
pull: ## Git pull.
	git pull

.PHONY: dedot
dedot: ## Decrypt dotfiles.
	~/bin/dots dedot

.PHONY: endot
endot: ## Encrypt dotfiles.
	~/bin/dots endot

.PHONY: rime
rime: ## Sync rime.
	~/bin/dots rime

.PHONY: startSyncthing
startSyncthing: ## Start syncthing docker.
	~/bin/dots startSyncthing

.PHONY: killSyncthing
killSyncthing: ## Stop syncthing docker.
	~/bin/dots killSyncthing

.PHONY: help
help: ## Show help.
	@echo 'Targets:'
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\t\033[36m%-30s\033[0m %s\n", $$1, $$2}'
