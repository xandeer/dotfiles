.PHONY: all
all: help

UNAME := $(shell uname -s)
CONFIGS := doom fonts git mr tmux zsh

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
	for config in $(CONFIGS); do \
		stow -d $(CURDIR) -t $(HOME) $$config; \
	done
	$(NIX_SUDO) stow -d $(MACHINE_DIR) -t $(NIX_TARGET) nix
	$(NIX_SUDO) ln -sf $(MACHINE_DIR)/*.nix $(NIX_DIR)

.PHONY: update
update: pull install ## Git pull and install all.

.PHONY: pull
pull: ## Git pull.
	git pull

.PHONY: help
help: ## Show help.
	@echo 'Targets:'
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\t\033[36m%-30s\033[0m %s\n", $$1, $$2}'
