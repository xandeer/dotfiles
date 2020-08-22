UNAME := $(shell uname -s)

ifeq ($(UNAME), Linux)
OS_DIR := $(HOME)/.config/fcitx/rime
endif

ifeq ($(UNAME), Darwin)
OS_DIR := $(HOME)/Library/Rime
endif

EMACS_DIR := $(HOME)/.cache/rime

.PHONY: install
install:
	cp -a $(CURDIR)/*.yaml $(EMACS_DIR)
	cp -a $(CURDIR)/*.yaml $(OS_DIR)
