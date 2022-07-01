UNAME := $(shell uname -s)

DIRS := $(HOME)/.cache/rime

ifeq ($(UNAME), Linux)
DIRS += $(HOME)/.config/fcitx/rime
OS := linux
endif

ifeq ($(UNAME), Darwin)
DIRS += $(HOME)/Library/Rime
OS := darwin
endif

LOCAL := local.dict

.PHONY: install
install:
	for dir in $(DIRS); do \
		mkdir -p $$dir; \
		cp -n $(CURDIR)/$(LOCAL) $$dir/$(LOCAL).yaml; \
		cp -RL $(CURDIR)/*.yaml $$dir; \
		cp -a $(CURDIR)/$(OS)/*.yaml $$dir; \
	done
