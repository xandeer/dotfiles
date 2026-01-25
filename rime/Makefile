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

.PHONY: install
install:
	for dir in $(DIRS); do \
		mkdir -p $$dir; \
		cp -a $(CURDIR)/rime.lua $$dir; \
		cp -a $(CURDIR)/opencc $$dir; \
		cp -a $(CURDIR)/cn_dicts $$dir; \
		cp -a $(CURDIR)/en_dicts $$dir; \
		cp -RL $(CURDIR)/*.yaml $$dir; \
		cp -a $(CURDIR)/$(OS)/*.yaml $$dir; \
		ln -sf $(CURDIR)/amz-v2n3m1-zh-hans.gram $$dir; \
	done
