EMACS ?= emacs

all: help
.PHONY: all

## Generate all
generate: init.el bootstrap core editor tools langs ui org
.PHONY: generate


## Generate init.el from literate/index.org
init.el: literate/index.org
	@echo "Generate init.el from literate/index.org"
	@$(EMACS) -Q --batch -l export.el --eval '(org-publish "init")'


## Generate bootstrap from literate/bootstrap/bootstrap.org
config/xandeer-bootstrap.el: literate/bootstrap/bootstrap.org
	@echo "Generate bootstrap from literate/bootstrap/bootstrap.org"
	@rm -rf var/org/timestamps/bootstrap.cache straight/build/xandeer-bootstrap
	@$(EMACS) -Q --batch -l export.el --eval '(org-publish "bootstrap")'

## Generate bootstrap from literate/bootstrap/bootstrap.org
bootstrap: config/xandeer-bootstrap.el


## Generate core from literate/core
config/core: $(wildcard literate/core/*.org)
	@echo "Generate core from literate/core"
	@rm -rf var/org/timestamps/core.cache straight/build/xandeer-core
	@$(EMACS) -Q --batch -l export.el --eval '(org-publish "core")'

## Generate core from literate/core
core: config/core


## Generate editor from literate/editor
config/editor: $(wildcard literate/editor/*.org)
	@echo "Generate editor from literate/editor"
	@rm -rf var/org/timestamps/editor.cache straight/build/xandeer-editor
	@$(EMACS) -Q --batch -l export.el --eval '(org-publish "editor")'

## Generate editor from literate/editor
editor: config/editor


## Generate tools from literate/tools
config/tools: $(wildcard literate/tools/*.org)
	@echo "Generate tools from literate/tools"
	@rm -rf var/org/timestamps/tools.cache straight/build/xandeer-tools
	@$(EMACS) -Q --batch -l export.el --eval '(org-publish "tools")'

## Generate tools from literate/tools
tools: config/tools


## Generate langs from literate/langs
config/langs: $(wildcard literate/langs/*.org)
	@echo "Generate langs from literate/langs"
	@rm -rf var/org/timestamps/langs.cache straight/build/xandeer-langs
	@$(EMACS) -Q --batch -l export.el --eval '(org-publish "langs")'

## Generate langs from literate/langs
langs: config/langs


## Generate org from literate/org
config/org: $(wildcard literate/org/*.org)
	@echo "Generate org from literate/org"
	@rm -rf var/org/timestamps/org.cache straight/build/xandeer-org
	@$(EMACS) -Q --batch -l export.el --eval '(org-publish "org")'

## Generate org from literate/org
org: config/org


## Generate ui from literate/ui
config/ui: $(wildcard literate/ui/*.org)
	@echo "Generate ui from literate/ui"
	@rm -rf var/org/timestamps/ui.cache straight/build/xandeer-ui
	@$(EMACS) -Q --batch -l export.el --eval '(org-publish "ui")'

## Generate ui
ui: config/ui
.PHONY: bootstrap core editor tools langs ui org


## clean build (var/org/timestamps/ & config/)
clean-build:
	rm -rf var/org/timestamps
	rm -rf config


## Clean straight (straight/)
clean-straight:
	rm -rf straight
.PHONY: clean-build


## clean all build straight
clean-all: clean clean-build clean-straight
.PHONY: clean-all


## Update config
update: clean-all
	git pull && make generate
.PHONY: update


# Update docs
docs:
	org2html README.org && git checkout gh-pages && mv README.html index.html && git commit -am "Update docs." && git push && git checkout master


# COLORS
GREEN  := $(shell tput -Txterm setaf 2)
YELLOW := $(shell tput -Txterm setaf 3)
WHITE  := $(shell tput -Txterm setaf 7)
RESET  := $(shell tput -Txterm sgr0)

TARGET_MAX_CHAR_NUM=20

## Show help
help:
	@echo ''
	@echo 'Usage:'
	@echo '  ${YELLOW}make${RESET} ${GREEN}<target>${RESET}'
	@echo ''
	@echo 'Targets:'
	@awk '/^[a-zA-Z\-\.\_0-9%]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
		  helpCommand = $$1; sub(/:$$/, "", helpCommand); \
		  helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
		  printf "  ${YELLOW}%-$(TARGET_MAX_CHAR_NUM)s${RESET} ${GREEN}%s${RESET}\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
.PHONY: help
