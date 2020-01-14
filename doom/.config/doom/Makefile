.PHONY: all
all: help

.PHONY: generate
generate: ## Generate config.el and packages.el from config.org
	@echo "Generate config.el and packages.el from config.org"
	@emacs -Q --batch --find-file "config.org" -f "org-org-export-to-org" && \
		emacs -Q --batch --find-file "config.org.org" -f "org-babel-tangle" && \
		rm -rf "config.org.org"

.PHONY: docs
docs: ## Update docs
	org2html README.org && git checkout gh-pages && && mv README.html index.html && git commit -am "Update docs." && git push && git checkout master

.PHONY: help
help: ## Show help.
	@echo 'Targets:'
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\t\033[36m%-30s\033[0m %s\n", $$1, $$2}'
