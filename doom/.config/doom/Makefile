.PHONY: generate
generate: ## Generate config.el and packages.el from configs/index.org
	@echo "Generate config.el and packages.el from configs/index.org"
	@emacs -Q --batch --find-file "configs/index.org" -f "org-org-export-to-org" && \
		mv configs/index.org.org config.org && \
		~/.emacs.d/bin/doom sync

# .PHONY: docs
# docs: ## Update docs
#	org2html README.org && git checkout gh-pages && && mv README.html index.html && git commit -am "Update docs." && git push && git checkout master

.PHONY: help
help: ## Show help.
	@echo 'Targets:'
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\t\033[36m%-30s\033[0m %s\n", $$1, $$2}'
