;;; xandeer/snippets/config.el -*- lexical-binding: t; -*-

(use-package! yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/doom/snippets"))
  (yas-global-mode 1))
