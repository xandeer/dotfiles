;;; xandeer/ui/config.el -*- lexical-binding: t; -*-

(setq
 doom-font (font-spec :family "Consola Mono")
 scroll-margin 0
 display-line-numbers-type 'relative)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(use-package! nord-theme
  :config
  (load-theme 'nord t))
