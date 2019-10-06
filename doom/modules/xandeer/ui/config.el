;;; xandeer/ui/config.el -*- lexical-binding: t; -*-

(setq
 scroll-margin 0
 display-line-numbers-type 'relative)

(if IS-MAC
    (+ui/set-font "Consola Mono" "Xingkai SC" 16 18)
  (+ui/set-font "Consola Mono" "WenQuanYi Micro Hei Mono" 24 27))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(use-package! nord-theme
  :config
  (load-theme 'nord t))

(after! org
  (+ui/set-org-pretty-symbols)
  (+ui/init-popup-rules))
