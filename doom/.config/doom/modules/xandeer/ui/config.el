;;; xandeer/ui/config.el -*- lexical-binding: t; -*-

(setq
 scroll-margin 0
 display-line-numbers-type 'visual)

(if IS-MAC
    (+ui/set-font "Consola Mono" "CloudKaiXingGBK" 16 18)
  (+ui/set-font "Consola Mono" "CloudKaiXingGBK" 30 36))
; fonts test
; 锐字云字库行楷体锐字云字库行楷体锐字云字库行楷体锐字云字库行楷体锐字云字库行楷
; HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(use-package! nord-theme
  :config
  (load-theme 'nord t))

(after! org
  (+ui/set-org-pretty-symbols)
  (+ui/init-popup-rules))
