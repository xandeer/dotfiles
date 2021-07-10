;;; early-init.el --- early-init -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)
(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Window size and features
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(setq default-frame-alist
      '((internal-border-width . 12)
        (width . 100)
        (height . 100)
        (alpha . (85 . 60))
        (vertical-scroll-bars . nil)))
(provide 'early-init)
;;; early-init.el ends here
