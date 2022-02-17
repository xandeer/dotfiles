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
        (top . 50)
        (left . 0.5)
        (width . 0.33)
        (height . 0.66)
        (alpha . (85 . 78))
        (vertical-scroll-bars . nil)))

(provide 'early-init)
;;; early-init.el ends here
