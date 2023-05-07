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

(add-to-list 'default-frame-alist '(internal-border-width . 2))
(add-to-list 'default-frame-alist '(top . 5))
(add-to-list 'default-frame-alist '(left . 0.5))
(add-to-list 'default-frame-alist '(width . 0.33))
(add-to-list 'default-frame-alist '(height . 0.66))
;; (add-to-list 'default-frame-alist '(background-color . "#f6f1d9"))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 78)))
;; (set-frame-parameter nil 'alpha 90)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(undecorated . t))
;; doesn't work on mac
;; (set-frame-parameter nil 'alpha-background 50)

(provide 'early-init)
;;; early-init.el ends here
