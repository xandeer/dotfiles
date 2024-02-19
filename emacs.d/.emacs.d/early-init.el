;;; early-init.el --- early-init -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)
(setq package-enable-at-startup nil)
;;; Adjust garbage collection thresholds during startup, and thereafter
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
            (lambda ()
              (require 'gcmh)
              (gcmh-mode)
              (setq gcmh-low-cons-threshold #x800000)
              (setq gcmh-high-cons-threshold #x880000)))

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

(let ((frame-options
       '((internal-border-width . 8)
         (top . 5)
         (left . 0.5)
         (width . 0.33)
         (height . 0.66)
         ;; (background-color . "#f6f1d9")
         ;; (alpha . (85 . 78))
         (vertical-scroll-bars)
         (undecorated . t))))
  (mapc (lambda (option)
          (add-to-list 'default-frame-alist option))
        frame-options))

;; (set-frame-parameter nil 'alpha 90)
;; doesn't work on mac
;; (set-frame-parameter nil 'alpha-background 50)

(provide 'early-init)
;;; early-init.el ends here
