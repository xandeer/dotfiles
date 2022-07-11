;;; x-mouse.el --- x-mouse -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
  blink-cursor-interval .6
  blink-matching-paren  t
  cursor-in-non-selected-windows t)
(blink-cursor-mode 1)

(setq hscroll-margin                  3
      scroll-margin                   3
      hscroll-step                    3
      scroll-step                     3
      scroll-conservatively           100000
      scroll-preserve-screen-position 'always
      scroll-error-top-bottom         t)

(setq pixel-scroll-precision-mode t)
;; (setq mouse-wheel-scroll-amount '(1))

;; (require-package 'disable-mouse nil)
;; (with-eval-after-load 'disable-mouse
;;   (setq disable-mouse-wheel-events nil)
;;   (global-disable-mouse-mode))

(provide 'x-mouse)
;;; x-mouse.el ends here
