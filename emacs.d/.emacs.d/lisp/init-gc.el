;;; init-gc.el --- init-gc -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf gcmh
  :doc "Use GCMH --  the Garbage Collector Magic Hack -- to adjust garbage collection."
  :url "https://gitlab.com/koral/gcmh"
  :straight t
  :hook after-init-hook
  :init
  (setq-default gc-cons-threshold #x8100000)
  :custom
  (gcmh-verbose             . t)
  (gcmh-lows-cons-threshold . #x800000)
  (gcmh-high-cons-threshold . #x8100000)
  (gcmh-idle-delay          . 3600))

(provide 'init-gc)
;;; init-gc.el ends here
