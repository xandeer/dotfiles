;;; x-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'recentf)
(setq recentf-save-file (x/expand-note "etc/recentf.el"))
(setq recentf-auto-cleanup 300)
(setq recentf-max-saved-items 300)
;; Replace $HOME with ~.
(setq recentf-filename-handlers '(abbreviate-file-name))
(setq recentf-exclude
      '("/tmp/" "/ssh:" no-littering-var-directory no-littering-etc-directory))

(x/append-init-hook #'recentf-mode)

(provide 'x-recentf)
;;; x-recentf.el ends here
