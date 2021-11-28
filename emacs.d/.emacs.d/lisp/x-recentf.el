;;; x-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'recentf)
(with-eval-after-load 'org
  (setq recentf-save-file (x/expand-note "etc/recentf.el"))
  (setq recentf-auto-cleanup 300)
  (setq recentf-max-saved-items 300)
  (setq recentf-filename-handlers '(abbreviate-file-name)) ; Replace $HOME with ~.
  (setq recentf-exclude
        '("/tmp/" "/ssh:" no-littering-var-directory no-littering-etc-directory))

  (add-hook 'after-init-hook 'recentf-mode))

(provide 'x-recentf)
;;; x-recentf.el ends here
