;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf no-littering
  :init
  (setq-default
   recentf-max-saved-items 1000
   recentf-exclude '("/tmp/" "/ssh:"))
  :hook (after-init-hook . recentf-mode)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (setq-default recentf-filename-handlers
                '(substring-no-properties
                  ;; Replace $HOME with ~.
                  abbreviate-file-name)))

(provide 'init-recentf)
;;; init-recentf.el ends here
