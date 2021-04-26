;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))

(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(add-hook 'after-init-hook 'recentf-mode)

(setq-default recentf-filename-handlers
              '(substring-no-properties
                ;; Replace $HOME with ~.
                abbreviate-file-name))
(provide 'init-recentf)
;;; init-recentf.el ends here
