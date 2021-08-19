;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf recentf
  :after org
  :require t
  :custom
  (recentf-save-file         . `,(expand-file-name "etc/recentf.el" org-directory))
  (recentf-auto-cleanup      . 300)
  (recentf-max-saved-items   . 300)
  (recentf-filename-handlers . '(abbreviate-file-name)) ; Replace $HOME with ~.
  (recentf-exclude
   . '("/tmp/" "/ssh:" no-littering-var-directory no-littering-etc-directory)))

(provide 'init-recentf)
;;; init-recentf.el ends here
