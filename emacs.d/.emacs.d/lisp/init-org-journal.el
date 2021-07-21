;;; init-org-journal.el --- Settings for org-journal -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf org-journal
  :straight t
  :require t
  :after org
  :hook
  (org-journal-mode-hook . (lambda () (visual-line-mode -1)))
  :init
  (setq org-journal-file-format "%Y-%m-%d.org")
  ;; I don't know why this doesn't work.
  (setq org-journal-follow-mode t)
  :bind
  ("C-c x t" . org-journal-open-current-journal-file)
  :config
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-dir (expand-file-name "journal" org-directory))
  (setq org-journal-file-header ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %B %d\n#+STARTUP: content\n\n")
  (setq org-journal-time-format "<%Y-%m-%d %R> ")
  (add-to-list 'auto-mode-alist '("notes/journal/.+\\.org\\'" . org-journal-mode)))

(provide 'init-org-journal)
;;; init-org-journal.el ends here
