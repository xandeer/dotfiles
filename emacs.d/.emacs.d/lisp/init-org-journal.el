;;; init-org-journal.el --- Settings for org-journal -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf org-journal
  :straight t
  :mode
  ("notes/journal/.+\\.org\\'" . org-journal-mode)
  :bind
  ("C-c x t" . org-journal-open-current-journal-file)
  :custom
  (org-journal-follow-mode . t)
  (org-journal-file-format . "%Y-%m-%d.org")
  (org-journal-time-format . "<%Y-%m-%d %R> ")
  (org-journal-dir         . `,(expand-file-name "journal" org-directory))
  (org-journal-file-header . ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %B %d\n#+STARTUP: content\n\n"))

(provide 'init-org-journal)
;;; init-org-journal.el ends here
