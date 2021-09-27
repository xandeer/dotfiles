;;; init-org-journal.el --- Settings for org-journal -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf org-journal
  :disabled t
  :straight t
  :require t
  :bind
  ("H-t" . org-journal-open-current-journal-file)
  :hook
  (org-journal-mode-hook . valign-mode)
  ;; visual line mode will override the org-special-ctrl-a/e
  (org-journal-mode-hook . disable-visual-line-mode)
  :custom
  (org-journal-follow-mode . t)
  (org-journal-file-format . "%Y-%m-%d.org")
  (org-journal-time-format . "<%Y-%m-%d %R> ")
  (org-journal-date-format . "%B %d")
  (org-journal-dir         . `,(expand-file-name "journal" org-directory))
  (org-journal-file-header . ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %B %d\n#+STARTUP: content\n\n")
  :config
  (defun disable-visual-line-mode ()
    (visual-line-mode -1)))

(provide 'init-org-journal)
;;; init-org-journal.el ends here
