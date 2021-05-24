;;; init-org-journal.el --- Settings for org-journal -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'org-journal)
(leaf org-journal
  :require t
  :after org
  :init
  (setq org-journal-file-format "%Y-%m-%d-%a.org")
  ;; I don't know why this doesn't work.
  (setq org-journal-follow-mode t)
  :bind
  ;; See [[file:init-org-roam.el]]
  ("C-c x j" . org-journal-new-entry)
  ("C-c x t" . org-journal-open-current-journal-file)
  (:org-mode-map
   ("C-c x C-b" . org-journal-open-pervious-entry)
   ("C-c x C-f" . org-journal-open-next-entry)
   ("C-c x C-s" . org-journal-schedule-view))
  :config
  (setq org-journal-file-format "%Y-%m-%d-%a.org")
  (setq org-journal-dir (expand-file-name "journal" org-directory))
  (setq org-journal-file-header ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %Y-%m-%d, %A\n#+STARTUP: content\n\n")
  (setq org-journal-time-format "<%Y-%m-%d %R> ")
  (add-to-list 'auto-mode-alist '("notes/journal/.+\\.org\\'" . org-journal-mode)))

(provide 'init-org-journal)
;;; init-org-journal.el ends here
