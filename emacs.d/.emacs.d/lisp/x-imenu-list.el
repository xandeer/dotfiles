;;; x-imenu-list.el --- x-imenu-list -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf imenu-list
  :straight t
  :bind
  ("C-," . imenu)
  ("C-." . imenu-list-smart-toggle)
  :custom
  (imenu-list-auto-resize . t))

(provide 'x-imenu-list)
;;; x-imenu-list.el ends here
