;;; init-imenu-list.el --- init-imenu-list -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf imenu-list
  :straight t
  :bind
  ("C-," . imenu)
  ("C-." . imenu-list-smart-toggle)
  :custom
  (imenu-list-auto-resize . t))

(provide 'init-imenu-list)
;;; init-imenu-list.el ends here
