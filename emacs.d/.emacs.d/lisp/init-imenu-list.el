;;; init-imenu-list.el --- init-imenu-list -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'imenu-list)
(leaf imenu-list
  :doc "Emacs plugin to show the current buffer's imenu entries in a seperate buffer"
  :url "https://github.com/bmag/imenu-list"
  :tag "tools" "convenience"
  :bind (("C-." . imenu-list-smart-toggle))
  :custom (imenu-list-auto-resize . t))

(provide 'init-imenu-list)
;;; init-imenu-list.el ends here
