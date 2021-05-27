;;; init-anzu.el --- anzu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf anzu
  :straight t
  :doc "anzu.el is an Emacs port of anzu.vim."
  :url "https://github.com/emacsorphanage/anzu"
  :hook (after-init-hook . global-anzu-mode)
  :bind (([remap query-replace]        . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ("M-K" . anzu-query-replace-regexp)))

(provide 'init-anzu)
;;; init-anzu.el ends here
