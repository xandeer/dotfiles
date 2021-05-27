;;; init-ts.el --- typescript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf tide
  :straight t typescript-mode
  :after typescript-mode flycheck company
  :require t
  :bind
  ("C-x f". tide-format)
  (:tide-mode-map
   ("M-." . lsp-ui-peek-find-definitions)
   ("M-," . lsp-ui-peek-find-references))
  :hook
  (typescript-mode-hook . tide-setup)
  (typescript-mode-hook . tide-hl-identifier-mode)
  (before-save-hook . tide-format-before-save))

(provide 'init-ts)
;;; init-ts.el ends here
