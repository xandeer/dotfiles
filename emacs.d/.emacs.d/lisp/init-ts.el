;;; init-ts.el --- typescript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf typescript-mode
  :straight t tide
  :bind
  ("C-x f". tide-format)
  (:tide-mode-map
   :package tide
   ("M-." . lsp-ui-peek-find-definitions)
   ("M-," . lsp-ui-peek-find-references))
  :hook
  (typescript-mode-hook . tide-setup)
  (typescript-mode-hook . tide-hl-identifier-mode)
  (before-save-hook . tide-format-before-save))

(provide 'init-ts)
;;; init-ts.el ends here
