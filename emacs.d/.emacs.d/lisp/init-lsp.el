;;; init-lsp.el --- lsp-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; The old version 2.17 doesn't provide function =-compose=,
;; which is required by =lsp-mode=.
(straight-register-package
 '(dash
   :host github
   :repo "magnars/dash.el"
   :tag "2.18.1"))
(leaf dash
  :straight t
  :doc "A modern list library for Emacs."
  :url "https://github.com/magnars/dash.el"
  :tag "lists"
  :defer-config
  (dash-enable-font-lock))

(leaf lsp-mode
  :straight t
  :require t
  :hook (typescript-mode-hook . lsp)
  :bind
  (:lsp-mode-map
   ("C-x C-r" . lsp-rename)))

(leaf lsp-ui
  :straight t
  :after lsp-mode
  :require t
  :bind
  (:lsp-ui-mode-map
   ("M-." . lsp-ui-peek-find-definitions)
   ("M-," . lsp-ui-peek-find-references))
  :custom
  ((lsp-ui-sideline-enable
    lsp-ui-doc-enable
    lsp-ui-peek-enable
    lsp-ui-peek-always-show) . t))

(provide 'init-lsp)
;;; init-lsp.el ends here
