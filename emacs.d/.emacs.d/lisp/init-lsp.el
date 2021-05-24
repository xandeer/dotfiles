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
(straight-use-package 'dash)
(leaf dash
  :doc "A modern list library for Emacs."
  :url "https://github.com/magnars/dash.el"
  :tag "lists"
  :defer-config
  (dash-enable-font-lock))

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

(leaf lsp-mode
  :require t
  :hook typescript-mode-hook
  :hook
  (typescript-mode-hook . lsp)
  :bind
  (:lsp-mode-map
   ("C-x C-r" . lsp-rename)))

(leaf lsp-ui
  :after lsp-mode
  :require t
  :bind
  (:lsp-ui-mode-map
   ("M-." . lsp-ui-peek-find-definitions)
   ("M-," . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-always-show t))

(provide 'init-lsp)
;;; init-lsp.el ends here
