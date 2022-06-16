;;; x-lsp.el --- lsp-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; The old version 2.17 doesn't provide function =-compose=,
;; which is required by =lsp-mode=.
(require-package
 '(dash
   :host github
   :repo "magnars/dash.el"
   :tag "2.18.1"))

(require 'dash)
(with-eval-after-load 'dash
  (dash-enable-font-lock))

(require-package 'lsp-mode)
(require 'lsp-mode)

(setq lsp-auto-guess-root nil)
(setq lsp-enable-snippet nil)

(define-key lsp-mode-map (kbd "C-x C-r") #'lsp-rename)
(define-key lsp-mode-map (kbd "C-x f") #'lsp-format-buffer)

(add-hook 'typescript-mode-hook #'lsp)

(require-package 'lsp-ui)
(require 'lsp-ui)

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-always-show t)

(define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "M-,") #'lsp-ui-peek-find-references)

(provide 'x-lsp)
;;; x-lsp.el ends here
