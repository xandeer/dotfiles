;;; x-lsp.el --- lsp-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; The old version 2.17 doesn't provide function =-compose=,
;; which is required by =lsp-mode=.
;; (require-package
;;  '(dash
;;    :host github
;;    :repo "magnars/dash.el"
;;    :tag "2.18.1"))

;; (require 'dash)
;; (with-eval-after-load 'dash
;;   (dash-enable-font-lock))

(require 'lsp-mode)

(setq lsp-auto-guess-root t)
(setq lsp-enable-snippet nil)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(x/define-keys lsp-mode-map '(("C-c C-f" lsp-format-buffer)
                              ("C-c C-r" lsp-rename)
                              ("M-," lsp-find-references)
                              ("C-," xref-go-back)
                              ("C-." xref-go-forward)))

;;; modes enabled lsp
;; (add-hook 'kotlin-mode-hook #'lsp)

;;; lsp-ui
(require 'lsp-ui)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-always-show t)

(x/define-keys lsp-ui-mode-map
               '(("M-." lsp-ui-peek-find-definitions)
                 ("M-," lsp-ui-peek-find-references)))

;;; corfu
;; https://www.reddit.com/r/emacs/comments/ql8cyp/corfu_orderless_and_lsp/
(setq lsp-completion-provider :none)
(defun x-lsp--corfu-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'x-lsp--corfu-setup)

;;; yasnippet
(autoload 'yas-expand-snippet "yasnippet" nil t)
(add-hook 'lsp-mode-hook #'yas-minor-mode)

(provide 'x-lsp)
;;; x-lsp.el ends here
