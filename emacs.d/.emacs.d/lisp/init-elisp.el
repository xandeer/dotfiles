;;; init-elisp.el --- init-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf elisp-mode
  :init
  (setq-default enable-local-variables :safe))

(leaf elisp-def
  :straight t
  :bind
  (:emacs-lisp-mode-map
   ("M-." . elisp-def)))

(leaf overseer
  :disabled t
  :straight t)

;; Behavior-Driven Emacs Lisp Testing
(leaf buttercup
  :disabled t
  :straight t)

(leaf flycheck-package
  :straight t
  :doc "Flycheck checker for elisp package metadata."
  :url "https://github.com/purcell/flycheck-package"
  :tag "lisp"
  :after flycheck
  :config
  (flycheck-package-setup))

(provide 'init-elisp)
;;; init-elisp.el ends here
