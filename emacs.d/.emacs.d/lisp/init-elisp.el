;;; init-elisp.el --- init-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf elisp-mode
  :init
  (setq-default enable-local-variables :safe))

(leaf helpful
  :straight t
  :bind
  ("C-c C-d" . helpful-at-point))

(leaf elisp-demos
  :straight t
  :advice
  (:after helpful-update elisp-demos-advice-helpful-update))

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
  :after flycheck
  :config
  (flycheck-package-setup))

(provide 'init-elisp)
;;; init-elisp.el ends here
