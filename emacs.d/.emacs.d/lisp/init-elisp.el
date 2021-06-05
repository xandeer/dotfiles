;;; init-elisp.el --- init-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf elisp-mode
  :bind
  (:emacs-lisp-mode-map
   ("C-j"   . nil)
   ("C-j f" . find-function)
   ("C-j v" . find-variable)
   ("C-j l" . find-library))
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

(provide 'init-elisp)
;;; init-elisp.el ends here
