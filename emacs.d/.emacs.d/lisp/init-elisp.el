;;; init-elisp.el --- init-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf helpful
  :straight t
  :bind ("C-j" . hydra-elisp-find-func/body)
  :hydra
  (hydra-elisp-find-func
   (:hint nil :exit t)
   "
Find the definition near point:
_f_: Function    _v_: Variable     _l_: Library

_d_: Helpful at point

Cancel: _q_ cancel
"
   ("d" helpful-at-point)
   ("f" find-function)
   ("v" find-variable)
   ("l" find-library)

   ("q" nil)))

;; Show evaluation result on the right of cursor.
(leaf eros
  :straight t
  :hook after-init-hook)

(leaf elisp-mode
  :init
  (setq-default enable-local-variables :safe)
  ;; This will be overriden by eros.
  :bind ([remap eval-last-sexp] . pp-eval-last-sexp)
  )

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
