;;; init-flycheck.el --- init-flycheck -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf flycheck
  :straight t
  ;; :bind (("C-x C-s" . save-buffer-maybe-show-errors))
  :hook prog-mode-hook
  :custom
  (flycheck-display-errors-function
   . #'flycheck-display-error-messages-unless-error-list)
  (flycheck-check-syntax-automatically . '(save idle-change mode-enabled))
  (flycheck-display-errors-delay       . 0.25)
  :bind
  (:flycheck-error-list-mode-map
   ("C-n" . flycheck-error-list-next-error)
   ("C-p" . flycheck-error-list-previous-error)
   ("RET" . flycheck-error-list-goto-error)
   ([return]  . flycheck-error-list-goto-error))
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point)
  (add-to-list 'flycheck-emacs-lisp-checkdoc-variables 'sentence-end-double-space))

(leaf flycheck-package
  :straight t
  :doc "Flycheck checker for elisp package metadata."
  :url "https://github.com/purcell/flycheck-package"
  :tag "lisp"
  :after flycheck
  :config
  (flycheck-package-setup))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
