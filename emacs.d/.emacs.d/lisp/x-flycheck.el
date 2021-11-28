;;; x-flycheck.el --- x-flycheck -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf flycheck
  :straight t
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

(provide 'x-flycheck)
;;; x-flycheck.el ends here
