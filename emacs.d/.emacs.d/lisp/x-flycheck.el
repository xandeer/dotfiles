;;; x-flycheck.el --- x-flycheck -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; config
(with-eval-after-load 'flycheck
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-display-errors-delay 0.25)

  (defalias 'show-error-at-point-soon #'flycheck-show-error-at-point)
  (add-to-list 'flycheck-emacs-lisp-checkdoc-variables 'sentence-end-double-space)

  (add-hook 'prog-mode-hook #'flycheck-mode)

  (define-key flycheck-error-list-mode-map (kbd "C-n") 'flycheck-error-list-next-error)
  (define-key flycheck-error-list-mode-map (kbd "C-p") 'flycheck-error-list-previous-error)
  (define-key flycheck-error-list-mode-map (kbd "RET") 'flycheck-error-list-goto-error))

(provide 'x-flycheck)
;;; x-flycheck.el ends here
