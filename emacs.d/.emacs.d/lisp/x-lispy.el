;;; x-lispy.el --- x-lispy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'lispy)
(require-package 'multiple-cursors)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(with-eval-after-load 'lispy
  (define-key lispy-mode-map (kbd "M-j") #'x/ace-goto-char-timer)
  (define-key lispy-mode-map (kbd "M-k") #'x/switch-to-last-buffer)
  (define-key lispy-mode-map (kbd "C-a") #'x/smart-beginning-of-line))

(provide 'x-lispy)
;;; x-lispy.el ends here
