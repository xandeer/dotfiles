;;; x-lispy.el --- x-lispy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'lispy)
(require-package 'multiple-cursors)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(with-eval-after-load 'lispy
  (define-key lispy-mode-map (kbd "M-j") #'x/ace-goto-char-timer)
  (define-key lispy-mode-map (kbd "M-k") #'x/switch-to-last-buffer)
  (define-key lispy-mode-map (kbd "C-a") #'x/smart-beginning-of-line)

  (define-key lispy-mode-map (kbd "M-p") #'lispy-backward)
  (define-key lispy-mode-map (kbd "M-n") #'lispy-forward)

  (defun x--lispy-insert-square-left ()
    (interactive)
    (insert "["))
  (defun x--lispy-insert-square-right ()
    (interactive)
    (insert "]"))

  (lispy-define-key lispy-mode-map (kbd "[") #'x--lispy-insert-square-left)
  (lispy-define-key lispy-mode-map (kbd "]") #'x--lispy-insert-square-right))

(provide 'x-lispy)
;;; x-lispy.el ends here
