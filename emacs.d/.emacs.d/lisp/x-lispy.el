;;; x-lispy.el --- x-lispy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'lispy)
(require-package 'multiple-cursors)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(setq lispy-completion-method 'default)
(setq lispy-visit-method 'projectile)
(setq lispy-avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

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
  (lispy-define-key lispy-mode-map (kbd "]") #'x--lispy-insert-square-right)

  (define-key lispy-mode-map (kbd "M-e") #'special-lispy-eval-and-comment)
  (lispy-define-key lispy-mode-map "E" #'lispy-eval-and-replace)

  (lispy-define-key lispy-mode-map "n" #'x/toggle-narrow)

  (lispy-define-key lispy-mode-map "%" #'lispy-delete)
  (lispy-define-key lispy-mode-map "y" #'lispy-new-copy))

(provide 'x-lispy)
;;; x-lispy.el ends here
