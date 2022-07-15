;;; x-lispy.el --- x-lispy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'lispy)
(require-package 'multiple-cursors)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'clojurescript-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))

(setq lispy-completion-method 'default)
(setq lispy-visit-method 'projectile)
(setq lispy-avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; (setq lispy-key-theme '(lispy c-digits))

(with-eval-after-load 'lispy
  (defun x--lispy-insert-square-left ()
    (interactive)
    (insert "["))

  (defun x--lispy-insert-square-right ()
    (interactive)
    (insert "]"))

  (let ((map lispy-mode-map))
    (define-key map (kbd "M-j") 'x-navigation-map)
    (define-key map (kbd "M-k") 'x/switch-to-last-buffer)
    (define-key map (kbd "C-a") 'x/smart-beginning-of-line)

    (define-key map (kbd "M-p") 'lispy-backward)
    (define-key map (kbd "M-n") 'lispy-forward)
    (define-key map (kbd "M-e") 'lispy-eval-and-comment)

    (lispy-define-key map (kbd "[") 'x--lispy-insert-square-left)
    (lispy-define-key map (kbd "]") 'x--lispy-insert-square-right)

    ;; navigation
    (lispy-define-key map "u" 'lispy-left)

    ;; miscellanea
    (lispy-define-key map "c" 'lispy-kill)
    (lispy-define-key map "n" 'x/toggle-narrow)
    (lispy-define-key map "W" 'lispy-new-copy)
    (lispy-define-key map "D" 'lispy-clone)))

(provide 'x-lispy)
;;; x-lispy.el ends here
