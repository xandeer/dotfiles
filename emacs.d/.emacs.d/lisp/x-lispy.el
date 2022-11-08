;;; x-lispy.el --- x-lispy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst x--lispy-modes '(emacs-lisp-mode
                           clojure-mode
                           clojurescript-mode
                           racket-mode))
(dolist (mode x--lispy-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              (lambda () (lispy-mode 1))))

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

  (x/define-keys lispy-mode-map
                 '(("M-j" x/navigation-map)
                   ("M-k" previous-buffer)
                   ("C-a" x/smart-beginning-of-line)
                   ("M-n" lispy-forward)
                   ("M-p" lispy-backward)
                   ("M-e" lispy-eval-and-comment)
                   ("M-," xref-find-references)
                   ("C-." xref-go-forward)
                   ("C-," xref-go-back)))

  (put 'lispy-goto-symbol 'repeat-map 'x/xref-repeat-map)

  (let ((map lispy-mode-map))
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
