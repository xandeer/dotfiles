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
  (defun x--lispy-insert-square-left ()
    (interactive)
    (insert "["))

  (defun x--lispy-insert-square-right ()
    (interactive)
    (insert "]"))

  (let ((map lispy-mode-map))
    (define-key map (kbd "M-j") 'x/ace-goto-char-timer)
    (define-key map (kbd "M-k") 'x/switch-to-last-buffer)
    (define-key map (kbd "C-a") 'x/smart-beginning-of-line)

    (define-key map (kbd "M-p") 'lispy-backward)
    (define-key map (kbd "M-n") 'lispy-forward)
    (define-key map (kbd "M-e") 'lispy-eval-and-comment)

    (lispy-define-key map (kbd "[") 'x--lispy-insert-square-left)
    (lispy-define-key map (kbd "]") 'x--lispy-insert-square-right)

    (lispy-define-key map "E" 'lispy-eval-and-replace)
    (lispy-define-key map "n" 'x/toggle-narrow)
    (lispy-define-key map "%" 'lispy-delete)
    (lispy-define-key map "o" 'lispy-backward)
    (lispy-define-key map "y" 'lispy-new-copy))

  (setq x-point-lisp-speed-commands
        '(("Navigation")
          ("j" . lispy-down)
          ("k" . lispy-up)
          ("f" . lispy-flow)
          ("b" . lispy-back)
          ("u" . lispy-undo)
          ("d" . lispy-different)

          ("Outline")))

  (defvar x-point-lisp-speed-command nil)

  ;; todo: this-command
  (defun x--lispy-self-insert-command (N)
    (interactive "p")
    (when (x-point-bol-p)
      (back-to-indentation))
    (cond
     ((lispy--in-string-or-comment-p)
      (call-interactively 'self-insert-command))
     ((let ((kv (this-command-keys-vector)))
        (setq x-point-lisp-speed-command
              (when (or (region-active-p)
                        (lispy-left-p)
                        (lispy-right-p)
                        (and (lispy-bolp)
                             (or (looking-at lispy-outline-header)
                                 (looking-at lispy-outline))))
                (cdr (assoc (make-string 1 (aref kv (1- (length kv))))
                            x-point-lisp-speed-commands)))))
      (cond ((commandp x-point-lisp-speed-command)
             (setq this-command x-point-lisp-speed-command)
             (call-interactively x-point-lisp-speed-command))
            ((functionp x-point-lisp-speed-command)
             (funcall x-point-lisp-speed-command))
            ((and x-point-lisp-speed-command (listp x-point-lisp-speed-command))
             (eval x-point-lisp-speed-command))
            (t (call-interactively 'x--lispy-self-insert-command))))
     (t (self-insert-command N))))

  ;(define-key emacs-lisp-mode-map [remap self-insert-command] #'x--lispy-self-insert-command)
  )

(provide 'x-lispy)
;;; x-lispy.el ends here
