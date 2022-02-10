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
(setq lispy-key-theme '(lispy c-digits))

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
    (lispy-define-key map (kbd "]") 'x--lispy-insert-square-right))

  (setq x-point-lisp-speed-commands
        '(("Navigation")
          ("j" . lispy-down)
          ("k" . lispy-up)
          ("f" . lispy-flow)
          ("b" . lispy-back)
          ("l" . lispy-right)
          ("u" . lispy-left)
          ("p" . lispy-eval-other-window)
          ("P" . lispy-paste)
          ("y" . lispy-occur)

          ("outline")
          ;; ("J" . lispy-outline-next)
          ;; ("K" . lispy-outline-prev)
          ("L" . lispy-outline-goto-child)

          ("Paredit transformations")
          (">" . lispy-slurp)
          ("<" . lispy-barf)
          ("/" . lispy-splice)
          ("r" . lispy-raise)
          ("R" . lispy-raise-some)
          ("+" . lispy-join)

          ("more transformations")
          ;; ("C" . lispy-convolute)
          ;; ("X" . lispy-convolute-left)
          ("w" . lispy-move-up)
          ("s" . lispy-move-down)
          ("O" . lispy-oneline)
          ("M" . lispy-alt-multiline)
          ("S" . lispy-stringify)

          ("marking")
          ("a" . lispy-ace-symbol)
          ("H" . lispy-ace-symbol-replace)
          ("m" . lispy-mark-list)

          ("dialect-specific")
          ("e" . lispy-eval)
          ("E" . lispy-eval-and-replace)
          ("G" . lispy-goto-local)
          ("g" . lispy-goto)
          ("F" . (lispy-follow t))
          ("A" . lispy-beginning-of-defun)
          ("_" . lispy-underscore)

          ("miscellanea")
          (" " . lispy-space)
          ("c" . lispy-kill)
          ("i" . lispy-tab)
          ("I" . lispy-shifttab)
          ("D" . lispy-clone)
          ("q" . lispy-ace-paren)
          ("Q" . lispy-ace-char)
          ("t" . lispy-teleport)
          ("W" . lispy-new-copy)
          ("b" . lispy-back)
          ("B" . lispy-ediff-regions)
          ("Z" . lispy-edebug-stop)
          ("V" . lispy-visit)
          ("-" . lispy-ace-subword)
          ("." . lispy-repeat)
          ("~" . lispy-tilde)))

  (defvar x--lispy-modes
    '(lisp-interaction-mode
      emacs-lisp-mode
      clojure-mode
      clojurescript-mode))

  (defun x--lispy-modes-p ()
    ;; (when (derived-mode-p
    ;;        'lisp-interaction-mode 'emacs-lisp-mode ))
    (member major-mode x--lispy-modes))

  (defun x-point-lisp-speed-command-activate (keys)
    "Hook for activating single-letter speed commands.
See `x-point-lisp-speed-commands' for configuring them."
    (when (and (x--lispy-modes-p)
               (x-point-bol-p))
      (back-to-indentation))
    (when (and (x--lispy-modes-p)
               (or (region-active-p)
                   (x-point-left-sexp-p)
                   (x-point-right-sexp-p)
                   (and (lispy-bolp)
                        (or (looking-at lispy-outline-header)
                            (looking-at lispy-outline)))))
      (cdr (assoc keys (append x-point-lisp-speed-commands
                               x-point-speed-commands)))))

  (add-hook 'x-point-speed-command-hook #'x-point-lisp-speed-command-activate -90)

  (define-key emacs-lisp-mode-map [remap self-insert-command] #'x-point-self-insert-command))

(provide 'x-lispy)
;;; x-lispy.el ends here
