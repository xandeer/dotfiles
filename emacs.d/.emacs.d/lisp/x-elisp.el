;;; x-elisp.el --- x-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(define-transient-command x/transient-elisp-helpful ()
  "Transient for Elisp."
  [["Elisp"
    ("d" "Helpful at point" helpful-at-point)
    ("f" "Find function" find-function)
    ("v" "Find variable" find-variable)
    ("l" "Find library" find-library)
    ("r" "Reload current file" x/load-current)]])

(define-key emacs-lisp-mode-map (kbd "H-k") #'x/transient-elisp-helpful)

;; disable flycheck on elisp mode
(setq flycheck-disabled-checkers '(emacs-lisp))

(defun x/elisp-setup ()
  "Setup for elisp mode."
  (add-to-list 'completion-at-point-functions #'cape-abbrev))
(add-hook 'emacs-lisp-mode-hook #'x/elisp-setup)

(setq-default enable-local-variables :safe)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(x/append-init-hook #'eros-mode)

;; (require-package 'elisp-demos)
;; (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(provide 'x-elisp)
;;; x-elisp.el ends here
