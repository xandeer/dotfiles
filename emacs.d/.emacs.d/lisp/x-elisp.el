;;; x-elisp.el --- x-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/define-keys
 help-map
 '(("f" helpful-callable)
   ("v" helpful-variable)
   ("k" helpful-key)
   ("F" helpful-function)
   ("C" helpful-command)))

(transient-define-prefix x/transient-elisp-helpful ()
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

(with-eval-after-load 'ert
  ;; https://docs.racket-lang.org/test-engine/index.html#%28form._%28%28lib._test-engine%2Fracket-tests..rkt%29._check-satisfied%29%29
  (defun statified? (exp pred?)
    "Return t if `EXP' is statified by `PRED?'."
    (should (funcall pred? exp)))

  (defun statified-not? (exp pred?)
    "Return t if `EXP' is not statified by `PRED?'."
    (should-not (funcall pred? exp))))

(provide 'x-elisp)
;;; x-elisp.el ends here
