;;; x-elisp.el --- x-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'helpful)
(defhydra hydra-elisp-helpful (:hint nil :exit t)
  "
Find the definition near point:
_f_: Function    _v_: Variable     _l_: Library

_d_: Helpful at point

Cancel: _q_ cancel
"
  ("d" helpful-at-point)
  ("f" find-function)
  ("v" find-variable)
  ("l" find-library)
  ("q" nil))

(global-set-key (kbd "C-j") 'hydra-elisp-helpful/body)

;; Show evaluation result on the right of cursor.
;; (require-package 'eros)
;; (x/append-init-hook #'eros-mode)

(setq-default enable-local-variables :safe)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(require-package 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(require-package 'elisp-def)
(define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-def)

(require-package 'suggest)

(provide 'x-elisp)
;;; x-elisp.el ends here
