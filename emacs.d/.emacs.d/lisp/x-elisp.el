;;; x-elisp.el --- x-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; helpful
(require-package 'helpful)
(defhydra x-hydra-elisp-helpful (:exit t :columns 4 :idle 0.3)
	"
Elisp\n"
  ("d" helpful-at-point "helpful at point")
  ("f" find-function "find function")
  ("v" find-variable "find variable")
  ("l" find-library "find library"))

(define-key emacs-lisp-mode-map (kbd "H-k") #'x-hydra-elisp-helpful/body)

;;; lispy
(define-key emacs-lisp-mode-map (kbd "M-n") #'special-lispy-outline-next)
(define-key emacs-lisp-mode-map (kbd "M-p") #'special-lispy-outline-prev)

;;; disable flycheck on elisp mode
(with-eval-after-load 'flycheck
  (defun x--disable-flycheck ()
    (flycheck-mode -1))
  (add-hook 'emacs-lisp-mode-hook #'x--disable-flycheck))

;;; pretty print
(setq-default enable-local-variables :safe)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;;; help demos
(require-package 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;;; elisp def
(require-package 'elisp-def)
(define-key emacs-lisp-mode-map (kbd "M-.") #'elisp-def)

;;; suggest
(require-package 'suggest)

(provide 'x-elisp)
;;; x-elisp.el ends here
