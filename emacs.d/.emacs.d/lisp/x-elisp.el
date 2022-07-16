;;; x-elisp.el --- x-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(defhydra x-hydra-elisp-helpful (:exit t :columns 4 :idle 0.3)
	"
Elisp\n"
  ("d" helpful-at-point "helpful at point")
  ("f" find-function "find function")
  ("v" find-variable "find variable")
  ("l" find-library "find library")
  ("r" x/load-current "reload current file"))

(define-key emacs-lisp-mode-map (kbd "H-k") #'x-hydra-elisp-helpful/body)

(defun x--end-of-defun ()
  (interactive)
  (forward-char)
  (end-of-defun)
  (backward-char))

;;; disable flycheck on elisp mode
(with-eval-after-load 'flycheck
  (defun x--disable-flycheck ()
    (flycheck-mode -1))
  (add-hook 'emacs-lisp-mode-hook #'x--disable-flycheck))

(setq-default enable-local-variables :safe)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; (require-package 'elisp-demos)
;; (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(define-key emacs-lisp-mode-map (kbd "M-.") #'elisp-def)

(provide 'x-elisp)
;;; x-elisp.el ends here
