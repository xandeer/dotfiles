;;; x-elisp.el --- x-elisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(with-eval-after-load 'helpful
  (x/define-keys helpful-mode-map
                 '(("f" . x/link-hint-open-in-current-window))))

(defhydra x-hydra-elisp-helpful (:exit t :columns 4 :idle 0.3)
	"
Elisp\n"
  ("d" helpful-at-point "helpful at point")
  ("f" find-function "find function")
  ("v" find-variable "find variable")
  ("l" find-library "find library")
  ("r" x/load-current "reload current file"))

(define-key emacs-lisp-mode-map (kbd "H-k") #'x-hydra-elisp-helpful/body)

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

(define-key emacs-lisp-mode-map (kbd "M-.") #'elisp-def)

(provide 'x-elisp)
;;; x-elisp.el ends here
