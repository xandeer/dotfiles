;;; x-template.el --- x-template -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(tempel :host github
        :repo "minad/tempel"
        :branch "main"))
(require-package 'tempel t)

;;; config
(setq tempel-file (expand-file-name "etc/templates" user-emacs-directory))

;;; key bindings
(define-key tempel-map (kbd "TAB") #'tempel-next)
(define-key tempel-map (kbd "C-<tab>") #'tempel-previous)
(define-key tempel-map (kbd "C-g") #'tempel-done)
(define-key tempel-map (kbd "C-c") #'tempel-abort)
(define-key tempel-map (kbd "M-a") #'tempel-beginning)
(define-key tempel-map (kbd "M-e") #'tempel-end)

;;; org
(defhydra x/hydra-template-org
  (:exit t :columns 4 :idle 0.3)
  "
Org Templates
"
  (";" (tempel-insert 'comment) "comment")
  ("q" (tempel-insert 'quote) "quote")
  ("v" (tempel-insert 'verse) "verse")
  ("w" x/wrap-block "wrap block")
  ("e" (tempel-insert 'elisp) "elisp")
  ("s" (tempel-insert 'sh) "sh")
  ("M-s" (tempel-insert 'src) "src")
  ("k" (tempel-insert 'kotlin) "kotlin")
  ("c" (tempel-insert 'clojure) "clojure")
  ("r" (tempel-insert 'weekly) "weekly review")
  ("m" (tempel-insert 'monthly) "monthly review")
  ("t" (tempel-insert 'time) "timestamp")
  ("d" (tempel-insert 'day) "day: 14(Fri)"))

(define-key org-mode-map (kbd "M-t") #'x/hydra-template-org/body)

;;; elisp
(defhydra x/hydra-template-elisp (:exit t :columns 4 :idle 0.3)
  "
Elisp Templates
"
  ("d" (tempel-insert 'docs) "elisp header and footer")
  ("h" (tempel-insert 'hydra) "defhydra")
  ("l" (tempel-insert 'lambda) "lambda")
  ("f" (tempel-insert 'fun) "defun")
  ("c" (tempel-insert 'command) "command")
  ("M-l" (tempel-insert 'let) "let")
  ("v" (tempel-insert 'var) "defvar")
  ("M-c" (tempel-insert 'const) "defconst")
  ("S-c" (tempel-insert 'custom) "defcustom"))

(define-key emacs-lisp-mode-map (kbd "M-t") #'x/hydra-template-elisp/body)

;;; global
(defhydra x/hydra-template-global (:exit t :columns 4 :idle 0.3)
  "
Global Templates
"
  ("d" (tempel-insert 'chglog) "git changelog")
  ("v" (tempel-insert 'version) "git app version")
  ("t" (tempel-insert 'time) "timestamp"))
(global-set-key (kbd "M-t") #'x/hydra-template-global/body)

(provide 'x-template)
;;; x-template.el ends here
