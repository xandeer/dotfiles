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
(define-key tempel-map (kbd "C-a") #'tempel-beginning)
(define-key tempel-map (kbd "C-e") #'tempel-end)

;;; org
(defhydra x/hydra-template-org
  (:exit t :columns 4 :idle 0.3)
  "
Org Skeleton
"
  ("b" (tempel-insert 'block) "block")
  (";" (tempel-insert 'comment) "comment")
  ("q" (tempel-insert 'quote) "quote")
  ("v" (tempel-insert 'verse) "verse")
  ("w" x/wrap-block "wrap block")
  ("c" (tempel-insert 'src) "code")
  ("e" (tempel-insert 'elisp) "elisp")
  ("s" x--org-block-sh "sh")
  ("k" x--org-block-kt "kotlin")
  ("r" x--weekly-review "weekly review")
  ("m" x--monthly-review "monthly review")
  ("t" x--text-time "timestamp")
  ("d" x--text-day "day: 14(Fri)"))

(define-key org-mode-map (kbd "M-t") #'x/hydra-template-org/body)

(provide 'x-template)
;;; x-template.el ends here
