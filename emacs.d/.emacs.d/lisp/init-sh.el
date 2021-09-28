;;; init-sh.el --- init-sh -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf company-shell
  :straight t
  :after sh-script
  :config
  (add-to-list 'company-backends 'company-shell))

(leaf eshell-mode
  :after org
  :hook (eshell-mode-hook . (lambda () (define-key eshell-mode-map [(control ?s)] #'xr/eshell-search-history) (company-mode -1)))
  :custom
  `(eshell-aliases-file . ,(xr/expand-note "etc/eshell.alias"))
  :init
  (defun xr/eshell-search-history ()
    "Search the eshell command history with ivy."
    (interactive)
    (require 'em-hist)
    (let* ((ivy-completion-beg (eshell-bol))
           (ivy-completion-end (point-at-eol))
           (input (buffer-substring-no-properties
                   ivy-completion-beg
                   ivy-completion-end)))
      (ivy-read "Command "
                (delete-dups
                 (when (> (ring-size eshell-history-ring) 0)
                   (ring-elements eshell-history-ring)))
                :initial-input input
                :action #'ivy-completion-in-region-action))))

(leaf eshell-z
  :straight t
  :after eshell
  :hook (eshell-mode-hook . (lambda () (require 'eshell-z))))

(leaf makefile-executor
  :straight t)

(provide 'init-sh)
;;; init-sh.el ends here
