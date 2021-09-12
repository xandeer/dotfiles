;;; init-sh.el --- init-sh -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf sh
  :straight company-shell
  :after sh-script
  :config
  (setq company-shell-delete-duplicates t))

(leaf eshell
  :after org
  :custom
  `(eshell-aliases-file . ,(xr/expand-note "etc/eshell.alias")))

(provide 'init-sh)
;;; init-sh.el ends here
