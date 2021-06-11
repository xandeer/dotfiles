;;; init-sh.el --- init-sh -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf sh
  :straight company-shell
  :after sh-script
  :config
  (setq company-shell-delete-duplicates t))

(provide 'init-sh)
;;; init-sh.el ends here
