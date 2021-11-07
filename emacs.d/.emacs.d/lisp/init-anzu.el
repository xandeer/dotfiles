;;; init-anzu.el --- anzu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'anzu)
(add-hook 'after-init-hook 'global-anzu-mode)
(global-set-key (kbd "M-K") 'anzu-query-replace-regexp)

(provide 'init-anzu)
;;; init-anzu.el ends here
