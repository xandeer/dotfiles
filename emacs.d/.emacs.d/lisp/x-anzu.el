;;; x-anzu.el --- anzu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/append-init-hook #'global-anzu-mode)
(global-set-key (kbd "H-K") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-x %") #'anzu-query-replace-regexp)

(provide 'x-anzu)
;;; x-anzu.el ends here
