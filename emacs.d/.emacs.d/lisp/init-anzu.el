;;; init-anzu.el --- anzu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf anzu
  :straight t
  :hook
  (after-init-hook . global-anzu-mode)
  :bind
  ("M-K" . anzu-query-replace-regexp))

(provide 'init-anzu)
;;; init-anzu.el ends here
