;;; init-exec-path.el --- init-exec-path -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf exec-path-from-shell
  :straight t
  :custom
  (exec-path-from-shell-check-startup-files . nil)
  :config
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
