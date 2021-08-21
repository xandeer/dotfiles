;;; init-exec-path.el --- init-exec-path -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-check-startup-files . nil))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
