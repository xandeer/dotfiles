;;; init-pass.el --- init-pass -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf password-store
  :straight t
  :require t
  :custom
  (epg-pinentry-mode . 'loopback)
  :config
  (defun password-store--run-edit (entry)
    (find-file (password-store--entry-to-file entry))))

(provide 'init-pass)
;;; init-pass.el ends here
