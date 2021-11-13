;;; init-pass.el --- init-pass -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'password-store)
(with-eval-after-load 'password-store
  (setq epg-pinentry-mode 'loopback)
  (defun password-store--run-edit (entry)
    (find-file (password-store--entry-to-file entry)))

  (with-eval-after-load 'embark
    (embark-define-keymap embark-password-store-actions
      "Keymap for actions for password-store."
      ("c" password-store-copy)
      ("f" password-store-copy-field)
      ("i" password-store-insert)
      ("I" password-store-generate)
      ("r" password-store-rename)
      ("e" password-store-edit)
      ("k" password-store-remove)
      ("U" password-store-url))

    (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-actions))

    ;; Either add a prompt classifier or overwrite password-store--completing-read
    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store))))

(provide 'init-pass)
;;; init-pass.el ends here
