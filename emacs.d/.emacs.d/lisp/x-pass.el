;;; x-pass.el --- x-pass -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq epg-gpg-home-directory "~/.gnupg")

(with-eval-after-load 'password-store
  (setq epg-pinentry-mode 'loopback)
  (defun password-store--run-edit (entry)
    (find-file (password-store--entry-to-file entry)))

  (with-eval-after-load 'embark
    (defvar-keymap embark-password-store-actions
      :doc "Keymap for actions for password-store."
      :parent embark-general-map
      "c" 'password-store-copy
      "f" 'password-store-copy-field
      "i" 'password-store-insert
      "I" 'password-store-generate
      "r" 'password-store-rename
      "e" 'password-store-edit
      "k" 'password-store-remove
      "U" 'password-store-url)

    (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-actions))

    ;; Either add a prompt classifier or overwrite password-store--completing-read
    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store))))

(provide 'x-pass)
;;; x-pass.el ends here
