;;; x-key-helper.el --- key helper -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq which-key-allow-imprecise-window-fit nil)
(x/append-init-hook #'which-key-mode)

(x/append-init-hook #'keyfreq-mode)
(x/append-init-hook #'keyfreq-autosave-mode)
(setq keyfreq-excluded-commands
        '(self-insert-command
          org-self-insert-command
          disable-mouse--handle
          forward-char
          backward-char
          previous-line
          next-line
          newline-and-indent))

(provide 'x-key-helper)
;;; x-key-helper.el ends here
