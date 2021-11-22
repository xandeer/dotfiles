;;; init-keybindings.el --- basic keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "H-a") 'mark-whole-buffer)
(global-set-key (kbd "H-c") 'kill-ring-save)
(global-set-key (kbd "H-d") 'xr/duplicate-line)
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-v") 'yank)

(global-set-key [remap move-beginning-of-line] 'xr/smart-beginning-of-line)
(global-set-key [remap newline] 'newline-and-indent)

(global-set-key (kbd "H-z") 'undo)
(global-set-key (kbd "H-n") 'make-frame)

(defun xr/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "M-[") 'xr/switch-to-last-buffer)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-x k") 'kill-current-buffer) ; override kill-buffer

(setq which-key-allow-imprecise-window-fit nil)
(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)

(require-package 'keyfreq)
(add-hook 'after-init-hook 'keyfreq-mode)
(add-hook 'after-init-hook 'keyfreq-autosave-mode)
(setq keyfreq-excluded-commands
        '(self-insert-command
          org-self-insert-command
          disable-mouse--handle
          forward-char
          backward-char
          previous-line
          next-line
          newline-and-indent))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
