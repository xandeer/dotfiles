;;; x-keybindings.el --- basic keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; https://stackoverflow.com/questions/145291/smart-home-in-emacs/145359
(defun x/smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line`.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [remap move-beginning-of-line] 'x/smart-beginning-of-line)
(global-set-key [remap newline] 'newline-and-indent)

(global-set-key (kbd "H-z") 'undo)
;; (global-set-key (kbd "H-n") 'make-frame)

(defun x/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "M-[") 'x/switch-to-last-buffer)
(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-x k") 'kill-current-buffer) ; override kill-buffer

(setq which-key-allow-imprecise-window-fit nil)
(require-package 'which-key)
(x/append-init-hook 'which-key-mode)

(require-package 'keyfreq)
(x/append-init-hook 'keyfreq-mode)
(x/append-init-hook 'keyfreq-autosave-mode)
(setq keyfreq-excluded-commands
        '(self-insert-command
          org-self-insert-command
          disable-mouse--handle
          forward-char
          backward-char
          previous-line
          next-line
          newline-and-indent))

(require-package 'command-log-mode)

(provide 'x-keybindings)
;;; x-keybindings.el ends here
