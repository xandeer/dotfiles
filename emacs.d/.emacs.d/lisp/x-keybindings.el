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

(x/define-keys global-map
               '(([remap move-beginning-of-line] x/smart-beginning-of-line)
                 ([remap newline] newline-and-indent)
                 ("M-k" previous-buffer)
                 ("M-[" previous-buffer)
                 ("M-]" next-buffer)
                 ("M-;" comment-line)
                 ("H-z" undo)))

(x/define-keys ctl-x-map
               '(("k" kill-current-buffer)))

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

(provide 'x-keybindings)
;;; x-keybindings.el ends here
