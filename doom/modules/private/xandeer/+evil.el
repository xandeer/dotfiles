;;; private/xandeer/+evil.el -*- lexical-binding: t; -*-


(defun evil-custom-end-of-buffer-dwim (&rest _)
  "If current line is epty, call `previous-line'."
  (when (looking-at-p "^$")
    (previous-line)))
(advice-add #'end-of-buffer :after #'evil-custom-end-of-buffer-dwim)

(map!
 :i "C-k" #'kill-line
 :i "M-p" #'evil-complete-previous
 :i "M-n" #'evil-complete-next
 :i "C-p" #'evil-previous-line
 :i "C-n" #'evil-next-line)

(general-evil-setup)
(general-imap "j"
  (general-key-dispatch 'self-insert-command
    :timeout 0.25
    "w" (lambda () (interactive) (evil-normal-state) (save-buffer))
    "j" 'evil-normal-state))
