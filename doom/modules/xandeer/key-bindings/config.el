;;; xandeer/key-bindings/config.el -*- lexical-binding: t; -*-

(map! :leader
  :desc "Switch buffer"  "."    #'switch-to-buffer
  :desc "Select the treemacs window if it is visible"    "z"    #'treemacs-select-window
  :desc "Agenda List"    "a"    #'org-agenda-list)

(map!
 :i "C-d" #'delete-char
 :i "C-f" #'forward-char
 :i "C-b" #'backward-char
 :i "C-k" #'kill-line
 :i "M-p" #'evil-complete-previous
 :i "M-n" #'evil-complete-next
 :i "C-p" #'evil-previous-line
 :i "C-n" #'evil-next-line)

(map! :map override
      ;; override for org mode
      :i "C-d" #'delete-char

      :gni "M-h" #'+workspace/switch-left
      :gni "M-l" #'+workspace/switch-right

      :i "C-y" #'yank
      :i "M-y" #'yank-pop
      :i "C-r" #'isearch-backward
      )

(general-evil-setup)
(general-imap "j"
  (general-key-dispatch 'self-insert-command
    :timeout 0.25
    "w" (lambda () (interactive) (evil-normal-state) (save-buffer))
    "j" 'evil-normal-state))
