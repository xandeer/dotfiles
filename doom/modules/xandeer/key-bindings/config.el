;;; xandeer/key-bindings/config.el -*- lexical-binding: t; -*-

;;;-------------------------------- global ------------------------------
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
;;;----------------------------------------------------------------------

;;;-------------------------------- org ---------------------------------
(map! :map org-mode-map
      :gni [M-return]   (λ! (+org/insert-item-below 1))
      :gni [s-return]   (λ! (+org/insert-item-below 1))
      :gni [M-S-return] (λ! (+org/insert-item-above 1))
      :gni [s-S-return] (λ! (+org/insert-item-above 1)))

(map! :map evil-org-mode-map
      :i "C-l" (general-predicate-dispatch 'recenter-top-bottom
                 (org-at-table-p) 'org-table-next-field)
      :i "C-h" (general-predicate-dispatch 'help
                 (org-at-table-p) 'org-table-previous-field)
      :i "C-k" (general-predicate-dispatch 'kill-line
                 (org-at-table-p) '+org/table-previous-row)
      :i "C-j" (general-predicate-dispatch 'org-down-element
                 (org-at-table-p) 'org-table-next-row)

      :ni "M-j" #'org-next-visible-heading
      :ni "M-k" #'org-previous-visible-heading)

(map! :localleader
      :map org-mode-map
      ;; scheduled to tomorrow
      "n" (λ! (org--deadline-or-schedule "" 'scheduled "+1d")))
;;;----------------------------------------------------------------------

;;;-------------------------------- agenda ------------------------------
(defun set-agenda-keys ()
  (evil-define-key 'motion org-agenda-mode-map
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    "t" 'org-agenda-todo
    "i" 'org-agenda-clock-in
    "o" 'org-agenda-clock-goto
    "w" 'org-agenda-week-view
    "d" 'org-agenda-day-view
    "f" 'org-agenda-later
    "b" 'org-agenda-earlier
    "s" 'org-save-all-org-buffers
    "l" 'org-agenda-log-mode))

(advice-add #'evil-org-agenda-set-keys :after #'set-agenda-keys)
;;;----------------------------------------------------------------------

;;;-------------------------------- ivy ---------------------------------
(after! ivy
  (map! :map ivy-minibuffer-map
        "C-d" (λ! (insert (format-time-string "Daily %Y-%m-%d" (current-time))))
        "C-w" (λ! (insert (format-time-string "Words %Y-%m-%d" (current-time))))
        "C-v" #'ivy-scroll-up-command))
;;;----------------------------------------------------------------------
