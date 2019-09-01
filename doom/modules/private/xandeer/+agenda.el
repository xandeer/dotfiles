;;; private/xandeer/+agenda.el -*- lexical-binding: t; -*-

(after! org
  (setq-default
   org-agenda-span 'day
   org-agenda-start-day nil
   org-clock-clocked-in-display 'both
   org-agenda-time-grid '((daily today require-timed)
          (700 900 1000 1100 1400 1500 1600 1700)
          "......"
          "----------------")
   org-agenda-start-on-weekday nil))

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
