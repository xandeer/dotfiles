;;; init-org-agenda.el --- Org-Agenda keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(leaf org-agenda
  :require t
  :after org
  :bind
  ("C-c x a" . org-agenda)
  ("C-c a" . org-agenda-list)
  ("C-c t" . xr/find-gtd)
  ;; (org-agenda 'C "x")
  (:org-agenda-mode-map
   ("M-l" . xr/agenda-toggle-clock-log)
   ("q" . meow-last-buffer)
   ("p" . org-agenda-previous-item)
   ("n" . org-agenda-next-item)
   ("T" . org-agenda-goto-today)
   ("i" . org-agenda-clock-in)
   ("o" . org-agenda-clock-goto))
  :config
  (defun xr/find-gtd ()
    "Find file in gtd."
    (interactive)
    (counsel-find-file (expand-file-name "gtd" org-directory)))
  :config
  (setq org-agenda-files `(,(expand-file-name "gtd/" org-directory)
                           ,(expand-file-name "journal/" org-directory)))
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)" "PHONE" "MEETING")))
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)
  (setq org-clock-clocked-in-display 'both)
  ;; Agenda log mode items to display (closed and state changes by default)
  (setq org-agenda-log-mode-items (quote (clock closed state)))
  (setq xr/agenda-clock-log nil)
  (defun xr/agenda-toggle-clock-log ()
    "Show entries that have received clocked time on that day or not."
    (interactive)
    (if xr/agenda-clock-log
        (progn
          (setq org-agenda-log-mode-items (quote (closed state)))
          (setq xr/agenda-clock-log nil))
      (progn
        (setq org-agenda-log-mode-items (quote (clock closed state)))
        (setq xr/agenda-clock-log t)))
    (org-agenda-redo))
  (setq org-agenda-persistent-filter t)
  (setq org-agenda-show-future-repeats 'next)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-start-with-clockreport-mode t)
  (setq org-agenda-use-time-grid nil)
  (setq org-agenda-clock-consistency-checks
        (quote (:max-duration "4:00"
                              :min-duration 0
                              :max-gap 0
                              :gap-ok-around ("4:00"))))
  (setq org-agenda-include-diary t)
  (setq diary-file (expand-file-name "gtd/standard-diary" org-directory))
  (setq org-agenda-window-setup 'other-window)
  (setq org-agenda-sorting-strategy
        '((agenda time-up category-keep)
          (todo category-keep)
          (tags category-keep)
          (search category-keep)))
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)
  (setq org-agenda-sticky t)
  (setq org-agenda-text-search-extra-files (quote (agenda-archives)))

  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        (quote (("n" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("p" "Phone Calls" tags "PHONE"
                 ((org-agenda-overriding-header "Phone Calls")
                 (org-tags-match-list-sublevels nil)))
                ("h" "Habits" tags-todo "STYLE=\"habit\""
                 ((org-agenda-overriding-header "Habits")
                  (org-agenda-sorting-strategy
                   '(todo-state-down effort-up category-keep))))
                ("x" "Agenda"
                 ((tags-todo "-CANCELLED/!NEXT"
                             ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))
                  (tags "REFILE"
                        ((org-agenda-overriding-header "Tasks to Refile")
                         (org-tags-match-list-sublevels nil)))
                  (tags-todo "-CANCELLED/!"
                             ((org-agenda-overriding-header "Stuck Projects")
                              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-HOLD-CANCELLED/!"
                             ((org-agenda-overriding-header "Projects")
                              (org-agenda-skip-function 'bh/skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-CANCELLED+WAITING|HOLD/!"
                             ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-tasks)
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                  (tags "-REFILE/"
                        ((org-agenda-overriding-header "Tasks to Archive")
                         (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                         (org-tags-match-list-sublevels nil))))
                 nil))))

  (setq calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (setq calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  ;; location
  (setq calendar-longitude 113.9442)
  (setq calendar-latitude 22.5395)

  ;; Copied from https://emacs-china.org/t/05-org-as/12092/4
  ;; 日出而作, 日落而息
  (defun xandeer/diary-sunrise ()
    (let ((dss (diary-sunrise-sunset)))
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (while (re-search-forward " ([^)]*)" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (search-forward ",")
        (buffer-substring (point-min) (match-beginning 0)))))

  (defun xandeer/diary-sunset ()
    (let ((dss (diary-sunrise-sunset))
          start end)
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (while (re-search-forward " ([^)]*)" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (search-forward ", ")
        (setq start (match-end 0))
        (search-forward " at")
        (setq end (match-beginning 0))
        (goto-char start)
        (capitalize-word 1)
        (buffer-substring start end)))))

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
