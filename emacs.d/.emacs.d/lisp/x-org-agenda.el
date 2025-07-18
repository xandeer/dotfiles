;;; x-org-agenda.el --- Org-Agenda keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'org
  (require 'org-agenda))

(setq org-agenda-files `(,(x/expand-note "gtd/")))
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)" "PHONE")))
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
(setq org-agenda-log-mode-items '(clock closed state))

(defvar x--agenda-clock-log nil)
(defun x/agenda-toggle-clock-log ()
  "Show entries that have received clocked time on that day or not."
  (interactive)
  (if x--agenda-clock-log
      (progn
        (setq org-agenda-log-mode-items '(closed state))
        (setq x--agenda-clock-log nil))
    (progn
      (setq org-agenda-log-mode-items '(clock closed state))
      (setq x--agenda-clock-log t)))
  (org-agenda-redo))

(setq org-agenda-persistent-filter t)
(setq org-agenda-show-future-repeats 'next)
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-start-with-clockreport-mode t)
(setq org-agenda-use-time-grid t)
(setq org-agenda-time-grid
 '((daily today require-timed)
   (800 930 1125 1625 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setq org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────────────")
(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
                            :min-duration 0
                            :max-gap 0
                            :gap-ok-around ("4:00"))))
(setq org-agenda-include-diary t)
(setq diary-file (x/expand-note "gtd/standard-diary"))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-sorting-strategy
      '((agenda time-up category-keep)
        (todo category-keep)
        (tags category-keep)
        (search category-keep)))
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)
(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)
(setq org-agenda-sticky t)
(setq org-agenda-text-search-extra-files '(agenda-archives))

(setq calendar-chinese-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq calendar-chinese-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
;; location
(setq calendar-longitude 113.9442)
(setq calendar-latitude 22.5395)

;; Copied from https://emacs-china.org/t/05-org-as/12092/4
;; 日出而作, 日落而息
(defun x/diary-sunrise ()
  (let ((dss (diary-sunrise-sunset)))
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ",")
      (buffer-substring (point-min) (match-beginning 0)))))

(defun x/diary-sunset ()
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
      (buffer-substring start end))))

(defun x/org-agenda-schedule (arg &optional time)
  "Schedule the item at point.
ARG is passed through to `x/org-schedule'."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-agenda-schedule arg t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
		                  (org-agenda-error)))
	        (type (marker-insertion-type marker))
	        (buffer (marker-buffer marker))
	        (pos (marker-position marker))
	        ts)
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	       (widen)
	       (goto-char pos)
	       (setq ts (x/org-schedule)))
       (org-agenda-show-new-time marker ts " S"))
     (message "%s" ts))))

(with-eval-after-load 'org-agenda
  (x/define-keys org-agenda-mode-map
                 '(("M-l" x/agenda-toggle-clock-log)
                   ("q" previous-buffer)
                   ("s" x/org-agenda-schedule)
                   ("k" org-agenda-previous-item)
                   ("p" org-agenda-previous-item)
                   ("n" org-agenda-next-item)
                   ("j" org-agenda-next-item)
                   ("T" org-agenda-goto-today)
                   ("i" org-agenda-clock-in)
                   ("o" org-agenda-clock-goto)
                   ("O" org-agenda-clock-goto))))

;;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("n" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("i" "Inbox"
         ((tags-todo "REFILE+CATEGORY=\"Inbox\""
                     ((org-agenda-overriding-header "Inbox")))))
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
         nil)))

;;; agenda emacs
(add-to-list 'org-agenda-custom-commands
             '("e" "Emacs"
               ((tags-todo "EMACS+Sword"
                           ((org-agenda-overriding-header "Sword")))
                (tags-todo "EMACS+CATEGORY=\"One-off\""
                           ((org-agenda-overriding-header "One Off")))
                (tags-todo "EMACS+CATEGORY=\"Xwidget\""
                           ((org-agenda-overriding-header "Xwidget")))
                (tags-todo "EMACS+CATEGORY=\"Embark\""
                           ((org-agenda-overriding-header "Embark"))))))

(defun x/create-project-agenda-views (key command-title tag categories)
  "Create a project-specific agenda command.
KEY is the key for the command.
COMMAND-TITLE is the title of the command.
TAG is the project tag to search for.
CATEGORIES is a list of sub-category strings."
  `(,key ,command-title
       ,(mapcar (lambda (category)
                  (let ((header (if (string= category "Feat") "Feature" category)))
                    `(tags-todo ,(format "%s+CATEGORY=\"%s\"" tag category)
                                ((org-agenda-overriding-header ,header)))))
                categories)))

(defvar x/project-categories '("Bug" "Feat" "Refactor" "Review" "Chore" "Other")
  "Default categories for a project.")

;;; agenda personal
(add-to-list 'org-agenda-custom-commands
             (x/create-project-agenda-views "p" "Personal" "PERSONAL"
                                            '("Learning" "Tasks" "Review" "Ideas" "Habits")))

(add-to-list 'org-agenda-custom-commands
             (x/create-project-agenda-views "m" "MoonDust" "MoonDust" x/project-categories))

(add-to-list 'org-agenda-custom-commands
             (x/create-project-agenda-views "w" "Hold Lines" "WordPower" x/project-categories))

(add-to-list 'org-agenda-custom-commands
             (x/create-project-agenda-views "r" "MoonReader" "MoonReader" x/project-categories))

(add-to-list 'org-agenda-custom-commands
             '("b" "Book"
               ((tags-todo ":CATEGORY=\"玫瑰的名字\""
                           ((org-agenda-overriding-header "玫瑰的名字")))
                (tags-todo ":CATEGORY=\"Effective Kotlin\""
                           ((org-agenda-overriding-header "Effective Kotlin"))))))

(provide 'x-org-agenda)
;;; x-org-agenda.el ends here
