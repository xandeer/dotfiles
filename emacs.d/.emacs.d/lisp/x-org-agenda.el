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
(setq org-agenda-use-time-grid nil)
(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
                            :min-duration 0
                            :max-gap 0
                            :gap-ok-around ("4:00"))))
(setq org-agenda-include-diary t)
(setq diary-file (x/expand-note "gtd/standard-diary"))
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
(setq org-agenda-text-search-extra-files '(agenda-archives))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("n" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("p" "Phone Calls" tags "PHONE"
         ((org-agenda-overriding-header "Phone Calls")
          (org-tags-match-list-sublevels nil)))
        ("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        ("e" "Emacs"
         ((tags-todo "EMACS+CATEGORY=\"One-off\""
                     ((org-agenda-overriding-header "One Off")))
          (tags-todo "EMACS+CATEGORY=\"Embark\""
                     ((org-agenda-overriding-header "Embark")))
          (tags-todo "EMACS+CATEGORY=\"Leaf\""
                     ((org-agenda-overriding-header "Without Leaf")))))
        ("w" "Work"
         ((tags-todo "WORK+CATEGORY=\"Bug\""
                     ((org-agenda-overriding-header "Bug")))
          (tags-todo "WORK+CATEGORY=\"Feat\""
                     ((org-agenda-overriding-header "Feature")))
          (tags-todo "WORK+CATEGORY=\"Refactor\""
                     ((org-agenda-overriding-header "Refactor")))
          (tags-todo "WORK+CATEGORY=\"Chore\""
                     ((org-agenda-overriding-header "Chore")))
          (tags-todo "WORK+CATEGORY=\"Other\""
                     ((org-agenda-overriding-header "Other")))))
        ("x" "Agenda"
         ((tags-todo "-WORK-CANCELLED/!NEXT"
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
          (tags-todo "-WORK-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-WORK-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-WORK-REFILE-CANCELLED-WAITING-HOLD/!"
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
          (tags-todo "-WORK-REFILE-CANCELLED-WAITING-HOLD/!"
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
          (tags-todo "-WORK-CANCELLED+WAITING|HOLD/!"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'bh/skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
          (tags "-WORK-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         nil)))

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

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "M-l") 'x/agenda-toggle-clock-log)
  (define-key org-agenda-mode-map (kbd "q") 'meow-last-buffer)
  (define-key org-agenda-mode-map (kbd "p") 'org-agenda-previous-item)
  (define-key org-agenda-mode-map (kbd "n") 'org-agenda-next-item)
  (define-key org-agenda-mode-map (kbd "T") 'org-agenda-goto-today)
  (define-key org-agenda-mode-map (kbd "i") 'org-agenda-clock-in)
  (define-key org-agenda-mode-map (kbd "o") 'org-agenda-clock-goto))

(defhydra x-hydra-agenda
  (:hit nil :exit t :columns 4)
  "
Agenda View\n"
  ("x" (org-agenda nil "x") "all")
  ("a" org-agenda-list "daily")
  ("e" (org-agenda nil "e") "emacs")
  ("t" (org-agenda nil "t") "todo")
  ("p" (org-agenda nil "p") "phone")
  ("l" org-agenda "list")
  ("w" (org-agenda nil "w") "work"))

(global-set-key (kbd "H-a") #'x-hydra-agenda/body)

;;; valign
(straight-use-package
 '(valign
   :host github
   :repo "casouri/valign"))
;; In documents with more than 10 tables, it will be very stuck.
(add-hook 'org-agenda-mode-hook #'valign-mode)

(provide 'x-org-agenda)
;;; x-org-agenda.el ends here