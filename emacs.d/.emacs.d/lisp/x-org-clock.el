;;; x-org-clock.el --- Settings for org clock -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'titlecase)

(setq org-clock-mode-line-total 'current)
(setq org-clock-history-length 23)
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
(defun x/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects."
  (when (and (not (and (boundp 'org-capture-mode) org-capture-mode))
             (member (org-get-todo-state) '("TODO" "WAITING")))
    "NEXT"))
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'x/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
(setq org-time-stamp-rounding-minutes (quote (1 1)))
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

(defun x/clock-in-task-with-id (id)
  "Clock in the particular task with `ID'."
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in '(16))))

(defvar x/organization-task-id "78C5A814-5215-47D0-AC09-6522CBCBA516")
(defun x/clock-in-organization-task ()
  "Clock in the organization task."
  (interactive)
  (x/clock-in-task-with-id x/organization-task-id))

(defvar x/reading-task-id "238104E0-22BC-4F0F-A0B3-87DA326828EF")
(defun x/clock-in-reading-task ()
  "Clock in the reading task."
  (interactive)
  (x/clock-in-task-with-id x/reading-task-id))

(defvar x/noting-task-id "A1400159-820B-4D93-A14A-E064F7C542C7")
(defun x/clock-in-noting-task ()
  "Clock in the noting task."
  (interactive)
  (x/clock-in-task-with-id x/noting-task-id))

(defvar x/working-task-id "20C42DE8-87B3-4F65-BD1F-988BCE4AD62B")
(defun x/clock-in-working-task ()
  "Clock in the working task."
  (interactive)
  (x/clock-in-task-with-id x/working-task-id))

(defvar x--clock-timer nil)

(defun x--clock-cancel ()
  (when (timerp x--clock-timer)
    (setq x--clock-timer (cancel-timer x--clock-timer))))

(defun x--clock-out ()
  (if (y-or-n-p "It's time to take a rest? ")
      (org-clock-out)
    (x--clock-in 3)))

(defun x--clock-in (&optional minutes)
  "Max MINUTES while clock in."
  (unless minutes
    (setq minutes 25))

  (x--clock-cancel)
  (setq x--clock-timer
        (run-with-timer (* minutes 60) nil #'x--clock-out)))

(add-hook 'org-clock-out-hook #'x--clock-cancel)
(add-hook 'org-clock-in-hook #'x--clock-in)
(add-hook 'org-clock-out-hook #'bh/clock-out-maybe 'append)

(defcustom x/org-sync-min-clock-duration 1
  "Minimum duration in minutes for syncing org-clock with calendar."
  :group 'x/org
  :type '(restricted-sexp :match (lambda (value)
                                   (and (integerp value) (>= value 0)))
                          :doc "A non-negative integer."))

(defcustom x/org-sync-calendar "Org"
  "The default calendar name for x/org synchronization."
  :group 'x/org
  :type 'string)

(defun x/org-clock-sync-to-calendar ()
  (when (and org-clock-start-time
             (> (/ (float-time (time-since org-clock-start-time)) 60)
                x/org-sync-min-clock-duration))
    (let ((heading (or org-clock-heading "No title"))
          (exit-code))
      (setq exit-code
            (shell-command
             (format
              "osascript -e 'tell application \"Calendar\" to tell calendar \"%s\" to make new event with properties {summary:\"%s\", start date:date \"%s\", end date:date \"%s\"}'"
              x/org-sync-calendar
              (titlecase--string (string-trim-left heading "<.*> ") titlecase-style)
              (time-to-calendar-string org-clock-start-time)
              (time-to-calendar-string))))
      (when (not (eq exit-code 0))
        (warn "Failed to sync to Calendar! Command exited with code %d" exit-code)))))

(add-hook 'org-clock-out-hook #'x/org-clock-sync-to-calendar)

(defun time-to-calendar-string (&optional time)
  "Convert `TIME' to string like: January 4, 2025 12:00 PM; January 4, 2025 11:30 AM."
  (let ((time (or time (current-time)))
        (format "%B %d, %Y %I:%M %p"))
    (format-time-string format time)))

(defun x/org-done-current ()
  (interactive)
  (if (org-clocking-p)
      (with-current-buffer (marker-buffer org-clock-marker)
        (let ((m org-clock-marker))
          ;; (pop-to-buffer-same-window (marker-buffer m))
          (if (or (< m (point-min)) (> m (point-max))) (widen))
          (goto-char m)
          (org-show-entry)
          (org-todo 'done)))
    (message "No running clock.")))

(provide 'x-org-clock)
;;; x-org-clock.el ends here
