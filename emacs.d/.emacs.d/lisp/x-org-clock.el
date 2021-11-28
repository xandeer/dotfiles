;;; x-org-clock.el --- Settings for org clock -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-clock-mode-line-total 'current)
(setq org-clock-history-length 23)
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
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

(setq bh/organization-task-id "78C5A814-5215-47D0-AC09-6522CBCBA516")

(defvar x--clock-timer nil)

(defun x--clock-cancel ()
  (when (timerp x--clock-timer)
    (setq x--clock-timer (cancel-timer x--clock-timer))))

(defun x--clock-out ()
  (if (y-or-n-p "It's time to take a rest? ")
      (org-clock-out)
    (x--clock-in "3")))

(defun x--clock-in (&optional minutes)
  "Max MINUTES while clock in."
  (when (s-blank-str? minutes)
    (setq minutes (read-from-minibuffer "Set a timer to stop(with siri?), default[52], j[45], k[25]: ")))
  (setq minutes (cond ((s-blank-str? minutes) 52)
                      ((s-equals? minutes "j") 45)
                      ((s-equals? minutes "k") 25)
                      (t (string-to-number minutes))))
  (x--clock-cancel)
  (when (> minutes 0)
    (setq x--clock-timer
          (run-with-timer (* minutes 60) nil #'x--clock-out))))

(add-hook 'org-clock-out-hook 'x--clock-cancel)
(add-hook 'org-clock-in-hook 'x--clock-in)
(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

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

(defhydra x-hydra-clock (:exit t)
  "
Org Clock\n\n"
  ("d" x/org-done-current "done current")
  ("g" org-clock-goto "goto current")
  ("i" org-clock-in-last "in last")
  ("o" org-clock-out "out"))
(global-set-key (kbd "H-c") #'x-hydra-clock/body)

(provide 'x-org-clock)
;;; x-org-clock.el ends here
