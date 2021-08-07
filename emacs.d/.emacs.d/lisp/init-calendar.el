;;; init-calendar.el --- init-calendar -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf cal-china-x
  :straight t
  :require t)

(leaf holidays
  :after cal-china-x
  :custom
  (calendar-holidays .  cal-china-x-chinese-holidays))
;; (cal-china-x-important-holidays . cal-china-x-chinese-holidays)
;; (cal-china-x-general-holidays . '((holiday-lunar 1 15 "元宵节")))
;; . (append cal-china-x-important-holidays
;; cal-china-x-general-holidays))

(leaf calendar
  :bind
  (:calendar-mode-map
   ("h" . calendar-backward-day)
   ("l" . calendar-forward-day)
   ("j" . calendar-forward-week)
   ("k" . calendar-backward-week)
   ("H" . calendar-backward-month)
   ("L" . calendar-forward-month)
   ("K" . calendar-backward-year)
   ("J" . calendar-forward-year)
   ("g t" . calendar-goto-today))
  (:org-read-date-minibuffer-local-map
   :package org
   ("h" . backward-one-day-in-calendar)
   ("l" . forward-one-day-in-calendar)
   ("j" . forward-one-week-in-calendar)
   ("k" . backward-one-week-in-calendar)
   ("g t" . goto-today-in-calendar))
  :config
  (defun goto-today-in-calendar ()
    (interactive)
    (org-eval-in-calendar '(calendar-goto-today)))

  (defun forward-one-day-in-calendar ()
    (interactive)
    (org-eval-in-calendar '(calendar-forward-day 1)))

  (defun backward-one-day-in-calendar ()
    (interactive)
    (org-eval-in-calendar '(calendar-backward-day 1)))

  (defun forward-one-week-in-calendar ()
    (interactive)
    (org-eval-in-calendar '(calendar-forward-week 1)))

  (defun backward-one-week-in-calendar ()
    (interactive)
    (org-eval-in-calendar '(calendar-backward-week 1))))

(provide 'init-calendar)
;;; init-calendar.el ends here
