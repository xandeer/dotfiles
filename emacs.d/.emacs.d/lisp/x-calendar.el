;;; x-calendar.el --- x-calendar -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'cal-china-x)
(with-eval-after-load 'cal-china-x
  (require 'holidays)
  (setq calendar-holidays cal-china-x-chinese-holidays))

;; (cal-china-x-important-holidays . cal-china-x-chinese-holidays)
;; (cal-china-x-general-holidays . '((holiday-lunar 1 15 "元宵节")))
;; . (append cal-china-x-important-holidays
;; cal-china-x-general-holidays))

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
  (org-eval-in-calendar '(calendar-backward-week 1)))

(with-eval-after-load 'calendar
  (define-key calendar-mode-map (kbd "h") 'calendar-backward-day)
  (define-key calendar-mode-map (kbd "l") 'calendar-forward-day)
  (define-key calendar-mode-map (kbd "j") 'calendar-forward-week)
  (define-key calendar-mode-map (kbd "k") 'calendar-backward-week)
  (define-key calendar-mode-map (kbd "H") 'calendar-backward-month)
  (define-key calendar-mode-map (kbd "L") 'calendar-forward-month)
  (define-key calendar-mode-map (kbd "K") 'calendar-backward-year)
  (define-key calendar-mode-map (kbd "J") 'calendar-forward-year)
  (define-key calendar-mode-map (kbd "g t") 'calendar-goto-today))

(with-eval-after-load 'org
  (define-key org-read-date-minibuffer-local-map (kbd "h") 'backward-one-day-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "l") 'forward-one-day-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "j") 'forward-one-week-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "k") 'backward-one-week-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "g t") 'goto-today-in-calendar))

(provide 'x-calendar)
;;; x-calendar.el ends here
