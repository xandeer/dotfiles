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

(defun forward-day-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-day 1)))

(defun backward-day-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-day 1)))

(defun forward-week-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-week 1)))

(defun backward-week-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-week 1)))

(defun forward-month-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-month 1)))

(defun backward-month-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-month 1)))

(defun forward-year-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-year 1)))

(defun backward-year-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-year 1)))

(with-eval-after-load 'calendar
  (define-key calendar-mode-map (kbd "h") #'calendar-backward-day)
  (define-key calendar-mode-map (kbd "l") #'calendar-forward-day)
  (define-key calendar-mode-map (kbd "j") #'calendar-forward-week)
  (define-key calendar-mode-map (kbd "k") #'calendar-backward-week)
  (define-key calendar-mode-map (kbd "H") #'calendar-backward-month)
  (define-key calendar-mode-map (kbd "L") #'calendar-forward-month)
  (define-key calendar-mode-map (kbd "K") #'calendar-backward-year)
  (define-key calendar-mode-map (kbd "J") #'calendar-forward-year)
  (define-key calendar-mode-map (kbd "g t") #'calendar-goto-today))

(with-eval-after-load 'org
  (define-key org-read-date-minibuffer-local-map (kbd "h") #'backward-day-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "l") #'forward-day-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "j") #'forward-week-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "k") #'backward-week-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "H") #'backward-month-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "L") #'forward-month-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "K") #'backward-year-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "J") #'forward-year-in-calendar)
  (define-key org-read-date-minibuffer-local-map (kbd "n")
              (lambda ()
                (interactive)
                (insert (format-time-string "%R"))))
  (define-key org-read-date-minibuffer-local-map (kbd "q") #'minibuffer-keyboard-quit)
  (define-key org-read-date-minibuffer-local-map (kbd "g") #'minibuffer-keyboard-quit))

(provide 'x-calendar)
;;; x-calendar.el ends here
