;;; init-calendar.el --- init-calendar -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf cal-china-x
  :straight t
  :require t
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays)))

(provide 'init-calendar)
;;; init-calendar.el ends here