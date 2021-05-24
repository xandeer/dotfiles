;;; init-org-habit.el --- Org-Habit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf org-habit
  :require t
  :init
  (setq org-habit-graph-column 78)
  ;; (setq org-habit-show-all-today t)
  (setq org-habit-show-habits-only-for-today nil)
  (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t))))

(provide 'init-org-habit)
;;; init-org-habit.el ends here
