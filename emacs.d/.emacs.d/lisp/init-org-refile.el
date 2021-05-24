;;; init-org-refile.el --- Settings for org refile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

; Exclude DONE state tasks from refile targets
(defun xr/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'xr/verify-refile-target)


(provide 'init-org-refile)
;;; init-org-refile.el ends here
