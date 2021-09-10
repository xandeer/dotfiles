;;; init-eva.el --- init-eva -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(eva
   :host github
   :repo "meedstrom/eva"
   :branch "master"
   :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))

(leaf eva
  :after org-journal
  :straight t
  :init
  (setq eva-cache-dir-path        (xr/expand-note "eva"))
  (setq eva-idle-log-path         (xr/expand-note "eva/idle.tsv"))
  (setq eva-buffer-focus-log-path (xr/expand-note "eva/buffer-focus.tsv"))
  (setq eva-buffer-info-path      (xr/expand-note "eva/buffer-info.tsv"))
  ;; (setq eva-main-ledger-path      "~/l.ledger")
  (setq eva-main-datetree-path    (xr/expand-note "eva/diary.tsv"))
  (setq ess-ask-for-ess-directory nil)
  :config
  (require 'eva-builtin)

  (add-hook 'eva-after-load-vars-hook #'eva-check-dangling-clock)
  (add-hook 'eva-after-load-vars-hook #'eva-check-org-vars)

  (setq eva--idle-secs-fn #'org-mac-idle-seconds)
  (setq eva-items
        (list
         (eva-item-create
          :fn #'eva-query-sleep
          :dataset (xr/expand-note "eva/sleep.tsv")
          :min-hours-wait 5
          :lookup-posted-time t)

         (eva-item-create :fn #'eva-present-org-agenda)

         (eva-item-create
          :fn #'eva-query-mood
          :dataset (xr/expand-note "eva/mood.tsv")
          :min-hours-wait 1)))
  (eva-mode))

(provide 'init-eva)
;;; init-eva.el ends here
