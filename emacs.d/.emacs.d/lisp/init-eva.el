;;; init-eva.el --- init-eva -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package
 '(eva
   :host github
   :repo "meedstrom/eva"
   :branch "master"
   :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))

(leaf eva
  :custom
  `(eva-cache-dir-path        . ,(no-littering-expand-var-file-name "eva"))
  `(eva-idle-log-path         . ,(no-littering-expand-var-file-name "eva/idle.tsv"))
  `(eva-buffer-focus-log-path . ,(no-littering-expand-var-file-name "eva/buffer-focus.tsv"))
  `(eva-buffer-info-path      . ,(no-littering-expand-var-file-name "eva/buffer-info.tsv"))
  (ess-ask-for-ess-directory . nil)
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

         (eva-item-create
          :fn #'eva-query-mood
          :dataset (xr/expand-note "eva/mood.tsv")
          :min-hours-wait 1)))
  (eva-mode))

(provide 'init-eva)
;;; init-eva.el ends here
