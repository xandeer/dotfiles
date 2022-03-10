;;; x-eva.el --- x-eva -*- lexical-binding: t -*-
;;; Commentary:

;;; How to modify mood alist?
;; 1. (setq eva-mood-alist '(("phone" . "5") ("watching" . "5") ("review" . "5") ("learning" . "5")   ("reading" . "5") ("emacs" . "5") ("donut" . "5") ("normal" . "5")))
;; 2. stop (eva-mode -1)
;; 3. start (eva-mode)
;; 4. could look at (find-file (expand-file-name "memory.tsv" eva-cache-dir-path))

;;; Code:

(require-package
 '(eva
   :host github
   :repo "meedstrom/eva"
   :branch "master"
   :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))

(setq eva-cache-dir-path (no-littering-expand-var-file-name "eva"))
(setq eva-idle-log-path (expand-file-name "idle.tsv" eva-cache-dir-path))
(setq eva-buffer-info-path (expand-file-name "buffer-info.tsv" eva-cache-dir-path))
(setq eva-buffer-focus-log-path (expand-file-name "buffer-focus.tsv" eva-cache-dir-path))
(setq ess-ask-for-ess-directory nil)

(with-eval-after-load 'eva
  (require 'eva-builtin)
  (setq eva--idle-secs-fn #'org-mac-idle-seconds)
  (setq eva-items
        (list
         (eva-item-create
          :fn #'eva-query-sleep
          :dataset (x/expand-note "eva/sleep.tsv")
          :min-hours-wait 16
          :lookup-posted-time t)

         (eva-item-create
          :fn #'eva-query-mood
          :dataset (x/expand-note "eva/mood.tsv")
          :min-hours-wait 1)))
  (advice-add 'eva-query-sleep :before #'eva-set-date-today))

(provide 'x-eva)
;;; x-eva.el ends here
