;;; init-basic.el --- Basic -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
 tab-width                           2
 word-wrap                           t
 fill-column                         76
 tooltip-delay                       1.5
 truncate-lines                      nil
 standard-indent                     2
 case-fold-search                    t
 indent-tabs-mode                    nil
 create-lockfiles                    nil
 make-backup-files                   nil
 visual-fill-column-width            80
 set-mark-command-repeat-pop         t
 ;; require-final-newline nil
 save-interprogram-paste-before-kill t)

(setq find-file-visit-truename t)
(setq enable-recursive-minibuffers nil)
(setq use-dialog-box nil)
(fset 'yes-or-no-p 'y-or-n-p)

(defun xr/enable-basic-modes ()
  "Enable some basic modes after init."
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (delete-selection-mode)
  (which-function-mode))
(add-hook 'after-init-hook 'xr/enable-basic-modes)

(require-package 'all-the-icons)
(setq inhibit-compacting-font-caches t)
;; TODO: for what?
(straight-use-package 'explain-pause-mode)

(require 'bookmark)
(with-eval-after-load 'org
  (setq bookmark-default-file (xr/expand-note "etc/bookmarks.el"))
  (add-hook 'after-init-hook 'bookmark-maybe-load-default-file))

(provide 'init-basic)
;;; init-basic.el ends here
