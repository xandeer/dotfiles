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

(setq enable-recursive-minibuffers t)
(fset 'yes-or-no-p 'y-or-n-p)

(defun xr/enable-basic-modes ()
  "Enable some basic modes after init."
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (delete-selection-mode)
  (which-function-mode))
(add-hook #'after-init-hook #'xr/enable-basic-modes)

(leaf bookmark
  :after org
  :require t
  :custom (bookmark-default-file . `,(expand-file-name "etc/bookmarks.el" org-directory))
  :hook (after-init-hook . bookmark-maybe-load-default-file))

(leaf all-the-icons
  :straight t
  :custom (inhibit-compacting-font-caches . t))

(leaf files
  :custom (find-file-visit-truename . t))

(leaf explain-pause-mode
  :straight t)

(provide 'init-basic)
;;; init-basic.el ends here
