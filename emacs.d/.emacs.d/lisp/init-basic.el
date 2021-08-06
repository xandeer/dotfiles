;;; init-basic.el --- Basic -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
 visual-fill-column-width       80
 fill-column 76
 word-wrap   t
 tab-width   2
 standard-indent  2
 tooltip-delay    1.5
 case-fold-search      t
 indent-tabs-mode      nil
 make-backup-files     nil
 ;; require-final-newline nil
 bookmark-default-file (no-littering-expand-var-file-name "bookmarks.el")
 save-interprogram-paste-before-kill t
 set-mark-command-repeat-pop    t
 tab-always-indent              t
 truncate-lines                 nil)

(setq enable-recursive-minibuffers t)
(fset 'yes-or-no-p 'y-or-n-p)

(defun xr/enable-basic-modes ()
  "Enable some basic modes after init."
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (delete-selection-mode)
  (which-function-mode))
(add-hook #'after-init-hook #'xr/enable-basic-modes)

(leaf all-the-icons
  :straight t
  :custom (inhibit-compacting-font-caches . t))

(provide 'init-basic)
;;; init-basic.el ends here
