;;; x-basic.el --- Basic -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Basic settings
(setq-default
 word-wrap                           t
 fill-column                         76
 tooltip-delay                       1.5
 line-spacing                        0.2
 truncate-lines                      nil
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
(setq large-file-warning-threshold 30000000)
(setq display-line-numbers-type nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq sentence-end-double-space nil)

;; prepend , in it
;; (setq sentence-end-base "[.?!…‽][]\"'”’)}»›,]*")
;; ，；
(setq sentence-end-without-space "，；。．：、？！「」『』,.?!…\"'——")

;;; Indentations
(setq-default tab-width 2)
(setq standard-indent 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq js3-indent-level 2)
(setq typescript-indent-level 2)
(setq kotlin-tab-width 2)
(setq sh-basic-offset 2)
;; (setq lisp-indent-offset 2)

;;; Some basic minor modes.
(defun x/enable-basic-modes ()
  "Enable some basic modes after init."
  (global-so-long-mode)
  (global-subword-mode)
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (delete-selection-mode)
  (which-function-mode))
(x/append-init-hook #'x/enable-basic-modes)

;; (autoload 'filesets-open "filesets" nil t)
;; (autoload 'filesets-add-buffer "filesets" nil t)
;; (add-hook 'kill-emacs-hook #'filesets-save-config)

;;; Icons
(setq inhibit-compacting-font-caches t)

;;; Bookmark
(require 'bookmark)
(setq bookmark-default-file (x/expand-note "etc/bookmarks.el"))
(setq bookmark-save-flag 1)
(x/append-init-hook #'bookmark-maybe-load-default-file)

(x/package-use '(ultra-scroll . "jdtsmith/ultra-scroll"))
(setq scroll-conservatively 101         ; important!
      scroll-margin 0)
(ultra-scroll-mode 1)

(provide 'x-basic)
;;; x-basic.el ends here
