;;; private/xandeer/+utils.el -*- lexical-binding: t; -*-
;;;

(defun xandeer/insert-present-timestamp ()
  "Insert the present timestamp like: [2019-05-29 Wed 11:23]"
  (interactive)
  (insert (format-time-string "[%F %a %R]" (current-time))))

(defun xandeer/nth-days-timestamp (n)
  "Return after n days's timestamp like: 2019-05-26 Sun"
  (format-time-string "%F %a"
                      (time-add (current-time) (* 24 3600 n))))

(defun xandeer/nth-days-inactive (n)
  (concat "[" (xandeer/nth-days-timestamp n) "]"))

(defun xandeer/update ()
  "Pull the private config and reload it"
  (interactive)
  (call-process-shell-command "cd ~/.config/doom ; git pull" nil nil)
  (doom/reload))

(defun xandeer/backup-notes ()
  "Backup notes"
  (interactive)
  (call-process-shell-command "~/Dropbox/notes/.backup.sh" nil nil))
