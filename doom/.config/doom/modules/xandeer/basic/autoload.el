;;; xandeer/basic/autoload.el -*- lexical-binding: t; -*-
;;;
;;;###autoload
(defun +basic/nth-days-timestamp (n)
  "Return after n days's timestamp like: 2019-05-26 Sun"
  (format-time-string "%F %a"
                      (time-add (current-time) (* 24 3600 n))))

;;;###autoload
(defun +basic/nth-days-inactive (n)
  (concat "[" (+basic/nth-days-timestamp n) "]"))

;;;###autoload
(defun +basic/backup-notes ()
  "Backup notes"
  (interactive)
  (call-process-shell-command "~/Dropbox/notes/.backup.sh" nil nil))
