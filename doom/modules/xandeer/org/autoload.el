;;; xandeer/org/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/archive-done-tasks ()
  "Archive finished tasks."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/+{|DONE|CANCELED}" 'tree))

;;;###autoload
(defun +org/refile-to-top ()
  "Move the current org headline to the top of its section."

  (interactive)
  ;; check if we are at the top level

  (let ((lvl (org-current-level)))
    (cond
     ;; above all headlines so nothing to do
     ((not lvl)
      (message "No headline to move"))
     ((= lvl 1)
      ;; if at top level move current tree to go above first headline
      (org-cut-subtree)
      (beginning-of-buffer)
      ;; test if point is now at the frst headline and if not then move
      ;; to the first headline
      (unless (looking-at-p "*")
        (org-next-visible-heading 1))
      (org-paste-subtree))
     ((> lvl 1)
      ;; if not at top level then get position of headline level above
      ;; current section and refile to that position.
      (let* ((org-reverse-note-order t)
             (pos (save-excursion
                    (outline-up-heading 1)
                    (point)))
             (filename (buffer-file-name))
             (rfloc (list nil filename nil pos)))
        (org-refile nil nil rfloc))))))

;;;###autoload
(defun +org/schedule-tomorrow ()
  "Return scheduled string on tomorrow."
  (format-time-string "SCHEDULED: <%F %a>"
                      (time-add (current-time) (* 24 3600))))
