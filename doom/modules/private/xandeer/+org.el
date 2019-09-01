(setq org-directory "~/Dropbox/notes")
(setq org-agenda-files '("~/Dropbox/notes/gtd.org"))

(after! org
  (setq org-refile-targets '((nil :maxlevel . 5)
                           ("someday.org" :maxlevel . 5)
                           ("journal.org" :maxlevel . 5)
                           ("diary.org" :maxlevel . 5)
                           ("learning.org" :maxlevel . 5)
                           ("reading.org" :maxlevel . 5)
                           ("xmind.org" :maxlevel . 5)
                           ("notes.org" :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5)))

  (map! :map org-mode-map
        :gni [M-return]   (λ! (+org/insert-item-below 1))
        :gni [s-return]   (λ! (+org/insert-item-below 1))
        :gni [M-S-return] (λ! (+org/insert-item-above 1))
        :gni [s-S-return] (λ! (+org/insert-item-above 1)))

  (map! :map evil-org-mode-map
        :i "C-l" (general-predicate-dispatch 'recenter-top-bottom
                   (org-at-table-p) 'org-table-next-field)
        :i "C-h" (general-predicate-dispatch 'help
                   (org-at-table-p) 'org-table-previous-field)
        :i "C-k" (general-predicate-dispatch 'kill-line
                   (org-at-table-p) '+org/table-previous-row)
        :i "C-j" (general-predicate-dispatch 'org-down-element
                   (org-at-table-p) 'org-table-next-row)

        :ni "M-j" #'org-next-visible-heading
        :ni "M-k" #'org-previous-visible-heading)

  (map! :localleader
        :map org-mode-map
        ;; scheduled to tomorrow
        "n" (λ! (org--deadline-or-schedule "" 'scheduled "+1d")))

  (setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DELEGATE(e)" "DONE(d)")
        (sequence "|" "CANCELED(c@/!)")))

  (load! "+org-prettify-src-block"))

(setq org-reverse-note-order t)
(setq org-archive-reversed-order t)

(defun org-custom-scheduled-tomorrow ()
  "Return scheduled string on tomorrow."
  (format-time-string "SCHEDULED: <%F %a>"
                      (time-add (current-time) (* 24 3600))))

(defun org-custom-archive-done-tasks ()
  "Archive finished tasks."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/+{|DONE|CANCELED}" 'tree))

(defun org-custom-to-top ()
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
