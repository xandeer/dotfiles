;;; xandeer/org/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/archive-tasks-of (type)
  "Archive tasks of the type."
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   (concat "/+{|" (upcase type) "}") 'tree))

;;;###autoload
(defun +org/archive-done-tasks ()
  "Archive done tasks."
  (interactive)
  (+org/archive-tasks-of "DONE"))

;;;###autoload
(defun +org/archive-canceled-tasks ()
  "Archive done tasks."
  (interactive)
  (+org/archive-tasks-of "CANCELED"))

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

;;;###autoload
(defun +org/set-publish-alist ()
  "Set org publish alist."
  (setq pub-base-dir "~/projects/personal/notes/pub/"
        pub-export-dir "~/projects/personal/xandeer.github.io/"
        website-html-head
        "<link rel=\"stylesheet\" href=\"css/notebook.css\" type=\"text/css\"/>
<link rel=\"stylesheet\" type=\"text/css\"
href=\"https://fonts.googleapis.com/css?family=Marck+Script|Pacifico\"/>
<link rel=\"icon\" type=\"image/x-icon\" href=\"favicon.ico\">"
        website-html-preamble
        "<div class=\"nav\"><ul>
<li><a href=\"index.html\">Home</a></li>
<li><a href=\"https://github.com/xandeer\">GitHub</a></li>
</ul><hr></div>"
        website-html-postamble
        "<div class=\"footer\"><hr>Copyright 2019 %a.<br>Last updated %C.<br>
Built with %c.</div>")
  (setq org-publish-project-alist
        `(
          ("org-notes"
           :base-extension "org"
           :base-directory ,pub-base-dir
           :publishing-directory ,pub-export-dir
           :publishing-function org-html-publish-to-html
           :recursive t
           :author "Kevin"
           :email "kkxandeer@gmail.com"
           :section-numbers nil
           :headline-levels 3
           :html-doctype "html5"
           :html-html5-fancy t
           ;; :html-head  ,website-html-head
           :html-head-extra ,website-html-head
           :auto-preamble t
           :html-preamble ,website-html-preamble
           :html-postamble ,website-html-postamble
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Xandeer's Home")
           ("org-static"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ico"
           :base-directory ,pub-base-dir
           :publishing-directory ,pub-export-dir
           :recursive t
           :publishing-function org-publish-attachment
           )
           ("org" :components ("org-notes" "org-static")))))
