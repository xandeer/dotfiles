;;; init-org-roam.el --- Settings for org-roam -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf org-roam
  :straight t
  :after org
  :hook
  (after-init-hook . org-roam-mode)
  (org-mode-hook   . xr/enable-valign-when-valign)
  :bind
  ("C-c x r" . org-roam-random-note)
  ("C-c r"   . org-roam-capture)
  ("C-c x y" . org-roam-dailies-find-yesterday)
  ("C-c x f" . org-roam-find-file)
  ("H-f"     . org-roam-find-file)
  (:org-mode-map
   ("H-i"     . org-roam-insert)
   ("C-c x i" . org-roam-insert))
  (:org-roam-mode-map
   ("C-c x M-r" . org-roam))
  :custom
  (org-roam-directory    . org-directory)
  (org-roam-buffer-width . 0.25)
  (org-roam-db-location  . `,(no-littering-expand-var-file-name "roam.db"))
  (org-roam-capture-templates
   . '(("d" "default" plain #'org-roam-capture--get-point "%?"
        :file-name "%<%Y%m%d%H%M%S>-${slug}"
        :head "#+TITLE: ${title}\n#+CREATED: <%<%Y-%m-%d %a %R>>\n#+ROAM_TAGS: fleeting\n"
        :unnarrowed t)))
  (org-roam-capture-immediate-template
   . '("d" "default" plain #'org-roam-capture--get-point "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+TITLE: ${title}\n#+CREATED: <%<%Y-%m-%d %a %R>>\n#+ROAM_TAGS: fleeting\n"
       :immediate-finish t
       :unnarrowed t))
  (org-roam-dailies-capture-templates
   . `(("d" "daily" plain
        (function xr/find-journal-location)
        "*** %(format-time-string org-journal-time-format)%?"
        :prepend t
        :clock-in t
        :clock-resume t
        :jump-to-captured t
        :file-name "journal/%<%Y-%m-%d>"
        :head ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%B %m-%d>\n#+STARTUP: content\n\n")))
  :config
  (defun xr/has-roam-tag (tag &optional file)
    "Check whether TAG is included in the FILE."
    (-contains? (org-roam--extract-tags file) tag))

  (defun xr/enable-valign-when-valign ()
    (when (xr/has-roam-tag "valign")
      (valign-mode)))

  (defun xr/roam-buffer-p ()
    "Whether the current is in roam directory."
    (and (buffer-file-name) (s-contains? (expand-file-name org-roam-directory) (buffer-file-name))))

  ;; Override the original, duplicate tags after title to make search easier.
  (defun org-roam--get-title-path-completions ()
    "Return an alist for completion. The car is the displayed title for
completion, and the cdr is the to the file."
    (let* ((rows (org-roam-db-query
                  [:select [files:file titles:title tags:tags files:meta]
                           :from titles
                           :left :join tags
                           :on (= titles:file tags:file)
                           :left :join files
                           :on (= titles:file files:file)]))
           completions)
      (seq-sort-by (lambda (x)
                     (plist-get (nth 3 x) :mtime))
                   #'time-less-p
                   rows)
      (dolist (row rows completions)
        (pcase-let ((`(,file-path ,title ,tags) row))
          (let ((k (concat
                    (when tags
                      (format "(%s) " (s-join org-roam-tag-separator tags)))
                    title
                    (when tags
                      (format " (%s)" (s-join org-roam-tag-separator tags)))))
                (v (list :path file-path :title title)))
            (push (cons k v) completions)))))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
