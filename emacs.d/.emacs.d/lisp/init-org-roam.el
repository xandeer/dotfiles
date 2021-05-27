;;; init-org-roam.el --- Settings for org-roam -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf org-roam
  :straight t
  :require t
  :after org
  :hook (after-init . org-roam-mode)
  :bind
  ("C-c x M-r" . org-roam-random-note)
  ("C-c r" . org-roam-capture)
  ("C-c x y" . org-roam-dailies-yesterday)
  ("C-c x f" . xr/roam-find-file)
  ("C-c x j" . org-roam-dailies-capture-today)
  (:org-mode-map
   ("C-c x i" . org-roam-insert))
  (:org-roam-mode-map
   ("C-c x r" . org-roam)
   ("C-c x g" . org-roam-graph))
  :config
  ;; (advice-add 'org-roam-capture--capture :after #'xr/deactivate-roam-buffer)
  ;; (xr/auto-toggle-roam-buffer-enable)
  (setq org-roam-directory  org-directory)
  (setq-default org-roam-buffer-width 0.25)
  (setq org-roam-db-location (expand-file-name "~/.cache/roam.db"))
  (setq org-roam-capture-templates
        '(("d" "default" plain #'org-roam-capture--get-point "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+CREATED: <%<%Y-%m-%d %a %R>>\n\n"
           :unnarrowed t)))
  (setq org-roam-capture-immediate-template
        '("d" "default" plain #'org-roam-capture--get-point "%?"
          :file-name "%<%Y%m%d%H%M%S>-${slug}"
          :head "#+TITLE: ${title}\n#+CREATED: <%<%Y-%m-%d %a %R>>\n\n"
          :immediate-finish t
          :unnarrowed t))
  (setq org-roam-dailies-capture-templates
        '(("d" "daily" entry #'org-roam-capture--get-point "* <%<%Y-%m-%d %R>> %?"
           :clock-in t
           :clock-resume t
           :jump-to-captured t
           :file-name "journal/%<%Y-%m-%d-%a>"
           :head ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%Y-%m-%d, %A>\n#+STARTUP: content\n\n")))

  (defun xr/roam-find-file ()
    (interactive)
    (xr/enable-pinyin)
    (call-interactively 'org-roam-find-file)
    (xr/disable-pinyin))

  (defun xr/is-roam-buffer ()
    (and (buffer-file-name) (s-contains? (expand-file-name org-roam-directory) (buffer-file-name))))

                                        ; Override the original, duplicate tags after title to make search easier.
  (defun org-roam--get-title-path-completions ()
    "Return an alist for completion. The car is the displayed title for
completion, and the cdr is the to the file."
    (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta] :from titles
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
