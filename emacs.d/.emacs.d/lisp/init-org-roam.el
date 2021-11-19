;;; init-org-roam.el --- Settings for org-roam -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq org-roam-v2-ack t)
(require-package 'org-roam)
(require 'org-roam)
(leaf org-roam
  :after gcmh
  :hook
  (org-mode-hook    . xr/enable-valign-when-valign)
  (org-mode-hook    . xr/disable-company-when-nocompany)
  ;; (before-save-hook . xr/roam-update-modified-date)
  :bind
  ("C-c x r" . org-roam-node-random)
  ("C-c r"   . org-roam-capture)
  ("H-f"     . org-roam-node-find)
  ("H-g"     . xr/roam-node-find-other-window)
  ("H-'"     . org-roam-dailies-goto-yesterday)
  ("H-;"     . org-roam-dailies-goto-today)
  ("H-t"     . org-roam-dailies-goto-date)
  (:org-mode-map
   ("H-i"     . org-roam-node-insert)
   ("H-r"     . org-roam-buffer-toggle)
   ("H-," . org-roam-dailies-goto-previous-note)
   ("H-." . org-roam-dailies-goto-next-note))
  (:org-roam-mode-map
   ("H-r" . kill-buffer-and-window))
  :custom
  (org-roam-directory             . org-directory)
  (org-roam-db-gc-threshold       . gc-cons-threshold)
  (org-roam-db-location           . `,(no-littering-expand-var-file-name "roam.db"))
  (org-roam-dailies-directory     . "journal/")
  (org-roam-node-display-template . "${title:48} ${tags:36}")
  ;; (org-roam-node-display-template . "${title:36} ${tags:20} ${backlinkscount:6}")
  (org-roam-capture-templates
   . '(("d" "default" plain "%?"
        :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+TITLE: ${title}\n#+CREATED: <%<%Y-%m-%d %a %R>>\n#+filetags: fleeting\n")
        :unnarrowed t)))
  (org-roam-capture-immediate-template
   . '("d" "default" plain "%?"
       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+TITLE: ${title}\n#+CREATED: <%<%Y-%m-%d %a %R>>\n#+filetags: fleeting\n")
       :immediate-finish t
       :unnarrowed t))
  (org-roam-dailies-capture-templates
   . '(("d" "daily" plain
        "*** %(format-time-string \"<%Y-%m-%d %R> \")%?"
        :prepend t
        :jump-to-captured t
        :target (file+head "%<%Y-%m-%d>.org"
                           ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%B %m-%d>\n#+STARTUP: content\n\n")
        ;; :file-name "%<%Y-%m-%d>.org"
        ;; :head ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%B %m-%d>\n#+STARTUP: content\n\n"
        )))
  :config
  (defun xr/has-roam-tag (tag)
    "Check whether TAG is included in the current file."
    (s-contains? tag (or (cadr (assoc "FILETAGS"
                                 (org-collect-keywords '("filetags"))))
                         "")))

  (defun xr/enable-valign-when-valign ()
    (when (xr/has-roam-tag "valign")
      (valign-mode)))

  (defun xr/disable-company-when-nocompany ()
    (when (xr/has-roam-tag "nocompany")
      (company-mode -1)))

  (defun xr/roam-buffer-p ()
    "Whether the current is in roam directory."
    (and (buffer-file-name)
         (s-ends-with? ".org" (buffer-file-name))
         (s-contains? (expand-file-name org-roam-directory) (buffer-file-name))))

  (defun xr/roam-update-modified-date ()
    (when (xr/roam-buffer-p)
      (org-roam-set-keyword "DATE" (format-time-string "<%Y-%m-%d %a %R>"))))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.25)
                 (window-height . fit-window-to-buffer)))

  (defun xr/refresh-roam-buffer ()
    (when (eq (org-roam-buffer--visibility) 'visible)
      (progn
        (select-window (get-buffer-window org-roam-buffer))
        (org-roam-buffer-refresh))))

  (defun xr/roam-node-find-other-window ()
    (interactive)
    (if (> (count-windows) 1)
        (other-window 1)
      (select-window (split-window-right)))
    (org-roam-node-find))

  (org-roam-db-autosync-enable)
  :advice
  (:after org-roam-buffer-toggle xr/refresh-roam-buffer))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
