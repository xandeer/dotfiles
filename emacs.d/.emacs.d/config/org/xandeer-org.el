;;; xandeer-org.el --- Xandeer's emacs.d init org file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's emacs.d init org file.

;;; Code:

(straight-use-package 'org-plus-contrib)
(leaf org
  :require t
  :hook
  (org-mode-hook . auto-fill-mode)
  :bind
  (:org-mode-map
   ("C-c x C-r" . org-table-recalculate))
  :custom
  ;; org appearance
  ((org-allow-promoting-top-level-subtree
    org-cycle-level-faces
    org-fontify-done-headline
    org-fontify-emphasized-text
    org-fontify-todo-headline
    org-fontify-whole-block-delimiter-line
    org-fontify-whole-heading-line
    org-hide-emphasis-markers)
   . t)
  (org-startup-folded . 'content)
  (org-confirm-babel-evaluate . nil)
  :defer-config
  ;; (xandeer/add-company-backend 'org-mode 'company-tabnine)
  ;; --------
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (scheme . t)
     (plantuml   . t)
     (restclient . t)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)))
  :config
  (setq org-version "9.5")
  (setq org-directory "~/projects/personal/notes/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-image-actual-width '(500))
  (setq org-adapt-indentation nil)
  (setq org-emphasis-regexp-components ;; markup chinesee without space
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1))
  (add-to-list 'org-emphasis-alist
               ;; set bold face
               '("*" (:foreground "#f00056" :weight bold)))
  ;; set emphasis support 16 lines
  (setcar (nthcdr 4 org-emphasis-regexp-components) 16)
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components))
(defun reset-filling ()
  (let ((paragraph-ending (concat (substring org-element-paragraph-separate 1)
                                  "\\|^\\(#\\+end_.*\\)")))
    (setq-local paragraph-start paragraph-ending)
    (setq-local paragraph-separate paragraph-ending)))
(advice-add 'org-setup-filling :after #'reset-filling)

(leaf org-agenda
  :require t
  :bind
  ("C-c x a" . org-agenda-list)
  (:org-agenda-mode-map
   ("p" . org-agenda-previous-item)
   ("n" . org-agenda-next-item)
   ("T" . org-agenda-goto-today)
   ("i" . org-agenda-clock-in)
   ("o" . org-agenda-clock-goto))
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DELEGATE(e)" "DONE(d)")
                            (sequence "|" "CANCELED(c@/!)" "SHELVE(s)")))
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)
  (setq org-clock-clocked-in-display 'both)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-time-grid '((daily today require-timed)
                               (600 900 1200 1500 1800 2100)
                               "..........."
                               "- - - - - - - - - - - - - - - - - - - - - - - - - - -"))
  (setq org-agenda-include-diary t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up)))
  (setq calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (setq calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  ;; location
  (setq calendar-longitude 113.9442)
  (setq calendar-latitude 22.5395)

  ;; Copied from https://emacs-china.org/t/05-org-as/12092/4
  ;; 日出而作, 日落而息
  (defun xandeer/diary-sunrise ()
    (let ((dss (diary-sunrise-sunset)))
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (while (re-search-forward " ([^)]*)" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (search-forward ",")
        (buffer-substring (point-min) (match-beginning 0)))))

  (defun xandeer/diary-sunset ()
    (let ((dss (diary-sunrise-sunset))
          start end)
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (while (re-search-forward " ([^)]*)" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (search-forward ", ")
        (setq start (match-end 0))
        (search-forward " at")
        (setq end (match-beginning 0))
        (goto-char start)
        (capitalize-word 1)
        (buffer-substring start end)))))

(defun forward-one-day-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-day 1)))

(defun backward-one-day-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-day 1)))

(defun forward-one-week-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-forward-week 1)))

(defun backward-one-week-in-calendar ()
  (interactive)
  (org-eval-in-calendar '(calendar-backward-week 1)))

(leaf org
  :bind
  (:org-read-date-minibuffer-local-map
   ("C-h" . backward-one-day-in-calendar)
   ("C-l" . forward-one-day-in-calendar)
   ("C-j" . forward-one-week-in-calendar)
   ("C-k" . backward-one-week-in-calendar)))

(leaf org-capture
  :require t
  :bind
  ("C-c x x" . org-capture)
  :config
  (unless (boundp 'org-capture-templates)
    (defvar org-capture-templates nil))

  (setq org-capture-templates nil)

  (add-to-list 'org-capture-templates
               '("s" "Stand" plain
                 (file "hledger-habit.org")
                 (file "capture-templates/habit-stand.tmpl")
                 :immediate-finish t
                 :jump-to-captured t
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("i" "Sit" plain
                 (file "hledger-habit.org")
                 (file "capture-templates/habit-sit.tmpl")
                 :immediate-finish t
                 :jump-to-captured t
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("t" "Sit up" plain
                 (file "hledger-habit.org")
                 (file "capture-templates/habit-sit-up.tmpl")
                 :immediate-finish t
                 :jump-to-captured t
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("p" "Push up" plain
                 (file "hledger-habit.org")
                 (file "capture-templates/habit-push-up.tmpl")
                 :immediate-finish t
                 :jump-to-captured t
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("f" "Food" plain
                 (file "hledger-financial.org")
                 (file "capture-templates/financial-food.tmpl")
                 :immediate-finish t
                 :jump-to-captured t
                 :empty-lines 1)))

(straight-use-package 'deft)
(leaf deft
  :after org
  :bind
  ("C-c x d" . deft)
  :config
  (setq deft-directory org-directory)
  (setq deft-extensions '("org"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t))

(straight-use-package 'org-journal)
(leaf org-journal
  :require t
  :after org
  :init
  (setq org-journal-file-format "%Y-%m-%d-%a.org")
  ;; I don't know why this doesn't work.
  (setq org-journal-follow-mode t)
  :bind
  ("C-c x j" . org-journal-new-entry)
  ("C-c x t" . org-journal-open-current-journal-file)
  (:org-mode-map
   ("C-c x C-b" . org-journal-open-pervious-entry)
   ("C-c x C-f" . org-journal-open-next-entry)
   ("C-c x C-s" . org-journal-schedule-view))
  :config
  (setq org-journal-file-format "%Y-%m-%d-%a.org")
  (setq org-journal-dir (concat org-directory "journal/"))
  (setq diary-file (concat org-journal-dir "standard-diary"))
  (setq org-agenda-files `(,(concat org-journal-dir)))
  (setq org-journal-file-header ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %Y-%m-%d, %A\n#+STARTUP: content\n\n")
  (setq org-journal-time-format "<%Y-%m-%d %R> ")
  (add-to-list 'auto-mode-alist '("notes/journal/.+\\.org\\'" . org-journal-mode)))

(straight-use-package 'org-roam)
(leaf org-roam
  :require t
  :after org
  :hook
  (after-init . org-roam-mode)
  :bind
  ("C-c x y" . org-roam-dailies-yesterday)
  ("C-c x f" . org-roam-find-file)
  ("C-c x c" . org-roam-capture)
  (:org-mode-map
   ("C-c x i" . org-roam-insert)
   ("C-c x I" . org-roam-insert-immediate))
  (:org-roam-mode-map
   ("C-c x r" . org-roam)
   ("C-c x b" . org-roam-switch-to-buffer)
   ("C-c x g" . org-roam-graph))
  :config
  ;; (advice-add 'org-roam-capture--capture :after #'xandeer/deactivate-roam-buffer)
  ;; (xandeer/auto-toggle-roam-buffer-enable)
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
   '(("d" "daily" plain (function org-roam-capture--get-point) ""
      :immediate-finish t
      :file-name "journal/%<%Y-%m-%d-%a>"
      :head ":PROPERTIES:\n:CATEGORY: Journal\n:END:\n#+TITLE: %<%Y-%m-%d, %A>\n#+STARTUP: content\n\n* %<%A, %x>")))

  (defun xandeer/is-roam-buffer ()
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

(straight-register-package
   '(org-roam-server :host github
            :repo "org-roam/org-roam-server"
            :branch "master"
            :files (:defaults "README.md" "assets" "*.el" "index.html")))

  (straight-use-package 'org-roam-server)
  (leaf org-roam-server
    :require t
    :config
    (setq org-roam-server-host "0.0.0.0")
    (setq org-roam-server-port 8787)
    (setq org-roam-server-authenticate nil)
    (setq org-roam-server-export-inline-images t))

(provide 'xandeer-org)
;;; xandeer-org.el ends here
