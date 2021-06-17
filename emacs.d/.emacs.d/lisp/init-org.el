;;; init-org.el --- Settings for org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-modules
      '(ol-docview
        ol-info
        ol-elisp-symbol
        org-toc
        org-id
        org-info
        org-jsinfo
        org-habit
        org-inlinetask
        org-protocol
        org-w3m))

(leaf org
  :straight restclient ob-restclient
  :require t
  :hook
  (org-mode-hook . auto-fill-mode)
  :bind
  (:org-mode-map
   ("M-p" . org-previous-visible-heading)
   ("M-n" . org-next-visible-heading)
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
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (scheme . t)
     (plantuml   . t)
     (restclient . t)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)))
  :config
  (setq org-directory "~/projects/personal/notes/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-image-actual-width '(500))
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (setq org-global-properties
        '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
          ("STYLE_ALL" . "habit")))
  (setq org-archive-location "archive/%s_archive::* Archived Tasks")
  (setq org-emphasis-regexp-components ;; markup chinesee without space
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1))
  (add-to-list 'org-emphasis-alist
               ;; set emphasis face
               '("*"
                 ;; :weight bold
                 (:foreground "#f00056")))
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
   ("C-k" . backward-one-week-in-calendar))
  :config
  (setq org-return-follows-link t)
  (setq org-attach-id-dir (expand-file-name ".attach/" org-directory))
  (setq org-attach-store-link-p 'file)
  (setq org-clone-delete-id t)
  (setq org-list-allow-alphabetical t)
  (setq org-edit-src-content-indentation 0)
  (setq org-catch-invisible-edits 'error)
  (setq org-startup-with-inline-images t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-cycle-separator-lines 0)
  (setq org-blank-before-new-entry
        '((heading)
          (plain-list-item . auto)))
  (setq org-use-speed-commands t)
  (setq org-speed-commands-user
        '(("Outline Navigation")
          ("j" org-speed-move-safe 'org-next-visible-heading)
          ("k" org-speed-move-safe 'org-previous-visible-heading)
          ("B" . ignore)
          ("F" . ignore)

          ("Outline Visibility")
          ("C" . ignore)

          ("Outline Structure Editing")
          ("A" . org-insert-heading-after-current)

          ("Clock Commands")
          ("c" . org-clock-goto)
          ("i" . org-clock-in)
          ("o" . org-clock-out)
          ("J" . org-clock-goto)
          ("S" . org-schedule)

          ("Meta Data Editing")
          ("0" . ignore)
          ("1" . ignore)
          ("2" . ignore)
          ("3" . ignore)
          ("," . ignore)
          ("z" . org-add-note)

          ("Agenda Views etc")
          ("q" . org-agenda-list)

          ("Misc")
          ("o" . org-open-at-point))))

(provide 'init-org)
;;; init-org.el ends here
