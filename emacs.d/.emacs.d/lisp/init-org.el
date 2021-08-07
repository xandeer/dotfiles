;;; init-org.el --- Settings for org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package
 '(valign
   :host github
   :repo "casouri/valign"))

(leaf valign
  :after org
  :hook
  ;; In documents with more than 10 tables, it will be very stuck.
  ;; (org-mode-hook        . valign-mode)
  (org-agenda-mode-hook . valign-mode))

(straight-use-package 'restclient)
(straight-use-package 'ob-restclient)

(straight-use-package '(org :type built-in))

(leaf org
  :hook
  (org-mode-hook . auto-fill-mode)
  :bind
  (:org-mode-map
   ("M-p"       . org-previous-visible-heading)
   ("M-n"       . org-next-visible-heading)
   ("C-c x C-r" . org-table-recalculate))
  :custom
  (org-modules
   . '(ol-docview
       ol-info
       org-toc
       org-id
       org-habit
       org-inlinetask
       org-protocol
       ol-w3m))
  (org-directory          . "~/projects/personal/notes/")
  (org-startup-folded     . 'content)
  (org-archive-location   . "archive/%s_archive::* Archived Tasks")
  (org-image-actual-width . '(500))
  ((org-adapt-indentation
    org-confirm-babel-evaluate) . nil)
  ((org-startup-indented
    org-cycle-level-faces
    org-fontify-done-headline
    org-fontify-todo-headline
    org-hide-emphasis-markers
    org-fontify-emphasized-text
    org-fontify-whole-heading-line
    org-allow-promoting-top-level-subtree
    org-fontify-whole-block-delimiter-line). t)
  (org-tag-alist
   . '((:startgroup)
       ("Mon"       . ?m)
       ("Tue"       . ?t)
       ("Wed"       . ?w)
       ("Thu"       . ?T)
       ("Fri"       . ?f)
       ("Sat"       . ?s)
       ("Sun"       . ?S)
       (:startgroup)
       ("@home"     . ?h)
       ("@office"   . ?o)
       (:endgroup)))
  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (org-global-properties
   . '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
       ("STYLE_ALL"  . "habit")))
  :defer-config
  (add-to-list 'org-babel-load-languages '(shell      . t))
  (add-to-list 'org-babel-load-languages '(plantuml   . t))
  (add-to-list 'org-babel-load-languages '(restclient . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  :config
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

(leaf org
  :config
  (setq org-return-follows-link t)
  (setq org-attach-id-dir (expand-file-name "attach/" org-directory))
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
          ("a" . org-insert-heading-after-current)
          ("A" . org-archive-subtree-default-with-confirmation)
          ("," . org-set-tags-command)

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
          ("z" . org-add-note)

          ("Agenda Views etc")
          ("q" . org-agenda-list)

          ("Misc")
          ("o" . org-open-at-point))))

(provide 'init-org)
;;; init-org.el ends here
