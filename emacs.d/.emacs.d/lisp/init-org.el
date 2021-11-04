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
   ("C-,"       . imenu)
   ("C-c x C-r" . org-table-recalculate))
  :custom
  (org-modules
   . '(ol-docview
       ol-info
       ;; org-toc
       org-id
       org-habit
       org-inlinetask
       org-protocol
       ol-w3m))
  (org-directory                    . "~/projects/personal/notes/")
  (org-startup-folded               . 'nofold)
  (org-archive-location             . "archive/%s_archive::* Archived Tasks")
  (org-clone-delete-id              . t)
  (org-id-locations-file-relative   . t)
  (org-id-locations-file            . `,(expand-file-name "id-locations.el" org-directory))
  (org-id-extra-files               . `,(remove (expand-file-name "index.org" org-directory)
                                                (directory-files org-directory 'full (rx ".org" eos))))
  (org-id-link-to-org-use-id        . 'create-if-interactive-and-no-custom-id)
  (org-image-actual-width           . '(500))
  (org-return-follows-link          . t)
  (org-attach-id-dir                . `,(expand-file-name "attach/" org-directory))
  (org-attach-store-link-p          . 'attached)
  (org-list-allow-alphabetical      . t)
  (org-edit-src-content-indentation . 0)
  (org-catch-invisible-edits        . 'error)
  ;; 代码区域禁用第一层缩进 https://emacs.stackexchange.com/a/18892/16450
  (org-src-preserve-indentation     . t)
  (org-startup-with-inline-images   . t)
  (org-cycle-separator-lines        . 0)
  (org-blank-before-new-entry
   . '((heading)
       (plain-list-item . auto)))
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
       (:startgroup)
       ("NightRunning" . ?n)
       ("MorningRunning" . ?r)
       (:endgroup)))
  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (org-global-properties
   . '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
       ("STYLE_ALL"  . "habit")))
  (org-special-ctrl-a/e   . t)
  (org-special-ctrl-k     . t)
  (org-use-speed-commands . t)
  (org-speed-commands-user
   . '(("Outline Navigation")
       ("j" org-speed-move-safe 'org-next-visible-heading)
       ("k" org-speed-move-safe 'org-previous-visible-heading)
       ("B" . ignore)
       ("F" . ignore)

       ("Outline Visibility")
       ("C" . ignore)

       ("Outline Structure Editing")
       ("A" . org-insert-heading-after-current)
       ("a" . org-archive-subtree-default-with-confirmation)
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
       ("o" . org-open-at-point)))
  :defer-config
  (add-to-list 'org-babel-load-languages '(shell      . t))
  (add-to-list 'org-babel-load-languages '(clojure    . t))
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
                   org-emphasis-regexp-components)

  (defun reset-filling ()
    (let ((paragraph-ending (concat (substring org-element-paragraph-separate 1)
                                    "\\|^\\(#\\+end_.*\\)")))
      (setq-local paragraph-start paragraph-ending)
      (setq-local paragraph-separate paragraph-ending)))
  :advice
  (:after org-setup-filling reset-filling))

(leaf org-attach
  :require t
  :config
  (defun org-attach-attach (file &optional visit-dir method)
  "Move/copy/link FILE into the attachment directory of the current outline node.
If VISIT-DIR is non-nil, visit the directory with dired.
METHOD may be `cp', `mv', `ln', `lns' or `url' default taken from
`org-attach-method'."
  (interactive
   (list
    (read-file-name "File to keep as an attachment: "
                    (or (progn
                          (require 'dired-aux)
                          (dired-dwim-target-directory))
                        (expand-file-name "~/Downloads")))
    current-prefix-arg
    nil))
  (setq method (or method org-attach-method))
  (let ((basename (file-name-nondirectory file)))
    (let* ((attach-dir (org-attach-dir 'get-create))
           (attach-file (expand-file-name basename attach-dir)))
      (cond
       ((eq method 'mv) (rename-file file attach-file))
       ((eq method 'cp) (copy-file file attach-file))
       ((eq method 'ln) (add-name-to-file file attach-file))
       ((eq method 'lns) (make-symbolic-link file attach-file))
       ((eq method 'url) (url-copy-file file attach-file)))
      (run-hook-with-args 'org-attach-after-change-hook attach-dir)
      (org-attach-tag)
      (cond ((eq org-attach-store-link-p 'attached)
	     (push (list (concat "attachment:" (file-name-nondirectory attach-file))
			 (file-name-nondirectory attach-file))
		   org-stored-links))
            ((eq org-attach-store-link-p t)
             (push (list (concat "file:" file)
			 (file-name-nondirectory file))
		   org-stored-links))
	    ((eq org-attach-store-link-p 'file)
	     (push (list (concat "file:" attach-file)
			 (file-name-nondirectory attach-file))
		   org-stored-links)))
      (if visit-dir
          (dired attach-dir)
        (message "File %S is now an attachment" basename))))))

(provide 'init-org)
;;; init-org.el ends here
