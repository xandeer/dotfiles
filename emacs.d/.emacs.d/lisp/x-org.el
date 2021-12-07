;;; x-org.el --- Settings for org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'restclient)
(require-package 'ob-restclient)

(require-package '(org :type built-in))

(setq org-modules
      '(ol-docview
        ol-info
        ;; org-toc
        org-id
        org-habit
        org-inlinetask
        org-protocol
        ol-w3m))
(setq org-directory "~/projects/personal/notes/")
(setq org-startup-folded 'nofold)
(setq org-archive-location "archive/%s_archive::* Archived Tasks")
(setq org-clone-delete-id t)
(setq org-id-locations-file-relative t)
(setq org-id-locations-file (x/expand-note "id-locations.el"))
(setq org-id-extra-files `,(remove (expand-file-name "index.org" org-directory)
                                   (directory-files org-directory 'full (rx ".org" eos))))
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(setq org-image-actual-width '(500))
(setq org-return-follows-link t)
(setq org-attach-id-dir (x/expand-note "attach/"))
(setq org-attach-store-link-p 'attached)
(setq org-list-allow-alphabetical t)
(setq org-edit-src-content-indentation 0)
(setq org-catch-invisible-edits 'error)
;; 代码区域禁用第一层缩进 https://emacs.stackexchange.com/a/18892/16450
(setq org-src-preserve-indentation t)
(setq org-startup-with-inline-images t)
(setq org-cycle-separator-lines 0)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-use-speed-commands t)
(setq org-blank-before-new-entry
      '((heading)
        (plain-list-item . auto)))
(setq org-adapt-indentation nil)
(setq org-confirm-babel-evaluate nil)
(setq org-startup-indented t)
(setq org-cycle-level-faces t)
(setq org-fontify-done-headline t)
(setq org-fontify-todo-headline t)
(setq org-hide-emphasis-markers t)
(setq org-fontify-emphasized-text t)
(setq org-fontify-whole-heading-line t)
(setq org-allow-promoting-top-level-subtree t)
(setq org-fontify-whole-block-delimiter-line t)
(setq org-tag-alist
      '((:startgroup)
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
(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
        ("STYLE_ALL"  . "habit")))
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

(defun x/fill-subtree ()
  "Toggle fill in current subtree."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (unfill-toggle)))

(defun x/wrap-block (beg end type)
  "Wrap block with TYPE between BEG and END."
  (interactive
   (let ((type (read-string "Block type, default[verse], q[quote]: ")))
     (list (region-beginning) (region-end) type)))

  (setq type
        (cond ((s-blank-str? type) "verse")
              ((s-equals? type "q") "quote")
              (t type)))
  (goto-char end)
  (insert (concat "\n#+end_" type))
  (goto-char beg)
  (insert (concat "#+begin_" type "\n")))

(defun x/org-heading-beginning-p ()
  "Whether the point is at beginning of a heading."
  (and (org-at-heading-p) (= (line-beginning-position) (point))))

(defun x/org-goto-heading-beginning ()
  "Goto beginning of the heading."
  (interactive)
  (org-back-to-heading)
  (if (x/org-heading-beginning-p) (org-beginning-of-line)))

(defun x/org-schedule ()
  "Insert an active timestamp at the beginning of the headline."
  (interactive)
  (save-excursion
    (x/org-goto-heading-beginning)
    (setq ts (call-interactively #'org-time-stamp))
    (x/org-goto-heading-beginning)
    (replace-regexp ">\\([^ ]\\)" "> \\1"))
  ts)

(add-hook 'org-mode-hook #'auto-fill-mode)

(with-eval-after-load 'org
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

  (defun x--reset-filling ()
    (let ((paragraph-ending (concat (substring org-element-paragraph-separate 1)
                                    "\\|^\\(#\\+end_.*\\)")))
      (setq-local paragraph-start paragraph-ending)
      (setq-local paragraph-separate paragraph-ending)))
  (advice-add 'org-setup-filling :after #'x--reset-filling)

  (add-to-list 'org-babel-load-languages '(shell      . t))
  (add-to-list 'org-babel-load-languages '(clojure    . t))
  (add-to-list 'org-babel-load-languages '(plantuml   . t))
  (add-to-list 'org-babel-load-languages '(restclient . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  (define-key org-mode-map (kbd "M-p") #'org-previous-visible-heading)
  (define-key org-mode-map (kbd "M-n") #'org-next-visible-heading)
  ;; (define-key org-mode-map (kbd "M-o") #'org-toggle-narrow-to-subtree)
  (define-key org-mode-map (kbd "C-,") #'imenu)
  ;; (define-key org-mode-map (kbd "M-,") #'org-mark-ring-goto)
  (define-key org-mode-map (kbd "M-k") #'org-mark-ring-goto)
  (define-key org-mode-map (kbd "C-c x C-r") #'org-table-recalculate)

  (define-key org-mode-map [remap org-schedule] #'x/org-schedule)

  (global-set-key (kbd "C-c l") #'org-store-link))

(with-eval-after-load 'org
  (require 'org-attach))

(with-eval-after-load 'org-attach
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
                          (expand-file-name "~/Downloads/")))
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

(provide 'x-org)
;;; x-org.el ends here
