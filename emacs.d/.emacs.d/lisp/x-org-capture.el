;;; x-org-capture.el --- Settings for org capture -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org-capture)
(global-set-key (kbd "C-c c") #'org-capture)

(defvar x/expenses-history nil)
(defvar org-roam-dailies-capture-templates)
(defvar org-roam-dailies-directory)
(defvar org-roam-directory)
(defvar x/org-today-tag)

(defun x/org-capture-expense-item ()
  (interactive)
  (completing-read "Expense: " x/expenses-history nil nil nil 'x/expenses-history "food"))

(with-eval-after-load 'org-capture
  (unless (boundp 'org-capture-templates)
    (defvar org-capture-templates nil))

  (defun x/cmb2wechat ()
    "Replace cmb with wechat."
    (interactive)
    (x/replace "cmb:6265" "wechat "))

  (x/define-keys org-capture-mode-map
                 '(("C-c w" x/cmb2wechat)
                   ("C-c M-w" x/insert-weather)))

  (defun x/org-capture-place-template-current-window (oldfun &rest args)
    "Place the capture buffer in the current window."
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &optional _action _norecord)
                 (switch-to-buffer buffer-or-name))))
      (apply oldfun args)))

  (advice-remove 'org-capture-place-template #'x/org-capture-place-template-current-window)
  (advice-add 'org-capture-place-template :around #'x/org-capture-place-template-current-window)

  (defvar x/org-capture--frame-name "x/org-capture")
  (defun x/org-capture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame `((name . ,x/org-capture--frame-name)))
    (select-frame-by-name x/org-capture--frame-name)
    (switch-to-buffer "#x/org-capture")
    (condition-case nil
        (org-capture)
      (user-error
       (x/org-capture--cleanup-frame))))

  (defun x/org-capture--cleanup-frame ()
    (when (and (equal x/org-capture--frame-name
                      (frame-parameter nil 'name))
               (not org-capture-is-refiling))
      (delete-frame nil t)))

  (add-hook 'org-capture-after-finalize-hook #'x/org-capture--cleanup-frame)
  (advice-add 'org-capture-refile :after #'x/org-capture--cleanup-frame)

  (defun x/find-phone-location ()
    "Positions point at current month heading in file"
    (goto-char (point-min))

    (if (search-forward (format-time-string "* %Y\n** %B") nil t)
        (progn
          (end-of-line)
          (newline))
      (progn
        (goto-char (point-min))
        (if (search-forward (format-time-string "* %Y") nil t)
            (insert (format-time-string "\n** %B\n"))
          (progn
            (re-search-forward "^$")
            (insert (format-time-string "\n* %Y\n** %B\n")))))))

  (defun x/org-capture--expand-time-template (template)
    "Expand Org capture %<...> time escapes in TEMPLATE."
    (replace-regexp-in-string
     "%<\\([^>]+\\)>"
     (lambda (match)
       (format-time-string (match-string 1 match)))
     template
     t
     t))

  (defun x/org-capture--daily-file-and-head ()
    "Return today's org-roam daily file and head template."
    ;; This intentionally supports the current single dailies template shape:
    ;;   (KEY DESC plain TEMPLATE :target (file+head FILE HEAD) ...)
    ;; Avoiding `org-roam-dailies-capture-today' here prevents nested
    ;; org-capture from mutating the outer capture plist.
    (let* ((template (car org-roam-dailies-capture-templates))
           (target (plist-get (nthcdr 4 template) :target)))
      (pcase target
        (`(file+head ,file ,head)
         (cons (expand-file-name
                (x/org-capture--expand-time-template file)
                (expand-file-name org-roam-dailies-directory org-roam-directory))
               (x/org-capture--expand-time-template head)))
        (_ (error "Unsupported org-roam daily target: %S" target)))))

  (defun x/org-capture--journal-target ()
    "Return today's journal buffer and point without displaying it."
    ;; Do not call `org-roam-dailies-capture-today' here.  It reuses Org
    ;; capture internally and mutates `org-capture-plist', which can make the
    ;; outer journal capture inherit dailies properties such as
    ;; `:jump-to-captured t'.
    (pcase-let ((`(,file . ,head) (x/org-capture--daily-file-and-head)))
      (make-directory (file-name-directory file) t)
      (let ((new-file-p (not (file-exists-p file)))
            (journal-buffer (find-file-noselect file)))
        (with-current-buffer journal-buffer
          (when new-file-p
            (insert head)
            (save-buffer))
          (cons journal-buffer (point-marker))))))

  (defun x/org-capture--goto-journal-location ()
    "Move point to today's journal insertion location in the current buffer."
    (goto-char (point-min))
    (if (search-forward (format-time-string "** %Y") nil t)
        (let ((year-heading-end (line-end-position))
              (year-subtree-end (save-excursion
                                  (forward-line 1)
                                  (if (re-search-forward "^\\*\\* " nil t)
                                      (match-beginning 0)
                                    (point-max)))))
          ;; Insert before the first existing journal entry for the year.  If
          ;; the year heading exists but has no entry yet, insert right below it.
          (goto-char year-heading-end)
          (if (re-search-forward "^\\*\\*\\* " year-subtree-end t)
              (beginning-of-line)
            (newline)))
      (progn
        (goto-char (point-min))
        (if (search-forward (format-time-string "* %B %d") nil t)
            (end-of-line)
          (goto-char (point-min))
          (search-forward "\n\n" nil t)
          (insert (format-time-string "* %B %d")))
        (insert (format-time-string "\n** %Y :%a:"))
        (insert (format "%s:" x/org-today-tag))
        (newline))))

  (defun x/find-journal-location ()
    "Open today's journal."
    (pcase-let ((`(,journal-buffer . ,journal-point)
                 (x/org-capture--journal-target)))
      (set-buffer journal-buffer)
      (goto-char journal-point)
      (set-marker journal-point nil)
      (x/org-capture--goto-journal-location)))

  (defun x/journal-capture-string (text)
    "Append TEXT to today's journal without opening a capture buffer."
    (interactive "sJournal text: ")
    (pcase-let ((`(,journal-buffer . ,journal-point)
                 (x/org-capture--journal-target)))
      (with-current-buffer journal-buffer
        (goto-char journal-point)
        (set-marker journal-point nil)
        (x/org-capture--goto-journal-location)
        (insert "*** " (format-time-string "<%Y-%m-%d %H:%M>") " ")
        (insert text)
        (unless (string-suffix-p "\n" text)
          (newline))
        (save-buffer))))

  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates
               '("p" "Phone call" plain
                 (file+function "gtd/phone.org" x/find-phone-location)
                 "*** PHONE %T %? :PHONE:\n"
                 :prepend t
                 :clock-in t
                 :jump-to-captured t
                 :clock-resume t))

  (add-to-list 'org-capture-templates
               '("j" "Journal entry" plain
                 #'x/find-journal-location
                 "*** %(format-time-string \"<%Y-%m-%d %R> \")%?"
                 :prepend t
                 :jump-to-captured nil))

  (add-to-list 'org-capture-templates
               '("h" "Habit" entry
                 (file "gtd/inbox.org")
                 "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n\n%U\n"))

  (add-to-list 'org-capture-templates
               '("c" "todo" entry
                 (file "gtd/inbox.org")
                 "* TODO %?"
                 :empty-lines-before 1))

  (add-to-list 'org-capture-templates
               '("a" "Anki Inbox" entry
                 (file+headline "anki/inbox.org" "Anki Inbox")
                 "* %?"
                 :jump-to-captured t))

  (defvar x/org-capture-anki-entry-title)
  (add-to-list 'org-capture-templates
               '("A" "Anki Entry" plain
                 #'(lambda () (-> (concat (format-time-string "anki/%Y%m%d%H%M%S-")
                                          (string-replace " " "_"
                                                          x/org-capture-anki-entry-title)
                                          ".org")
                                  (expand-file-name org-directory)
                                  find-file))
                 #'(lambda ()
                     (setq x/org-capture-anki-entry-title (read-string "Anki Entry: "))
                     (let ((title x/org-capture-anki-entry-title))
                       (concat "#+TITLE: " title "\n"
                               "#+CREATED: <%<%Y-%m-%d %a %R>>\n"
                               "#+FILETAGS: anki\n"
                               "\n"
                               "* " title " :" title ":\n"
                               "%?")))
                 :jump-to-captured t))

  (let ((sketches '("el" "kt" "org" "ts")))
    (add-to-list 'org-capture-templates
                 '("s" "Sketch"))
    (dolist (sketch sketches)
      (add-to-list 'org-capture-templates
                   `(,(concat "s" (substring sketch 0 1)) ,(concat sketch " sketch") plain
                     #'(lambda () (-> (concat org-directory "sketch/")
                                      x/string-append-time-suffix
                                      (concat "." ,sketch)
                                      find-file))
                     "%?"
                     :jump-to-captured t))))

  (add-to-list 'org-capture-templates
               '("F" "Monthly Financial" plain
                 (file "hledger-financial.org")
                 (file "capture-templates/monthly-financial.tmpl")
                 :jump-to-captured t
                 :immediate-finish t))

  (add-to-list 'org-capture-templates
               '("f" "Financial" plain
                 (file "hledger-financial.org")
                 (file "capture-templates/financial.tmpl")
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               '("e" "EM Cash" plain
                 (file "hledger-financial.org")
                 (file "capture-templates/em.tmpl")
                 :empty-lines 1)))

(provide 'x-org-capture)
;;; x-org-capture.el ends here
