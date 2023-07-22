;;; x-org-capture.el --- Settings for org capture -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org-capture)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "H-.") #'org-capture)

(defvar x/expenses-history nil)

(defun x/org-capture-expense-item ()
  (interactive)
  (completing-read "Expense: " x/expenses-history nil nil nil 'x/expenses-history "food"))

(with-eval-after-load 'org-capture
  (unless (boundp 'org-capture-templates)
    (defvar org-capture-templates nil))

  (defun x/find-phone-location ()
    "Positions point at current month heading in file"
    (beginning-of-buffer)

    (if (search-forward (format-time-string "* %Y\n** %B") nil t)
        (progn
          (end-of-line)
          (newline))
      (progn
        (beginning-of-buffer)
        (if (search-forward (format-time-string "* %Y") nil t)
            (insert (format-time-string "\n** %B\n"))
          (progn
            (re-search-forward "^$")
            (insert (format-time-string "\n* %Y\n** %B\n")))))))

  (defun x/find-journal-location ()
    "Open today's journal."
    (org-journal-new-entry t)
    (beginning-of-buffer)
    (if (search-forward (format-time-string "** %Y") nil t)
        (progn
          (end-of-line)
          (newline))
      (progn
        (beginning-of-buffer)
        (if (search-forward (format-time-string "* %B %d") nil t)
            (progn
              (end-of-line)
              (insert (format-time-string "\n** %Y :%a:")))
          (progn
            (beginning-of-buffer)
            (search-forward "\n\n" nil t)
            (insert (format-time-string "* %B %d\n** %Y :%a:"))))
        (org-set-tags (format-time-string ":%a:"))
        (newline))))

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
                 "*** %(format-time-string org-journal-time-format)%?"
                 :prepend t
                 :clock-in t
                 :clock-resume t))

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
    (dolist (sketch sketches)
      (add-to-list 'org-capture-templates
                   `(,(substring sketch 0 1) ,(concat sketch " sketch") plain
                     (function (lambda () (-> (concat org-directory "sketch/")
                                              x/string-append-time-suffix
                                              (concat "." ,sketch)
                                              find-file)))
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
                 ;; :jump-to-captured t
                 :empty-lines 1)))

(provide 'x-org-capture)
;;; x-org-capture.el ends here
