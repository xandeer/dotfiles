;;; init-org-capture.el --- Settings for org capture -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf org-capture
  :require t
  :bind
  ("C-c c" . org-capture)
  :config
  (unless (boundp 'org-capture-templates)
    (defvar org-capture-templates nil))

  (defun xr/find-phone-location ()
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

  (defun xr/find-journal-location ()
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
                 (file+function "gtd/phone.org" xr/find-phone-location)
                 "*** PHONE %T %? :PHONE:\n"
                 :prepend t
                 :clock-in t
                 :jump-to-captured t
                 :clock-resume t))

  (add-to-list 'org-capture-templates
               '("j" "Journal entry" plain
                 (function xr/find-journal-location)
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
                 "* TODO %?\n%U"
                 :empty-lines-before 1))

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
                 :jump-to-captured t
                 :empty-lines 1)))

(provide 'init-org-capture)
;;; init-org-capture.el ends here
