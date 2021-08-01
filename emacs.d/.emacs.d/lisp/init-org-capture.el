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

  (defun xr/find-phone-insertion-point ()
    "Positions point at current month heading in file"
    (interactive)
    (beginning-of-buffer)

    (if (search-forward (format-time-string "* %Y\n** %B") nil t)
        (progn
          (end-of-line)
          (insert "\n"))
      (progn
        (beginning-of-buffer)
        (re-search-forward "^$")
        (insert (format-time-string "* %Y\n** %B\n")))))

  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates
               '("p" "Phone call" plain
                 (file+function "gtd/phone.org" xr/find-phone-insertion-point)
                 "*** PHONE %T %? :PHONE:\n"
                 :prepend t
                 :clock-in t
                 :clock-resume t))

  (add-to-list 'org-capture-templates
               '("m" "Meeting" entry
                 (file "gtd/inbox.org")
                 "* %T MEETING with %? :MEETING:\n"
                 :clock-in t
                 :clock-resume t))

  (add-to-list 'org-capture-templates
               '("h" "Habit" entry
                 (file "gtd/inbox.org")
                 "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n\n%U\n"))

  (add-to-list 'org-capture-templates
               '("w" "org-protocol" entry
                 (file "gtd/inbox.org")
                 "* TODO Review %c\n%U\n"
                 :immediate-finish t))

  (add-to-list 'org-capture-templates
               '("n" "note" entry
                 (file "gtd/inbox.org")
                 "* %? :NOTE:\n%U\n"
                 :clock-in t
                 :clock-resume t))

  (add-to-list 'org-capture-templates
               '("c" "todo" entry
                 (file "gtd/inbox.org")
                 "* TODO %?\n%U\n"
                 :clock-in t
                 :clock-resume t
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
