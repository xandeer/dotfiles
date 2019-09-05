;;; xandeer/org/config.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/Dropbox/notes"
        org-reverse-note-order t
        org-archive-reversed-order t
        org-agenda-files '("~/Dropbox/notes/gtd.org")
        org-todo-keywords '((sequence "TODO(t)" "|" "DELEGATE(e)" "DONE(d)")
                            (sequence "|" "CANCELED(c@/!)"))
        org-refile-targets '((nil :maxlevel . 5)
                           ("someday.org" :maxlevel . 5)
                           ("journal.org" :maxlevel . 5)
                           ("diary.org" :maxlevel . 5)
                           ("learning.org" :maxlevel . 5)
                           ("reading.org" :maxlevel . 5)
                           ("xmind.org" :maxlevel . 5)
                           ("notes.org" :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5))
        org-default-notes-file (concat org-directory "/journal.org")
        org-capture-templates `(("t" "todo" entry (file+headline "gtd.org" "Tasks")
                                 "* TODO %?\n%U\n" :clock-resume t :prepend t)
                                ("c" "cache" entry (file+headline "" "Cache") ; "" => `org-default-notes-file'
                                 "* %? %U" :prepend t)
                                ("D" "Do It Tomorrow" entry (file+headline "gtd.org" "Tasks")
                                 "* TODO %?\n%(org-custom-scheduled-tomorrow)\n%U\n"
                                 :clock-resume t :prepend t)
                                ("w" "word" item (file+olp+datetree "learning.org" "Words")
                                 "%?" :prepend t)
                                ("S" "sentences" item (file+headline "learning.org" "Sentences")
                                 "%? %U" :prepend t)
                                ("x" "work" entry (file+olp+datetree "work.org" "Weekly Summaries")
                                 (file ".work.tmpl.org") :prepend t)
                                ("d" "daily review" entry (file+olp+datetree "diary.org" "Daily Review")
                                 (file ".daily.tmpl.org") :prepend t)
                                ("W" "weekly review" entry (file+olp+datetree "diary.org" "Weekly Review")
                                 (file ".weekly.tmpl.org") :prepend t :tree-type week)
                                ("b" "books want to read" item (file+olp "someday.org" "Books" "Want to Read")
                                 "1. %? %U" :prepend t)
                                ("B" "books had read" item (file+olp "someday.org" "Books" "Read" "2019")
                                 "1. %? %^u" :prepend t)
                                ("f" "films want to watch" item (file+olp "someday.org" "Films" "Want to Watch")
                                 "1. %? %U" :prepend t)
                                ("F" "films had watched" item (file+olp "someday.org" "Films" "Watched" "2019")
                                 "1. %? %^u" :prepend t)
                                ("i" "Ideas" entry (file+headline "someday.org" "Ideas")
                                 "* %? %U" :prepend t))
        org-agenda-span 'day
        org-agenda-start-day nil
        org-clock-clocked-in-display 'both
        org-agenda-time-grid '((daily today require-timed)
                               (700 900 1000 1100 1400 1500 1600 1700)
                               "......"
                               "----------------")
        org-agenda-start-on-weekday nil)

  (map! :map org-mode-map
        :gni [M-return]   (λ! (+org/insert-item-below 1))
        :gni [s-return]   (λ! (+org/insert-item-below 1))
        :gni [M-S-return] (λ! (+org/insert-item-above 1))
        :gni [s-S-return] (λ! (+org/insert-item-above 1)))

  (map! :map evil-org-mode-map
        :i "C-l" (general-predicate-dispatch 'recenter-top-bottom
                   (org-at-table-p) 'org-table-next-field)
        :i "C-h" (general-predicate-dispatch 'help
                   (org-at-table-p) 'org-table-previous-field)
        :i "C-k" (general-predicate-dispatch 'kill-line
                   (org-at-table-p) '+org/table-previous-row)
        :i "C-j" (general-predicate-dispatch 'org-down-element
                   (org-at-table-p) 'org-table-next-row)

        :ni "M-j" #'org-next-visible-heading
        :ni "M-k" #'org-previous-visible-heading)

  (map! :localleader
        :map org-mode-map
        ;; scheduled to tomorrow
        "n" (λ! (org--deadline-or-schedule "" 'scheduled "+1d"))))

(after! plantuml-mode
  (setq plantuml-jar-path "~/.local/share/plantuml/plantuml.jar"
        org-plantuml-jar-path plantuml-jar-path))
