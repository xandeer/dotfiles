;;; xandeer/org/config.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/projects/personal/notes"
        org-reverse-note-order t
        org-archive-reversed-order t
        org-agenda-files '("~/projects/personal/notes/gtd.org")
        org-todo-keywords '((sequence "TODO(t)" "|" "DELEGATE(e)" "DONE(d)")
                            (sequence "|" "CANCELED(c@/!)"))
        org-refile-targets '((nil :maxlevel . 5)
                           ("someday.org" :maxlevel . 5)
                           ("journal.org" :maxlevel . 5)
                           ("diary.org" :maxlevel . 5)
                           ("learning.org" :maxlevel . 5)
                           ("reading.org" :maxlevel . 5)
                           ("work.org" :maxlevel . 5)
                           ("notes.org" :maxlevel . 5)
                           (org-agenda-files :maxlevel . 5))
        org-default-notes-file (concat org-directory "/journal.org")
        org-capture-templates `(("t" "todo" entry (file+headline "gtd.org" "Tasks")
                                 "* TODO %?\n%U\n" :clock-resume t :prepend t)
                                ("c" "cache" entry (file+headline "" "Cache") ; "" => `org-default-notes-file'
                                 "* %? %U" :prepend t)
                                ("D" "Do It Tomorrow" entry (file+headline "gtd.org" "Tasks")
                                 "* TODO %?\n%(+org/schedule-tomorrow)\n%U\n"
                                 :clock-resume t :prepend t)
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
        org-agenda-start-on-weekday nil))

(after! plantuml-mode
  (setq
   plantuml-default-exec-mode 'jar
   plantuml-jar-path "~/.local/share/plantuml/plantuml.jar"
   org-plantuml-jar-path plantuml-jar-path))
