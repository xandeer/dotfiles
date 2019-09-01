;;; private/xandeer/+capture.el -*- lexical-binding: t; -*-

(after! org
  (setq
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
         "* %? %U" :prepend t))))
